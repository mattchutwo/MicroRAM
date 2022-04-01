{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RiscV.Transpiler where -- (transpiler)

import Control.Monad.State
import Data.Default (def)
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe, isJust)
import Data.Foldable (toList)
-- import Test.QuickCheck (Arbitrary, arbitrary, oneof)
import Compiler.IRs
import Compiler.Metadata
import Compiler.Errors
import MicroRAM (MWord, Instruction'(..), MemWidth(..))
import Compiler.Common
import Compiler.Registers (RegisterData( NumRegisters ))
-- import Compiler.Analysis (AnalysisData)
import Compiler.CompilationUnit
import Compiler.IRs(lazyPc, hereLabel)
import Compiler.LazyConstants
import Compiler.Analysis (AnalysisData(..))

import qualified Data.List as List (partition)
import qualified Data.Map as Map
import  Data.Bits (shiftL, (.|.),(.&.))

import Debug.Trace (trace)

import RiscV.RiscVAsm

import Control.Lens (makeLenses, ix, at, to, (^.), (.=), (%=), (?=), _Just, use, over, Lens')


-- | Option
_warningOn :: Bool
_warningOn = True

-- | Unsafe: alerts of a problem, but not quite an error.
warning :: String -> a -> a
warning msg a | _warningOn = trace msg a
              | otherwise = a


              
data Section = Section
  { _secName :: String
  -- From the tags, only the options we care about
  , _flag_exec :: Bool  -- ^ executable "e" 
  , _flag_write :: Bool -- ^ Writable  "w"
  -- | section type
  , _secType :: Maybe SectionType
  , _secContent :: Seq.Seq LineOfRiscV
  }
  deriving (Show, Eq, Ord)

execSection, writeSection, readSection :: Section
readSection =   Section { _secName = ""
                        , _flag_exec = False 
                        , _flag_write = False
                        , _secType = Nothing
                        , _secContent = Seq.empty
                        }
writeSection =  readSection { _flag_write = True }
execSection =   readSection { _flag_exec = True }

flagSection :: String -> [Flag] -> Maybe SectionType ->  Seq.Seq LineOfRiscV -> Section
flagSection name flags maybeType content = Section { _secName = name
                                                   , _flag_exec = Flag_e `elem` flags
                                                   , _flag_write = Flag_w `elem` flags
                                                   , _secType = maybeType
                                                   , _secContent = content
                                           }
makeLenses ''Section







                                                   

---------------                                                   
-- New Version
---------------

data SymbolTP = SymbolTP
  { _symbName  :: String                    
  , _symbSize  :: Maybe (Either Integer Imm)
  , _symbAlign :: Maybe Integer             
  , _symType   :: Maybe DirTypes
  }
defaultSym :: SymbolTP
defaultSym = SymbolTP "Default Name" Nothing Nothing Nothing 

makeLenses ''SymbolTP
              
data TPState = TPState
  { _currSectionTP  :: Section
  , _currFunctionTP :: String
  , _currBlockTP    :: String
  , _currBlockContentTP :: Seq.Seq (MAInstruction Int MWord, Metadata)
  , _commitedBlocksTP :: [NamedBlock Metadata Int MWord]
  , _sectionsTP :: Map.Map String Section
    
  -- Memory
  , _curObject :: Maybe (GlobalVariable MWord)
  , _genvTP :: GEnv MWord

  -- Markers
  , _atFuncStartTP :: Bool -- ^ If the next instruction needs to be marked as function start
  , _afterFuncCallTP :: Bool -- ^ If the next instruction needs to be marked as after call
    
  -- Symbols
  , _symbolTableTP :: Map.Map String SymbolTP
  
  }

makeLenses ''TPState
-- makeLenses ''GlobalVariable

initStateTP :: TPState
initStateTP = TPState
  { _currSectionTP      = readSection { _secName = "initSection"}
  , _currFunctionTP     = "NoneInit"
  , _currBlockTP        = "NoneInit"
  , _currBlockContentTP = Seq.empty
  , _commitedBlocksTP   = [] 
  , _sectionsTP         = Map.empty
  , _curObject          = Nothing
  , _genvTP             = []
  , _atFuncStartTP      = False -- An instruction type needs to be found first.
  , _afterFuncCallTP    = False
  , _symbolTableTP    = Map.empty
  }

  
type Statefully = StateT TPState Hopefully

transpiler :: [LineOfRiscV] -> Hopefully (CompilationUnit (GEnv MWord) (MAProgram Metadata Int MWord))
transpiler rvcode = evalStateT (mapM transpilerLine rvcode >> finalizeTP) initStateTP
  where 
    transpilerLine :: LineOfRiscV -> Statefully ()
    transpilerLine (LabelLn lbl) =
      ifM (use $ currSectionTP . flag_exec) (codeLbl lbl) (memLbl lbl) 
    transpilerLine (Directive dir) =     transpileDir dir
    transpilerLine (Instruction instr) = transpileInstr instr

    codeLbl,memLbl :: String -> Statefully ()
    codeLbl lbl = do
      -- Commit current block
      commitBlock
      -- Start a new block
      currBlockTP .= lbl
      -- If entering a function...
      lblTyp <- _symType <<$>> use (symbolTableTP . at lbl)
      case lblTyp of
        (Just (Just DTFUNCTION)) -> do
          -- Set the current Function
          currFunctionTP .= lbl
          -- Set function start (will mark the frist instruction's
          -- metadata)
          atFuncStartTP .= True
        _ -> return ()
    memLbl lbl = do
      -- Store current object
      saveObject
      -- Start a new object
      curObject . _Just %= (\obj -> obj {globName = quickName lbl})



saveObject :: Statefully ()
saveObject = do
  -- Get the current obejct
  curObj <- use $ curObject
  -- add it to the list of globals
  mapM (\x -> genvTP %= (:) x) curObj
  -- Clear current obje
  curObject .= Nothing

transpileDir :: Directive -> Statefully ()
transpileDir dir =
  case dir of
    -- Ignored 
    ALIGN _         -> ignoreDire -- We are ignoring alignment
    P2ALIGN _a _v _m-> ignoreDire -- We are ignoring alignment
    BALIGN _ _      -> ignoreDire -- We are ignoring alignment
    FILE _          -> ignoreDire
    IDENT _         -> ignoreDire  -- just places tags in object files
    ADDRSIG         -> ignoreDire -- We ignore address-significance 
    ADDRSIG_SYM _nm -> ignoreDire -- We ignore address-significance 
    CFIDirectives _ -> ignoreDire -- ignore control-flow integrity
    -- Currently unimplemented 
    Visibility _ _ -> unimplementedDir -- Not in binutils. Remove?
    STRING _st     -> unimplementedDir
    ASCIZ  _st     -> unimplementedDir
    EQU _st _val   -> unimplementedDir
    OPTION _opt    -> unimplementedDir -- Rarely used
    VARIANT_CC _st -> unimplementedDir -- Not in binutils. Remove?
    SLEB128 _val   -> unimplementedDir -- How do we do this?
    ULEB128 _val   -> unimplementedDir -- How do we do this?
    MACRO _ _ _    -> unimplementedDir -- No macros.
    ENDM           -> unimplementedDir
    -- Implemented
    -- ## Declare Symbols 
    COMM   nm size align -> setSymbol nm (Just $ Left  size) (Just align)   Nothing
    COMMON nm size align -> setSymbol nm (Just $ Left  size) (Just align)   Nothing
    SIZE   nm size       -> setSymbol nm (Just $ Right size) Nothing        Nothing
    TYPE   nm typ        -> setSymbol nm Nothing             Nothing        (Just typ)
    -- ## Sections
    TEXT    ->                       setSection "text"   []    Nothing []
    DATA    ->                       setSection "data"   []    Nothing []
    RODATA  ->                       setSection "rodata" []    Nothing []
    BSS     ->                       setSection "bss"    []    Nothing []
    SECTION nm flags typ flagArgs -> setSection nm       flags typ     flagArgs
    -- ## Attributes (https://sourceware.org/binutils/docs/as/RISC_002dV_002dATTRIBUTE.html)
    ATTRIBUTE _tag _val -> ignoreDire -- We ignore for now, but it has some alignment infomrations 
    -- ## Emit data
    DirEmit typ val  -> mapM_ (emitValue $ emitSize typ) val
    ZERO size        -> emitValue size (ImmNumber 0)

    where
      ignoreDire = return ()
      -- TODO: for testing we allow unimplemented directives.
      unimplementedDir = ignoreDire -- implError $ "Directive not yet implemented: " <> show dir

      -- | Creates a symbol if it doens't exists and modifies the
      -- attributers provided
      setSymbol :: String                     -- ^ Name
                -> Maybe (Either Integer Imm) -- ^ Size
                -> Maybe Integer              -- ^ Alignment
                -> Maybe DirTypes             -- ^ Type
                -> Statefully ()
      setSymbol name size align typ = do
        -- Get the symbol from the table, or the default if the symbol is not there.
        symbol <-  fromMaybe defaultSym <$> use (symbolTableTP . at name)
        -- Then set the values given.
        let symbol' = SymbolTP {
              _symbName    = name
              , _symbSize  = firstJust size  (_symbSize  symbol) 
              , _symbAlign = firstJust align (_symbAlign symbol) 
              , _symType   = firstJust typ   (_symType   symbol) 
              }
        symbolTableTP . at name ?= symbol'

      -- | Write some values to the current object in memory.
      --
      -- Note: the problem here is that RISCV emits values in
      -- unaligned chunks of 1 to 8 bytes. While MRAM stores
      -- everything in Words.
      emitValue :: Integer -> Imm -> Statefully ()
      emitValue _ _ = return () -- TODO fix (temp. fix for debugging)


      -- | The number of bytes produced by each directive. We follow the
      -- Manual set for loads and stores: "The SD, SW, SH, and SB
      -- instructions store 64-bit, 32-bit, 16-bit, and 8-bit values
      -- from the low bits of register rs2 to memory respectively."
      emitSize :: EmitDir -> Integer
      emitSize emitTyp =
        case emitTyp of
          BYTE        -> 1 
          BYTE2       -> 2
          HALF        -> 2
          SHORT       -> 2 -- short is a 16-bit unsigned integer 
          BYTE4       -> 4
          WORD        -> 4
          LONG        -> 8 -- Or 4 in RV32I
          BYTE8       -> 8
          DWORD       -> 8
          QUAD        -> 8 -- it emits an 8-byte integer. If the
                           -- bignum won’t fit in 8 bytes, it prints a
                           -- warning message; and just takes the
                           -- lowest order 8 bytes of the bignum.
          DTPRELWORD  -> 4 -- dtp relative word, probably shouldn't show up 
          DTPRELDWORD -> 8



-- | Creates a section if it doens't exists and starts appending at
-- the end of it This also saves the current section back into the
-- map by calling `saveSection`.
--
-- We don't check if the existing section matches the settings given.
setSection :: String                     -- ^ Name
          -> [Flag]                      -- ^ 
          -> (Maybe SectionType)         -- ^ 
          -> [FlagArg]                   -- ^ 
          -> Statefully ()               -- ^
setSection name flags secTyp flagArgs = do
  -- Save the current section
  saveSection
  -- Create ampty section in case it doesn't exist
  let initSection = makeInitsection name flags secTyp flagArgs
  --
  theSection <- fromMaybe initSection <$> use (sectionsTP . at name)
  currSectionTP .= theSection

  where
    makeInitsection :: String -> [Flag] -> (Maybe SectionType) -> [FlagArg] -> Section
    makeInitsection name flag secTyp _flagArgs =
      Section { _secName = name
              , _flag_exec = Flag_x `elem` flag -- Must we check that 'e'
                                             -- is not a flag? What
                                             -- are excluded sections?
              , _flag_write = Flag_w `elem` flag
              , _secType = secTyp
              , _secContent = Seq.empty
              }
  
-- | Save current section into the sections map.
saveSection :: Statefully ()
saveSection = do
  currSec :: Section <- use $ currSectionTP
  let name = (_secName currSec)
  sectionsTP . at name ?= currSec 
  return ()





-- ## Registers

tpReg :: Reg -> Int
tpReg = fromEnum

-- We need an extra register, to translate RISCV to MRAMAsm
newReg :: Int
newReg = 1 + (fromEnum $ (maxBound::Reg)::Int)


-- ## Immediates
tpImm :: Imm -> LazyConst MWord
tpImm imm = case imm of
              ImmNumber c -> SConst c
              ImmSymbol str -> lazyAddrOf $ quickName str         
              ImmMod mod imm1 ->
                let lc = tpImm imm1 in lazyUop (modifierFunction mod) lc
              ImmBinOp immop imm1 imm2 ->
                let lc1 = tpImm imm1 in
                  let lc2 = tpImm imm2 in
                    lazyBop (tpImmBop immop) lc1 lc2
  where
    tpImmBop :: ImmOp -> MWord -> MWord -> MWord
    tpImmBop bop =
      case bop of
        ImmAnd   -> (.&.)
        ImmOr    -> (.|.)
        ImmAdd   -> (+)
        ImmMinus -> (-)
    
    modifierFunction :: Modifier -> MWord -> MWord
    modifierFunction mod _ =
      case mod of
        ModLo              -> undefined
        ModHi              -> undefined
        ModPcrel_lo        -> undefined
        ModPcrel_hi        -> undefined
        ModGot_pcrel_hi    -> undefined
        ModTprel_add       -> undefined
        ModTprel_lo        -> undefined
        ModTprel_hi        -> undefined
        ModTls_ie_pcrel_hi -> undefined
        ModTls_gd_pcrel_hi -> undefined

addrRelativeToAbsolute :: Imm -> MAOperand Int MWord
addrRelativeToAbsolute off = let off' = tpImm off in
  LImm $ lazyPc + off'

tpAddress :: Imm  -> MAOperand Int MWord
tpAddress address = LImm $ tpImm address

pcPlus :: MWord -> MAOperand Int MWord
pcPlus off = LImm $ lazyPc + SConst off

-- batch conversion of arguemtns (looks cleaner)
tpRegImm :: Reg -> Imm -> (Int, LazyConst MWord)
tpRegImm r1 imm = (tpReg r1, tpImm imm)
tpRegRegImm :: Reg -> Reg -> Imm -> (Int, Int, LazyConst MWord)
tpRegRegImm r1 r2 imm = (tpReg r1, tpReg r2, tpImm imm)
tpRegRegReg :: Reg -> Reg -> Reg -> (Int, Int, Int)
tpRegRegReg r1 r2 r3 = (tpReg r1, tpReg r2, tpReg r3)


-- ## Instructions
transpileInstr :: Instr -> Statefully ()
transpileInstr instr = do
  let instrs = case instr of
                 Instr32I instrRV32I     -> transpileInstr32I instrRV32I     
                 Instr64I instrRV64I     -> transpileInstr64I instrRV64I   
                 Instr32M instrExt32M    -> transpileInstr32M instrExt32M  
                 Instr64M instrExt64M    -> transpileInstr64M instrExt64M  
                 InstrPseudo pseudoInstr -> transpileInstrPseudo pseudoInstr
                 InstrAlias aliasInstr   -> transpileInstralias aliasInstr
  instrMD <- traverse addMetadata instrs
  currBlockContentTP %= flip mappend instrMD -- Instructions are added at the end.
    where
      addMetadata :: MAInstruction Int MWord
                  -> Statefully (MAInstruction Int MWord, Metadata)
      addMetadata instr = do
        funName <- use currFunctionTP
        blockName <- use currBlockTP
        line <- return 0 -- Bogus
        return (instr, Metadata (quickName funName) (quickName blockName) line False False False False)
               
      transpileInstr32M    :: InstrExt32M -> Seq (MAInstruction Int MWord) 
      transpileInstr64M    :: InstrExt64M -> Seq (MAInstruction Int MWord) 
      transpileInstralias  :: AliasInstr  -> Seq (MAInstruction Int MWord)
      transpileInstr32M    instr = undefined
      transpileInstr64M    instr = undefined
      transpileInstralias  instr = undefined

transpileInstrPseudo :: PseudoInstr -> Seq (MAInstruction Int MWord)  
transpileInstrPseudo instr =
  Seq.fromList $
  case instr of 
    RetPI -> [Ijmp . AReg $ tpReg X1]
    CallPI Nothing off ->
      -- MicroRam has no restriction on the size of offsets,
      -- So there is no need to use `auipc`
      [Imov (tpReg X1) (pcPlus 2),
       Ijmp $ tpAddress off]
    CallPI (Just rd) off -> 
      -- MicroRam has no restriction on the size of offsets,
      -- So there is no need to use `auipc`
      [Imov (tpReg rd) (pcPlus 2),
       Ijmp $ tpAddress off]
    TailPI off ->
      -- From the RiscV Manual, tail calls are just a call that uses
      -- the X6 register (page 140 table 25.3)
      [Imov (tpReg X1) (pcPlus 2),
       Ijmp $ tpAddress off]
    FencePI -> []
    LiPI rd imm->
      let (rd',imm') = tpRegImm rd imm in
      -- MicroRam has no restriction on the size of offsets, so there
      -- is no need to use 'lui, addi, slli, addi'. We can directly
      -- mov the constant
      [Imov rd' (LImm imm')]
    NopPI ->
      -- We are already changing the alignemnt.
      -- can we ignore nop's (i.e. return [])
      [Iadd (tpReg X0) (tpReg X0) (LImm 0) ]
    AbsolutePI _ -> error "AbsolutePI" -- AbsolutePseudo
    UnaryPI  unop reg1 reg2  -> unaryPseudo unop reg1 reg2  -- UnaryPseudo Reg Reg
    CMovPI  _ _ _-> error "1" -- CMovPseudo Reg Reg
    BranchZPI bsp rd off -> pseudoBranch bsp rd off  -- BranchZPseudo Reg Offset
    BranchPI  _ _ _ _-> error "3" -- BranchPseudo Reg Reg Offset
    JmpImmPI  JPseudo off -> [Ijmp . LImm $ tpImm off]
    JmpImmPI  JLinkPseudo off -> [Imov (tpReg X1) (pcPlus 2),
                                  Ijmp . LImm $ tpImm off]
    JmpRegPI  _ _ -> error "5" -- JumpPseudo Reg
  where
    unaryPseudo :: UnaryPseudo -> Reg -> Reg  -> [MAInstruction Int MWord]
    unaryPseudo unop reg1 reg2 =
      let (rs1',rs2, _) = tpRegRegImm reg1 reg2 (ImmNumber 0) in
        let rs2' = AReg rs2 in 
          case unop of
            MOV   -> [Imov rs1' rs2']  
            NOT   -> [Inot rs1' rs2']
            NEG   -> [Imov newReg (LImm 0), Isub rs1' newReg rs2']
            NEGW  -> [Imov newReg (LImm 0), Isub rs1' newReg rs2']
            SEXTW -> [Imov rs1' rs2'] <> restrictAndSignExtendResult rs1'
        
    
    pseudoBranch :: BranchZPseudo ->  Reg ->  Offset -> [MAInstruction Int MWord]
    pseudoBranch bsp rs1 off =
      let (rs1',off') = tpRegImm rs1 off in
        let (computCond, negate) = condition bsp rs1' in
          computCond : [(if negate then Icnjmp newReg else Icjmp newReg) 
                         $ tpAddress off]
      

    
    -- Returns an instruction that computes the condition and
    -- a boolean describing if the result should be negated.
    -- (MRAM doens't have BNE, but has `Icnjmp` to negate `Icmpe`).
    condition cond r1 =
      case cond of  
        BEQZ -> (Icmpe newReg r1 (LImm 0), False)
        BNEZ -> (Icmpe newReg r1 (LImm 0), True)
        BLTZ -> (Icmpge newReg r1 (LImm 0), True)
        BLEZ -> (Icmpge newReg (tpReg X0) (AReg r1), False)   
        BGEZ -> (Icmpge newReg r1 (LImm 0), False)    
        BGTZ -> (Icmpge newReg (tpReg X0) (AReg r1), True)   


-- | For operations over words (operands ending in 'W')
-- The result ignores overflow and is signextended to 64bits. 
restrictAndSignExtendResult :: Int -> [MAInstruction Int MWord]
restrictAndSignExtendResult rd' =
  [ -- restrict the result (Forget overflows)
    Iand rd' rd' (LImm $ 2^32 - 1),
    -- Sign extend
    Ishr newReg rd' (LImm 31), -- sign
    Imull newReg newReg (LImm $ 2^64-2^32), -- extension
    Ior rd' newReg (AReg rd') -- set sign extension
  ]

transpileInstr64I    :: InstrRV64I  -> Seq (MAInstruction Int MWord) 
transpileInstr64I    instr =
  Seq.fromList $
  case instr of
    MemInstr64 mop r1 off r2 ->
      memInstr64 mop r1 off r2  
    ImmBinop64 binop64I reg1 reg2 imm  -> transpileImmBinop64I binop64I reg1 reg2 imm -- Binop64I Reg Reg Imm
    RegBinop64 binop64  reg1 reg2 reg3 -> transpileRegBinop64I binop64  reg1 reg2 reg3 -- Binop64 Reg Reg Reg
  where
    transpileImmBinop64I :: Binop64I -> Reg -> Reg -> Imm -> [MAInstruction Int MWord]
    transpileImmBinop64I binop64I reg1 reg2 imm =
      let (rd',rs1',off'') = tpRegRegImm reg1 reg2 imm in
        let off' = LImm off'' in
          (case binop64I of
            ADDIW -> [Iadd rd' rs1' off']
            -- It should be true that the  'off' < 2^5'
            -- but we do not check for it. 
            SLLIW -> [Ishl rd' rs1' off']
            SRLIW -> [Iand newReg rs1' (LImm $ 2^32 - 1),
                      Ishr rd' rs1' off']
            SRAIW -> error "Arithmetic right shift not implemented (64I)" -- TODO
          ) <>
          restrictAndSignExtendResult rd'
      where
        -- We assume, but don't check, that the immediate is less than 32bits long.
        -- in fact, because of RiscV encoding it should be less than 12bits long.
        wordRestricted
          :: Int -> Int -> LazyConst MWord
          -> (Int -> Int -> (MAOperand Int MWord) -> MAInstruction Int MWord)
          -> [MAInstruction Int MWord]
        wordRestricted rd' rs1' imm op =
          [ -- Restrict the values to 32b
            Ior newReg rs1' (LImm $ 2^32 - 1),
            -- do the operation 
            op rd' newReg (LImm imm)] <>
          restrictAndSignExtendResult rd'
      
    transpileRegBinop64I :: Binop64  -> Reg -> Reg -> Reg -> [MAInstruction Int MWord]
    transpileRegBinop64I binop32 reg1 reg2 reg3 =
      let (rd',rs1',rs2') = tpRegRegReg reg1 reg2 reg3 in
        (case binop32 of
            ADDW -> [Iadd rd' rs1' (AReg rs2')]                                 
            SUBW -> [Isub rd' rs1' (AReg rs2')]                                 
            SLLW -> [Iand newReg rs2' (LImm $ 2^5 - 1), -- restrict input to 5b
                     Ishl rd' rs1' (AReg newReg)]                                 
            SRLW -> [Iand newReg rs1' (LImm $ 2^32 - 1), -- restrict input to 32b
                     Iand rd'    rs2' (LImm $ 2^5 - 1), -- restrict input to 5b
                     Ishr rd' newReg (AReg rd')]                                  
            SRAW -> error "Arithmetic right shift not implemented" -- TODO
            ) <>
          restrictAndSignExtendResult rd'
      where
            
            

            
        
    memInstr64 :: MemOp64 -> Reg -> Offset -> Reg -> [MAInstruction Int MWord]
    memInstr64  mop r1 off r2 =
      let (rd',rs1',off'') = tpRegRegImm r1 r2 off in
        let off' = LImm off'' in
      case mop of
        -- unsigned load
        LWU -> [Iadd newReg rs1' off',
                 Iload W1 rd' (AReg newReg)]
        -- double mems
        LD  -> [Iadd newReg rs1' off',
                 Iload W1 rd' (AReg newReg)]
        SD  -> [Iadd newReg rs1' off', 
                     Istore W1 (AReg newReg) rd' ]
      
transpileInstr32I :: InstrRV32I -> Seq (MAInstruction Int MWord) 
transpileInstr32I instr =
  Seq.fromList $
  case instr of
    JAL rd off -> let (rd',_off') = tpRegImm rd off in 
                    [Imov rd' (pcPlus 2), -- Or is it 8?
                     Ijmp $ tpAddress off] -- Is our instruction numbering compatible?
    JALR rd rs1 off -> let (rd',rs1',off') = tpRegRegImm rd rs1 off in 
                        [Iadd newReg rs1' (LImm off'), -- this instruction must go first, in case rd=rs1
                         Imov rd' (pcPlus 2), -- or is it 8? 
                         Ijmp $ AReg newReg] -- Is our instruction numbering compatible?
    BranchInstr cond src1 src2 off -> let (src1',src2',off') = tpRegRegImm src1 src2 off in
                                        let (computCond, negate) = condition cond src1' src2' in
                                          computCond : [(if negate then Icnjmp newReg else Icjmp newReg) 
                                                        $ tpAddress off]
    MemInstr32 memOp32 reg1 off reg2  -> memOp32Instr memOp32 reg1 off reg2 
    LUI reg imm -> let (reg',imm') = tpRegImm reg imm in 
      [Imov reg' (LImm $ lazyUop luiFunc imm')]
    AUIPC reg off -> let (reg',off') = tpRegImm reg off in 
                       [Imov reg' (LImm $ lazyPc + lazyUop luiFunc off')]
    ImmBinop32 binop32I reg1 reg2 imm -> transpileImmBinop32I binop32I reg1 reg2 imm
    RegBinop32 binop32 reg1 reg2 reg3 -> transpileRegBinop32I binop32  reg1 reg2 reg3
    FENCE setOrdering                 -> undefined -- Probably a noop?
    FENCEI                            -> undefined -- Probably a noop?
  where
    -- Memory operations
    -- Big TODO TODO TODO
    memOp32Instr :: MemOp32 -> Reg -> Offset -> Reg -> [MAInstruction Int MWord]
    memOp32Instr memOp32 reg1 off reg2 =
      let (rd',rs1',off'') = tpRegRegImm reg1 reg2 off in
        let off' = LImm off'' in
          case memOp32 of
            -- Loads
            -- Is there a better way to do sign extended loads?
            LB  -> [Iadd rd' rs1' off', 
                     Iload W1 rd' (AReg rd'),
                     -- get the sign
                     Ishr newReg rd' (LImm$ 8-1),
                     -- make an extension mask (a prefix of o's or 1's)
                     Imull newReg newReg (LImm $ (-1)),
                     Iand newReg newReg (LImm $ 2^8-1),
                     -- add the extension bits
                     Ior  rd' newReg (AReg rd')
                     ]    
            LH  -> [Iadd newReg rs1' off', 
                     Iload W2 rd' (AReg newReg),
                     -- get the sign
                     Ishr newReg rd' (LImm $ 16-1),
                     -- make an extension mask (a prefix of o's or 1's)
                     Imull newReg newReg (LImm (-1)),
                     Iand newReg newReg (LImm $ 2^16-1),
                     -- add the extension bits
                     Ior  rd' newReg (AReg rd')]
            LW  -> [Iadd newReg rs1' off', 
                     Iload W4 rd' (AReg newReg),
                     -- get the sign
                     Ishr newReg rd' (LImm $ 32-1),
                     -- make an extension mask (a prefix of o's or 1's)
                     Imull newReg newReg (LImm $ (-1)),
                     Iand newReg newReg (LImm $ 2^32-1),
                     -- add the extension bits
                     Ior  rd' newReg (AReg rd')]
            -- Unsigned loads
            LBU ->  [Iadd newReg rs1' off',
                      Iload W1 rd' (AReg newReg)] 
            LHU ->  [Iadd newReg rs1' off',
                      Iload W2 rd' (AReg newReg)] 
            -- Stores
            SB  -> [Iadd newReg rs1' off', 
                     Istore W1 (AReg newReg) rd' ] 
            SH  -> [Iadd newReg rs1' off',
                     Istore W2 (AReg newReg) rd' ] 
            SW  -> [Iadd newReg rs1' off',
                     Istore W4 (AReg newReg) rd' ] 
    
    -- transpileRegBinop32I
    transpileRegBinop32I :: Binop32 -> Reg -> Reg -> Reg -> [MAInstruction Int MWord]
    transpileRegBinop32I binop reg1 reg2 reg3 =
      let (rd',rs1',rs2) = tpRegRegReg reg1 reg2 reg3 in
        let rs2' = AReg rs2 in  
          case binop of
            ADD  -> [Iadd  rd' rs1' rs2']
            SUB  -> [Isub  rd' rs1' rs2']
            SLL  -> [Ishl  rd' rs1' rs2']
            SLT  -> [Icmpg rd' rs1' rs2']
            SLTU -> [Icmpa rd' rs1' rs2']
            XOR  -> [Ixor rd' rs1' rs2']
            SRL  -> [Ishr  rd' rs1' rs2']
            SRA  -> error "Arithmetic right shift not implemented" -- TODO
            OR   -> [Ior rd' rs1' rs2']
            AND  -> [Iand rd' rs1' rs2']
          

          
    -- transpileImmBinop32I
    transpileImmBinop32I :: Binop32I -> Reg -> Reg -> Imm -> [MAInstruction Int MWord]
    transpileImmBinop32I binop reg1 reg2 imm =
      let (rd',rs1',off') = tpRegRegImm reg1 reg2 imm in
        let off_imm = LImm off' in
        case binop of
          ADDI  -> [Iadd  rd' rs1' off_imm]
          SLTI  -> [Imov newReg off_imm,
                    Icmpg rd' newReg (AReg rs1')] -- Could we write this in one instruction?
                                                  -- Perhaps we should flip MicroRAM cmpa-cmpg
                                                  -- to match RiscV. 
          SLTIU -> [Imov newReg off_imm,
                    Icmpa rd' newReg (AReg rs1')]
          XORI  -> [Ixor  rd' rs1' off_imm]
          ORI   -> [Ior   rd' rs1' off_imm]
          ANDI  -> [Iand  rd' rs1' off_imm]
          SLLI  -> [Ishl  rd' rs1' off_imm]
          SRLI  -> [Ishr  rd' rs1' off_imm]
          SRAI  -> error "Arithmetic right shift not implemented" -- TODO
      
    
    -- build 32-bit constants and uses the U-type format. LUI
    -- places the U-immediate value in the top 20 bits of the
    -- destination register rd, filling in the lowest 12 bits
    -- with zeros. (We don't cehck the immediate for overflow,
    -- but it could technically be larger than 20bits)
    luiFunc :: MWord -> MWord
    luiFunc w = shiftL w 12 
    -- Returns an instruction that computes the condition and
    -- a boolean describing if the result should be negated.
    -- (MRAM doens't have BNE, but has `Icnjmp` to negate `Icmpe`).
    condition cond r1 r2 =
      case cond of
            BEQ -> (Icmpe newReg r1 (AReg r2), False)
            BNE -> (Icmpe newReg r1 (AReg r2), True)
            BLT -> (Icmpge newReg r1 (AReg r2), True)
            BGE -> (Icmpge newReg r1 (AReg r2), False)
            BLTU-> (Icmpae newReg r1 (AReg r2), True)
            BGEU-> (Icmpae newReg r1 (AReg r2), False)                      

              
      
      

-- Utility functions for the transpiler

finalizeTP :: Statefully (CompilationUnit (GEnv MWord) (MAProgram Metadata Int MWord))
finalizeTP = do
  -- First commit the last section/block
  _ <- commitBlock
  -- Build program
  prog <- use commitedBlocksTP
  -- Build memory
  (genv, mem) <- getMem
  -- Then build the CompilationUnit
  return $ CompUnit { programCU = ProgAndMem prog mem mempty,
                      -- TraceLen is bogus
                      traceLen = 0,
                      -- We added one new register, which is now the largest
                      regData = NumRegisters newReg,
                      -- Analysis data is bogus
                      aData = AnalysisData mempty mempty,
                      -- Name bound is currently bogus, but we should fix it
                      -- TODO: fix name bound.
                      nameBound = 0,
                      intermediateInfo = genv }
  where getMem = return (mempty, mempty) -- Bogus for now TODO


-- Finalize current block and commit it
-- i.e. add it to the list.
commitBlock :: Statefully ()
commitBlock = do
  contnt <- use currBlockContentTP
  currBlockContentTP .= mempty
  name <- use currBlockTP
  let block = NBlock (Just $ quickName name) $ toList contnt
  commitedBlocksTP %= (:) block
  return ()
  
-- Monadic stuff
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b <- b; if b then t else f
whenM :: Monad m => m Bool -> m () -> m ()
whenM mb thing = do { b <- mb
                    ; when b thing }
whenL b s = whenM (use b) s

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just a) _ = Just a
firstJust _ x = x

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap










---------------                                                   
-- Old Version
---------------

-- | Translating compiler from RiscV Assembly to MicroAssembly
transpiler' :: [LineOfRiscV] -> Hopefully (CompilationUnit (GEnv MWord) (MAProgram Metadata regT MWord))
transpiler' rvProg = do
  -- First remove pointless directives
  let cleanProg = cleanUP rvProg
  -- Works in several steps:
  -- 1. Filter undeeded sections and separate the
  -- rest in into excutable and data
  -- 2.1 Translate executable code to MicroAssembly
  -- 2.2 Translate data into InitialMemory
  (execs, datas) <- separateSections cleanProg
  prog <- transpileCode $ Seq.fromList execs
  globs <- transpileData datas
  return $ (compUnitFromCompUnit' prog) {intermediateInfo = globs}
  where
    compUnitFromCompUnit' :: CompilationUnit' a prog -> CompilationUnit a prog
    compUnitFromCompUnit' cu = cu {programCU = ProgAndMem (programCU cu) [] Map.empty}
  

-- | Filters unneeded sections and then splits sections into executable and data sections.
-- 
-- NOTE: This function might produce several sectiosn with the same
-- name. Technically we should concatenate the contents of all equally
-- named sections and then join all subsections of the same
-- section. For now I will just concatenate everything into two
-- sections "executable" and "data". Each variable is individually
-- marked as readable/writable.
--
-- TODO: Fix sections as mentioned above.
separateSections :: [LineOfRiscV] -> Hopefully ([Section], [Section])
separateSections allLines = do
  let (left, sectionParts) = splitKeep isSectionStart allLines
  when (Seq.null left) $ assumptError $ "Some lines outside a section: \n" <> show left
  sections <- mapM makeSection sectionParts
  let neededSecs = filterUnneeded sections
  return $ List.partition _flag_exec neededSecs
  
  where
    -- | Check if a line is the begining of a section, by testing if
    -- it could create a section
    isSectionStart :: LineOfRiscV -> Bool
    isSectionStart line = not . isError $ makeSection (line, Seq.empty)
    
    -- | Like Split, but keeps the separators
    splitKeep :: (a -> Bool) -> [a] -> (Seq a,[(a,Seq a)])
    splitKeep _f [] = (Seq.empty,[])
    splitKeep f (x:xs) | f x , (left, rest) <- splitKeep f xs = (Seq.empty, (x,left) : rest)
    splitKeep f (x:xs) | (left, rest) <- splitKeep f xs = (x:<|left, rest)

    makeSection :: (LineOfRiscV, Seq.Seq LineOfRiscV) -> Hopefully Section
    makeSection (Directive TEXT , contnt)   = return $ execSection { _secName = "text", _secContent = contnt }
    makeSection (Directive DATA ,contnt)   = return $ writeSection { _secName = "data", _secContent = contnt }
    makeSection (Directive RODATA ,contnt) = return $ readSection { _secName = "rodata", _secContent = contnt }
    makeSection (Directive BSS ,contnt)    = return $ writeSection { _secName = "bss", _secContent = contnt }
    makeSection (Directive (SECTION name flags maybeType _) ,contnt) =
      return $ flagSection name flags maybeType contnt
    makeSection (otherLine, _) = assumptError $ "Expected a section directive but found: " <> show otherLine

    -- | for now we don't filter anything
    filterUnneeded :: [Section] -> [Section]
    filterUnneeded secs = filter unneeded secs
      where unneeded _ = True  

-- | Intermediate respresentation of blocks
data IBlock = IBlock
  { nameIB :: String
  , directivesIB :: [Directive]
  , instructionsIB :: [Instr]
  }


-- | Translates sections of executable code, into MicroAssembly
transpileCode :: Seq.Seq Section -> Hopefully (CompilationUnit' () (MAProgram Metadata regT MWord))
transpileCode sections = compUnit . concat <$> mapM transpileSection sections
  where
    compUnit progCode =  CompUnit {programCU = progCode,
                               traceLen  = 0, -- Bogus
                               regData   = NumRegisters 32, -- hardcoded
                               aData     = def, -- Empty
                               nameBound = 0, -- Bogus
                               intermediateInfo = ()}
    transpileSection :: Section -> Hopefully [NamedBlock Metadata r w]
    transpileSection sec | not $ _flag_exec sec = assumptError $ "Expected an executable section but " <> _secName sec <> " is not." 
    transpileSection sec = transpileCodes . toList $ _secContent sec

    transpileCodes :: [LineOfRiscV] -> Hopefully [NamedBlock Metadata r w]
    transpileCodes lines = do
      blocks <- splitBlocks lines
      evalStateT (traverse transpileBlock blocks) ""

    -- Assumes that the code is blocks of the form
    -- @@
    -- [directives]
    -- lable
    -- [instructions]
    -- @@
    -- with either list possibly empty. 
    splitBlocks :: [LineOfRiscV] -> Hopefully [IBlock]
    splitBlocks lns = do
      let (directives , lns1) = spanMaybe getDirectives lns
      (label, lns2) <- case lns1 of
                            (LabelLn lbl):lns2 -> return (lbl, lns2)
                            _ -> assumptError "Directives must be followed by a label"
      let (instrs, lns3) = spanMaybe getInstrs lns2
      blocks <- splitBlocks lns3
      return $ IBlock label directives instrs : blocks

    getDirectives :: LineOfRiscV -> Maybe Directive 
    getDirectives (Directive dir) = Just dir
    getDirectives _ = Nothing
    
    getInstrs :: LineOfRiscV -> Maybe Instr 
    getInstrs (Instruction i) = Just i
    getInstrs _ = Nothing

-- Bogus ID, for now
quickName :: String -> Name
quickName st = Name 0 $ string2short st

-- | The state carries the current function
transpileBlock  :: IBlock -> StateT String Hopefully (NamedBlock Metadata r w)
transpileBlock (IBlock name dirs instrs) = do
  currentF <- checkFunction name dirs
  let md = Metadata {mdFunction = quickName currentF,
                     mdBlock = quickName name,
                     mdLine = 0, -- Can't knwo this. Should we pass it down from the parser?
                     mdFunctionStart = False, -- Only first instr
                     mdReturnCall = False, 
                     mdIsCall = False,     
                     mdIsReturn = False}
  instrsMRAM <- lift $ concat <$> mapM (transpileInstr' md) instrs
  return $ NBlock (Just $ quickName name) instrsMRAM
  
  where 

    -- Expects all function type declarations to be done right before the label in question.
    -- throws error if not the case.
    checkFunction :: String -> [Directive] -> StateT String Hopefully String
    checkFunction name dirs =
      case firstJust' getFunctType dirs of
        Just nameT -> do
          when (name /= nameT) $ assumptError $ "Type definition name " <> nameT <> " doesn't match name of current block " <> name
          put name
          return name
        Nothing -> get

    getFunctType :: Directive -> Maybe String
    getFunctType (TYPE nameT DTFUNCTION) = Just nameT
    getFunctType _ = Nothing

transpileInstr' :: Metadata -> Instr -> Hopefully [(MAInstruction r w, md)]
transpileInstr' md instr = undefined
                    
-- | Translates sections of data into a list of global variables
transpileData :: [Section] -> Hopefully (GEnv MWord)
transpileData _ = return [] -- undefined


-- # Code cleanup

-- | Remove debuging directives
cleanUP, removeUseless, removeDebug :: [LineOfRiscV] -> [LineOfRiscV]
cleanUP = removeDebug . removeUseless

-- Removes CFI and debug directives
removeDebug = filter isNoDebug
  where isNoDebug :: LineOfRiscV -> Bool
        isNoDebug (Directive (CFIDirectives _)) = False
        -- isNoDebug (Directive $ LOC) = True
        isNoDebug _ = True

removeUseless = filter isNotUseless
  where isNotUseless  :: LineOfRiscV -> Bool
        isNotUseless  (Directive (FILE _ )) = False
        isNotUseless  (Directive (SIZE _ _ )) = False
        isNotUseless _ = True
--  # General utility

-- | Like 'span' but with 'Maybe' predicate
--
spanMaybe :: (a -> Maybe b) -> [a] -> ([b],[a])
spanMaybe _ [] =  ([], [])
spanMaybe p xs@(x:xs') = case p x of
    Just y  -> let (ys, zs) = spanMaybe p xs' in (y : ys, zs)
    Nothing -> ([], xs)

-- | Find the first element of a list for which the operation returns 'Just', along
--   with the result of the operation. Like 'find' but useful where the function also
--   computes some expensive information that can be reused.
firstJust' :: (a -> Maybe b) -> [a] -> Maybe b
firstJust' f = listToMaybe . mapMaybe f




--- Quick testing
