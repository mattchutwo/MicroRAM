{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RiscV.Transpiler where -- (transpiler)

import Control.Monad.State
import Data.Default (def)
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe, isJust)
import Data.Foldable (toList)
import Data.List (foldl')
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
import Compiler.Registers

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
  } deriving (Show)
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
  , _curObjectTP :: String
  , _curObjectContentTP :: Maybe ( Seq (LazyConst MWord, Int))
  , _genvTP :: GEnv MWord

  -- Markers
  , _atFuncStartTP :: Bool -- ^ If the next instruction needs to be marked as function start
  , _afterFuncCallTP :: Bool -- ^ If the next instruction needs to be marked as after call
    
  -- Symbols
  , _symbolTableTP :: Map.Map String SymbolTP

  -- Name ID counter
  -- All names must have a distinct ID number
  , _nameIDTP :: Word
  , _nameMap :: Map.Map String Name
  } deriving (Show)

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
  , _curObjectTP        = "NoneInit"
  , _curObjectContentTP = Nothing
  , _genvTP             = []
  , _atFuncStartTP      = False -- An instruction type needs to be found first.
  , _afterFuncCallTP    = False
  , _symbolTableTP      = Map.empty
  , _nameIDTP           = firstUnusedName
  , _nameMap            = Map.fromList [("main", mainName)]
  }

  
type Statefully = StateT TPState Hopefully


-- Bogus ID, for now
quickName :: String -> Name
quickName st =
  trace "You are using quickName which gives bogus IDs which might overlap" $
  Name 0 $ string2short st
  
-- Checks if the name already exist. Otherwise it creates a new one with unique ID
getName :: String -> Statefully Name
getName st = do
  nmap <- use nameMap
  nameRet <- case Map.lookup st nmap of
               Just name -> return name
               Nothing -> do uniqueID <- use nameIDTP
                             nameIDTP %= (1 +)
                             let nameRet = Name uniqueID $ string2short st
                             nameMap .= Map.insert st nameRet nmap
                             return $ nameRet
  return nameRet

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
      commitBlock
      saveObject
      -- Start a new object
      curObjectTP .= lbl



saveObject :: Statefully ()
saveObject = do
  st <- get 
  -- Get the current obejct
  curObj <- use $ curObjectContentTP
  case curObj of
    Nothing -> return ()
    Just ls -> do
        let init = packInWords $ toList ls
        gname <- getName =<< use curObjectTP
        sectionName <- use (currSectionTP . secName)
        readOnly <- not <$> use (currSectionTP . flag_write)
        let gvar = GlobalVariable {globName = gname,
                                   isConstant = readOnly,
                                   -- RiscV doesn't have types 
                                   gType = TVoid,
                                   initializer = Just init,
                    		   gSize = toEnum $ length init,
                                   -- Currently ignored
                                   -- TODO: implement alignment
                                   gAlign = 1,
                    		   secret = sectionName == "__DATA,__secret" ||
                                   sectionName == ".data.secret",
                                   -- What variables are not heap-init in RiscV?
                                   gvHeapInit = True}
        -- add it to the list of globals
        genvTP %= (:) gvar
        -- Clear current obje
        curObjectContentTP .= Nothing
  -- where packInWords :: [(MemWidth, LazyConst MWord)] -> [LazyConst wrdT]
  --       packInWords ls = snd $ foldl' packNextValue initState ls
  --       initState :: (Int, [LazyConst wrdT], LazyConst wrdT)
  --       initState = (0,[],SConst 0) 

  --       packNextValue :: (Int, [LazyConst wrdT], LazyConst wrdT)
  --                     -> (MemWidth, LazyConst MWord)
  --                     -> (Int, [LazyConst wrdT], LazyConst wrdT)
  --       packNextValue (bytsTaken, partResult, partialWord)  (mw, lazy) =
          
          

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
    TEXT    ->                       setSection "text"   [Flag_x]    Nothing []
    DATA    ->                       setSection "data"   []          Nothing []
    RODATA  ->                       setSection "rodata" []          Nothing []
    BSS     ->                       setSection "bss"    []          Nothing []
    SECTION nm flags typ flagArgs -> setSection nm       flags typ     flagArgs
    -- ## Attributes (https://sourceware.org/binutils/docs/as/RISC_002dV_002dATTRIBUTE.html)
    ATTRIBUTE _tag _val -> ignoreDire -- We ignore for now, but it has some alignment infomrations 
    -- ## Emit data
    DirEmit typ val  -> mapM_ (emitValue $ emitSize typ) val
    -- This might be slow if there is a very large zero instruction.
    ZERO size        -> replicateM_ (fromInteger size) $ emitValue 1 (ImmNumber 0)

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

      -- | Pushes a value to the current memory object
      emitValue :: Int -> Imm -> Statefully ()
      emitValue size val = do
        -- Make Imm into a lazy constant and push into the current object
        pushMemVal size =<< tpImm val
        where pushMemVal :: Int -> LazyConst MWord -> Statefully ()
              pushMemVal size val = do
                obj <- maybe mempty id <$> use curObjectContentTP
                -- Values are pushed in the end of the object
                curObjectContentTP .= Just (obj :|> (val,size))
              
        


      -- | The number of bytes produced by each directive. We follow the
      -- Manual set for loads and stores: "The SD, SW, SH, and SB
      -- instructions store 64-bit, 32-bit, 16-bit, and 8-bit values
      -- from the low bits of register rs2 to memory respectively."
      emitSize :: EmitDir -> Int
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
  -- start of a new section closes previous sections. Save ongoing objects and commit blocks
  commitBlock
  saveObject
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
tpImm :: Imm -> Statefully (LazyConst MWord)
tpImm imm = case imm of
              ImmNumber c -> return $ SConst c
              ImmSymbol str -> lazyAddrOf <$> getName str         
              ImmMod mod imm1 -> do
                lc <- tpImm imm1
                return $ lazyUop (modifierFunction mod) lc
              ImmBinOp immop imm1 imm2 -> do
                lc1 <- tpImm imm1
                lc2 <- tpImm imm2
                return $ lazyBop (tpImmBop immop) lc1 lc2
  where
    tpImmBop :: ImmOp -> MWord -> MWord -> MWord
    tpImmBop bop =
      case bop of
        ImmAnd   -> (.&.)
        ImmOr    -> (.|.)
        ImmAdd   -> (+)
        ImmMinus -> (-)
    
    modifierFunction :: Modifier -> MWord -> MWord
    modifierFunction mod w =
      case mod of
        -- The low 12 bits of absolute address for symbol.
        ModLo              -> w .&. (2^12-1)
        -- The high 20 bits of absolute address for symbol. This is
        -- usually used with the %lo modifier to represent a 32-bit
        -- absolute address.
        ModHi              -> w .&. (2^32-2^12)
        ModPcrel_lo        -> undefined
        ModPcrel_hi        -> undefined
        ModGot_pcrel_hi    -> undefined
        ModTprel_add       -> undefined
        ModTprel_lo        -> undefined
        ModTprel_hi        -> undefined
        ModTls_ie_pcrel_hi -> undefined
        ModTls_gd_pcrel_hi -> undefined

addrRelativeToAbsolute :: Imm -> Statefully (MAOperand Int MWord)
addrRelativeToAbsolute off = do
  off' <- tpImm off
  return $ LImm $ lazyPc + off'

tpAddress :: Imm  -> Statefully (MAOperand Int MWord)
tpAddress address = LImm <$> tpImm address

pcPlus :: MWord -> MAOperand Int MWord
pcPlus off = LImm $ lazyPc + SConst off

-- batch conversion of arguemtns (looks cleaner)
tpRegImm :: Reg -> Imm -> Statefully (Int, LazyConst MWord)
tpRegImm r1 imm = do
  imm' <- tpImm imm 
  return (tpReg r1, imm')
tpRegRegImm :: Reg -> Reg -> Imm -> Statefully (Int, Int, LazyConst MWord)
tpRegRegImm r1 r2 imm =  do
  imm' <- tpImm imm
  return (tpReg r1, tpReg r2, imm')
tpRegRegReg :: Reg -> Reg -> Reg -> (Int, Int, Int)
tpRegRegReg r1 r2 r3 = (tpReg r1, tpReg r2, tpReg r3)


-- ## Instructions
transpileInstr :: Instr -> Statefully ()
transpileInstr instr = do
  instrs <- case instr of
              Instr32I instrRV32I     -> transpileInstr32I instrRV32I     
              Instr64I instrRV64I     -> transpileInstr64I instrRV64I   
              Instr32M instrExt32M    -> return $ transpileInstr32M instrExt32M  
              Instr64M instrExt64M    -> return $ transpileInstr64M instrExt64M  
              InstrPseudo pseudoInstr -> transpileInstrPseudo pseudoInstr
              InstrAlias aliasInstr   -> return $ transpileInstralias aliasInstr
  instrMD <- traverse addMetadata instrs
  currBlockContentTP %= flip mappend instrMD -- Instructions are added at the end.
    where
      addMetadata :: MAInstruction Int MWord
                  -> Statefully (MAInstruction Int MWord, Metadata)
      addMetadata instr = do
        funName <- getName =<< use currFunctionTP
        blockName <- getName =<< use currBlockTP
        line <- return 0 -- Bogus
        return (instr, Metadata funName blockName line False False False False)
               
      transpileInstr32M    :: InstrExt32M -> Seq (MAInstruction Int MWord) 
      transpileInstr64M    :: InstrExt64M -> Seq (MAInstruction Int MWord) 
      transpileInstralias  :: AliasInstr  -> Seq (MAInstruction Int MWord)
      transpileInstr32M    instr = undefined
      transpileInstr64M    instr = undefined
      transpileInstralias  instr = undefined

transpileInstrPseudo :: PseudoInstr -> Statefully (Seq (MAInstruction Int MWord))  
transpileInstrPseudo instr =
  Seq.fromList <$>
  case instr of 
    RetPI -> return [Ijmp . AReg $ tpReg X1]
    CallPI Nothing off -> do
      off' <- tpAddress off
      -- MicroRam has no restriction on the size of offsets,
      -- So there is no need to use `auipc`
      return [Imov (tpReg X1) (pcPlus 2),
       Ijmp $ off']
    CallPI (Just rd) off -> do
      off' <- tpAddress off
      -- MicroRam has no restriction on the size of offsets,
      -- So there is no need to use `auipc`
      return [Imov (tpReg rd) (pcPlus 2),
              Ijmp $ off']
    TailPI off -> do
      off' <- tpAddress off
      -- From the RiscV Manual, tail calls are just a call that uses
      -- the X6 register (page 140 table 25.3)
      return [Imov (tpReg X1) (pcPlus 2),
       Ijmp $ off']
    FencePI -> return []
    LiPI rd imm-> do
      (rd',imm') <- tpRegImm rd imm
      -- MicroRam has no restriction on the size of offsets, so there
      -- is no need to use 'lui, addi, slli, addi'. We can directly
      -- mov the constant
      return [Imov rd' (LImm imm')]
    NopPI ->
      -- We are already changing the alignemnt.
      -- can we ignore nop's (i.e. return [])
      return [Iadd (tpReg X0) (tpReg X0) (LImm 0) ]
    AbsolutePI _ -> error "AbsolutePI" -- AbsolutePseudo
    UnaryPI  unop reg1 reg2  -> return $ unaryPseudo unop reg1 reg2  -- UnaryPseudo Reg Reg
    CMovPI  _ _ _-> error "1" -- CMovPseudo Reg Reg
    BranchZPI bsp rd off -> pseudoBranch bsp rd off  -- BranchZPseudo Reg Offset
    BranchPI  _ _ _ _-> error "3" -- BranchPseudo Reg Reg Offset
    JmpImmPI  JPseudo off -> do
      off' <- tpImm off
      return [Ijmp . LImm $ off']
    JmpImmPI  JLinkPseudo off -> do
      off' <- tpImm off
      return [Imov (tpReg X1) (pcPlus 2),
               Ijmp . LImm $ off']
    JmpRegPI  _ _ -> error "5" -- JumpPseudo Reg
  where
    unaryPseudo :: UnaryPseudo -> Reg -> Reg  -> [MAInstruction Int MWord]
    unaryPseudo unop reg1 reg2 = 
      let (rs1',rs2) = (tpReg reg1, tpReg reg2) in
        let rs2' = AReg rs2 in 
          case unop of
            MOV   -> [Imov rs1' rs2']  
            NOT   -> [Inot rs1' rs2']
            NEG   -> [Imov newReg (LImm 0), Isub rs1' newReg rs2']
            NEGW  -> [Imov newReg (LImm 0), Isub rs1' newReg rs2']
            SEXTW -> [Imov rs1' rs2'] <> restrictAndSignExtendResult rs1'
        
    
    pseudoBranch :: BranchZPseudo ->  Reg ->  Offset -> Statefully [MAInstruction Int MWord]
    pseudoBranch bsp rs1 off = do
      (rs1',off') <- tpRegImm rs1 off
      let (computCond, negate) = condition bsp rs1'
      return $ computCond : [(if negate then Icnjmp newReg else Icjmp newReg) 
                     $ LImm off']
      

    
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

transpileInstr64I    :: InstrRV64I  -> Statefully (Seq (MAInstruction Int MWord)) 
transpileInstr64I    instr =
  Seq.fromList <$>
  case instr of
    MemInstr64 mop r1 off r2 -> memInstr64 mop r1 off r2  
    ImmBinop64 binop64I reg1 reg2 imm  -> transpileImmBinop64I binop64I reg1 reg2 imm
    RegBinop64 binop64  reg1 reg2 reg3 -> return $ transpileRegBinop64I binop64  reg1 reg2 reg3
  where
    transpileImmBinop64I :: Binop64I -> Reg -> Reg -> Imm -> Statefully [MAInstruction Int MWord]
    transpileImmBinop64I binop64I reg1 reg2 imm = do
      (rd',rs1',off'') <- tpRegRegImm reg1 reg2 imm
      let off' = LImm off''
      return $ (case binop64I of
                  ADDIW -> [Iadd rd' rs1' off']
                  -- It should be true that the  'off' < 2^5'
                  -- but we do not check for it. 
                  SLLIW -> [Ishl rd' rs1' off']
                  SRLIW -> [Iand newReg rs1' (LImm $ 2^32 - 1),
                            Ishr rd' rs1' off']
                  SRAIW -> error "Arithmetic right shift not implemented (64I)" -- TODO
               ) <> restrictAndSignExtendResult rd'
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
            
            

            
        
    memInstr64 :: MemOp64 -> Reg -> Offset -> Reg -> Statefully [MAInstruction Int MWord]
    memInstr64  mop r1 off r2 = do
      (rd',rs1',off'') <- tpRegRegImm r1 r2 off
      let off' = LImm off''
      return $ case mop of
        -- unsigned load
        LWU -> [Iadd newReg rs1' off',
                 Iload W1 rd' (AReg newReg)]
        -- double mems
        LD  -> [Iadd newReg rs1' off',
                 Iload W1 rd' (AReg newReg)]
        SD  -> [Iadd newReg rs1' off', 
                     Istore W1 (AReg newReg) rd' ]
      
transpileInstr32I :: InstrRV32I -> Statefully (Seq (MAInstruction Int MWord)) 
transpileInstr32I instr =
  Seq.fromList <$>
  case instr of
    JAL rd off -> do
      (rd', off') <- tpRegImm rd off 
      return [Imov rd' (pcPlus 2), -- Or is it 8?
               Ijmp $ LImm off'] -- Is our instruction numbering compatible?
    JALR rd rs1 off -> do
      (rd',rs1',off') <- tpRegRegImm rd rs1 off 
      return [Iadd newReg rs1' (LImm off'), -- this instruction must go first, in case rd=rs1
              Imov rd' (pcPlus 2), -- or is it 8? 
              Ijmp $ AReg newReg] -- Is our instruction numbering compatible?
    BranchInstr cond src1 src2 off -> do
      (src1',src2',off') <- tpRegRegImm src1 src2 off
      let (computCond, negate) = condition cond src1' src2'
      return $ computCond : [(if negate then Icnjmp newReg else Icjmp newReg) 
                             $ LImm off']
    MemInstr32 memOp32 reg1 off reg2  -> memOp32Instr memOp32 reg1 off reg2 
    LUI reg imm -> do
      (reg',imm') <- tpRegImm reg imm 
      return [Imov reg' (LImm $ lazyUop luiFunc imm')]
    AUIPC reg off -> do
      (reg',off') <- tpRegImm reg off 
      return [Imov reg' (LImm $ lazyPc + lazyUop luiFunc off')]
    ImmBinop32 binop32I reg1 reg2 imm -> transpileImmBinop32I binop32I reg1 reg2 imm
    RegBinop32 binop32 reg1 reg2 reg3 -> return $ transpileRegBinop32I binop32  reg1 reg2 reg3
    FENCE setOrdering                 -> undefined -- Probably a noop?
    FENCEI                            -> undefined -- Probably a noop?
  where
    -- Memory operations
    -- Big TODO TODO TODO
    memOp32Instr :: MemOp32 -> Reg -> Offset -> Reg -> Statefully [MAInstruction Int MWord]
    memOp32Instr memOp32 reg1 off reg2 = do
      (rd',rs1',off'') <- tpRegRegImm reg1 reg2 off
      let off' = LImm off''
      return $ case memOp32 of
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
    transpileImmBinop32I :: Binop32I -> Reg -> Reg -> Imm -> Statefully [MAInstruction Int MWord]
    transpileImmBinop32I binop reg1 reg2 imm = do
      (rd',rs1',off') <- tpRegRegImm reg1 reg2 imm
      let off_imm = LImm off'
      return $ case binop of
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
  _ <- saveObject
  -- Build program
  prog <- use commitedBlocksTP
  -- Add a premain. We need this to be backwards compatible
  let prog' = (NBlock (Just premainName) premainCode): prog
  -- Build memory
  genv <- use genvTP
  -- Then build the CompilationUnit
  return $ CompUnit {
    -- Memory is filled in 'RemoveLabels'
    programCU = ProgAndMem prog' [] mempty,
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
  where premainCode =
          -- bp is a caller saved reg that keeps the return address.
          [(Imov bp (pcPlus 2), md),
           -- Call main
           (Ijmp $ Label mainName, md {mdIsCall = True}),
           -- When main returns, answer (risk stores answer in a0==X10)
           (Ianswer (AReg $ tpReg X10),md {mdIsReturn = True})]

        md = trivialMetadata premainName defaultName

-- Finalize current block and commit it
-- i.e. add it to the list.
commitBlock :: Statefully ()
commitBlock = do
  contnt <- use currBlockContentTP
  when (not $ null contnt) $ do
    currBlockContentTP .= mempty
    name <- getName =<< use currBlockTP
    let block = NBlock (Just $ name) $ toList contnt
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









