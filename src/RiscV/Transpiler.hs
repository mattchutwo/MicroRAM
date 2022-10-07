{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RiscV.Transpiler where -- (transpiler)

import Control.Monad.State
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Text (unpack, pack)
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe, isJust)
import Data.Foldable (toList)
import Data.List (foldl')
-- import Test.QuickCheck (Arbitrary, arbitrary, oneof)
import Compiler.IRs
import Compiler.Metadata
import Compiler.Errors
import MicroRAM
import Compiler.Common
import Compiler.Registers (RegisterData( NumRegisters ))
-- import Compiler.Analysis (AnalysisData)
import Compiler.CompilationUnit
import Compiler.IRs(lazyPc, hereLabel)
import Compiler.LazyConstants
import Compiler.Analysis (AnalysisData(..))
-- import Compiler.Registers

import qualified Data.List as List (partition)
import qualified Data.Map as Map
import  Data.Bits (shiftL, (.|.),(.&.))
import Data.Char (ord)

import Debug.Trace (trace)

import RiscV.RiscVAsm

import Control.Lens (Lens, lens, makeLenses, ix, at, to, (^.), (.=), (%=), (?=), _Just, use, over, Lens')


-- | Option
_warningOn :: Bool
_warningOn = True

-- | Unsafe: alerts of a problem, but not quite an error.
warning :: String -> a -> a
warning msg a | _warningOn = trace msg a
              | otherwise = a

-- data MemEntry =
     
data MemData = MemData
  -- | data content in section
  { _mdData :: Seq.Seq (LazyConst MWord, Int)
  -- | Data is entered by bytes but stored by words. This pointer is
  -- the next available byte-aligned address
  , _mdNextData :: Int
  -- | Locations of objects by name
  , _mdEntryPoints :: Map.Map Name MWord
  }
  deriving (Show, Eq)

emptyMemData :: MemData
emptyMemData = MemData mempty 0 mempty

makeLenses ''MemData

data Section = Section
  { _secName :: String
  -- From the tags, only the options we care about
  , _flag_exec :: Bool  -- ^ executable "e" 
  , _flag_write :: Bool -- ^ Writable  "w"
  -- | section type
  , _secType :: Maybe SectionType
  , _secContent :: Seq.Seq LineOfRiscV
  -- | data
  , _secData :: MemData
  , _secMaxAlign :: Int                  -- ^ Max alignment of all objects in section
  }
  deriving (Show, Eq)

execSection, writeSection, readSection :: Section
readSection =   Section { _secName = ""
                        , _flag_exec = False 
                        , _flag_write = False
                        , _secType = Nothing
                        , _secContent = Seq.empty
                        -- data
                        , _secData = emptyMemData
                        , _secMaxAlign = 1
                        }
writeSection =  readSection { _flag_write = True }
execSection =   readSection { _flag_exec = True }

flagSection :: String -> [Flag] -> Maybe SectionType ->  Seq.Seq LineOfRiscV -> Section
flagSection name flags maybeType content = Section { _secName = name
                                                   , _flag_exec = Flag_x `elem` flags
                                                   , _flag_write = Flag_w `elem` flags
                                                   , _secType = maybeType
                                                   , _secContent = content
                                                   -- data
                                                   , _secData = emptyMemData
                                                   , _secMaxAlign = getAlignFlag
                                           }
  where
    getAlignFlag = fromEnum $ maximum $ mapMaybe
                   (\case
                       Flag_number n -> Just n
                       _ -> Nothing
                   ) (Flag_number 1 : flags)
                   
          
makeLenses ''Section







                                                   

---------------                                                   
-- New Version
---------------

      

data SymbolTP = SymbolTP
  { _symbName  :: String                    
  , _symbSize  :: Maybe (Either Integer Imm)             
  , _symType   :: Maybe DirTypes
  } deriving (Show)
defaultSym :: SymbolTP
defaultSym = SymbolTP "Default Name" Nothing Nothing 

makeLenses ''SymbolTP
              
data TPState = TPState
  { _currSectionTP  :: Section
  , _currFunctionTP :: String
  , _currBlockTP    :: Maybe String
  , _currBlockContentTP :: Seq.Seq (MAInstruction Int MWord, Metadata)
  , _commitedBlocksTP :: Seq.Seq (NamedBlock Metadata Int MWord)
  , _sectionsTP :: Map.Map String Section
    
  -- Memory
  , _curObjectTP :: String
  -- , _curObjectContentTP :: Maybe ( Seq (LazyConst MWord, Int))
  -- , _genvTP :: GEnv MWord

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

initStateTP :: Word -> Map.Map String Name -> TPState
initStateTP firstUnusedName nameMap  = TPState
  { _currSectionTP      = readSection { _secName = "initSection"}
  , _currFunctionTP     = "NoneInitFunction"
  , _currBlockTP        = Nothing 
  , _currBlockContentTP = Seq.empty
  , _commitedBlocksTP   = Seq.empty
  , _sectionsTP         = Map.empty
  , _curObjectTP        = "NoneInitObj"
  , _atFuncStartTP      = False -- An instruction type needs to be found first.
  , _afterFuncCallTP    = False
  , _symbolTableTP      = Map.empty
  , _nameIDTP           = firstUnusedName
  , _nameMap            = Map.insert "main" mainName nameMap
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

transpiler :: Bool
           -> Word
           -> Map.Map String Name
           ->  [LineOfRiscV]
           -> Hopefully (CompilationUnit (GEnv MWord) (MAProgram Metadata Int MWord))
transpiler verb firstUnusedName nameMap rvcode =
  evalStateT (mapM transpilerLine rvcode >> finalizeTP)
  (initStateTP firstUnusedName nameMap)
  where 
    transpilerLine :: LineOfRiscV -> Statefully ()
    transpilerLine (LabelLn lbl) =
      ifM (use $ currSectionTP . flag_exec) (codeLbl lbl) (memLbl lbl) 
    transpilerLine (Directive dir) =     transpileDir verb dir
    transpilerLine (Instruction instr) = transpileInstr instr

codeLbl,memLbl :: String -> Statefully ()
codeLbl lbl = do
  -- Commit current block
  commitBlock
  -- Start a new block
  currBlockTP .= Just lbl
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
  -- Start a new object
  curObjectTP .= lbl
  objName <- getName lbl
  -- Mark data with a new entry point
  dataPointer <- use $ currSectionTP . secData . mdNextData
  currSectionTP . secData . mdEntryPoints %= (Map.insert objName (toEnum dataPointer))  

sectionVariable :: Section -> Statefully (Maybe (GlobalVariable MWord))
sectionVariable (Section _sectionName True _write _secTy _code _mdata _align) = return Nothing
sectionVariable (Section sectionName False write _secTy _code (MemData d _ entries) align) = do
  secName <- getName sectionName
  let initBuffer = packInWords $ toList d
  return $ Just $ GlobalVariable
    { globSectionName = secName
    , entryPoints = Map.toList entries
    , isConstant = not write
    , initializer = Just $ initBuffer
    , gSize = toEnum $ length initBuffer
    , gAlign = toEnum align
    , secret = sectionName == "__DATA,__secret" ||
               sectionName == ".data.secret"
    -- What variables are not heap-init in RiscV?
    , gvHeapInit = True}
  
  -- nameCurObj <- use curObjectTP
  -- -- Get the current obejct
  -- curObj <- use $ curObjectContentTP
  -- case curObj of
  --   Nothing -> return ()
  --   Just ls -> do
  --       let init = packInWords $ toList ls
  --       gname <- getName =<< use curObjectTP
  --       sectionName <- use (currSectionTP . secName)
  --       readOnly <- not <$> use (currSectionTP . flag_write)
  --       let gvar = GlobalVariable {globName = gname,
  --                                  -- entryPoints = 
  --                                  isConstant = readOnly,
  --                                  -- RiscV doesn't have types 
  --                                  gType = TVoid,
  --                                  initializer = Just init,
  --                   		   gSize = toEnum $ length init,
  --                                  -- Currently ignored
  --                                  -- TODO: implement alignment
  --                                  gAlign = 1,
  --                   		   secret = sectionName == "__DATA,__secret" ||
  --                                  sectionName == ".data.secret",
  --                                  -- What variables are not heap-init in RiscV?
  --                                  gvHeapInit = True}
  --       -- add it to the list of globals
  --       genvTP %= (:) gvar
  --       gvlen <- length <$> use genvTP 
  --       -- Clear current obje
  --       curObjectContentTP .= Nothing
      
  
transpileDir :: Bool -> Directive -> Statefully ()
transpileDir verb dir =
  case dir of
    -- Ignored 
    ALIGN align     -> alignment align Nothing Nothing 
    P2ALIGN a v m   -> alignment (2 ^ a) v m
    BALIGN a v      -> alignment a v Nothing
    FILE _          -> ignoreDire "FILE _          " 
    IDENT _         -> ignoreDire "IDENT _         "  -- just places tags in object files
    ADDRSIG         -> ignoreDire "ADDRSIG         " -- We ignore address-significance 
    ADDRSIG_SYM _nm -> ignoreDire "ADDRSIG_SYM _nm " -- We ignore address-significance 
    CFIDirectives _ -> ignoreDire "CFIDirectives _ " -- ignore control-flow integrity
    -- Currently unimplemented 
    Visibility _ _ -> ignoreDire "Visibility _ _" -- TODO: unimplementedDir -- Not in binutils. Remove?
    STRING st      -> emitString st True
    ASCII  st      -> emitString st False
    ASCIZ  st      -> emitString st True -- alias for string
    EQU _st _val   -> unimplementedDir
    OPTION _opt    -> unimplementedDir -- Rarely used
    VARIANT_CC _st -> unimplementedDir -- Not in binutils. Remove?
    SLEB128 _val   -> unimplementedDir -- How do we do this?
    ULEB128 _val   -> ignoreDire "ULEB128 _val" -- Only used for debugging/exceptions 
    MACRO _ _ _    -> unimplementedDir -- No macros.
    ENDM           -> unimplementedDir
    -- Implemented
    -- ## Declare Symbols 
    COMM   nm size align -> commSetSymbol nm size align
    COMMON nm size align -> commSetSymbol nm size align
    SIZE   nm size       -> setSymbol nm (Just $ Right size) Nothing        Nothing
    TYPE   nm typ        -> setSymbol nm Nothing             Nothing        (Just typ)
    -- ## Sections
    TEXT    ->                       setSection "text"   [Flag_x]    Nothing []
    DATA    ->                       setSection "data"   []          Nothing []
    RODATA  ->                       setSection "rodata" []          Nothing []
    BSS     ->                       setSection "bss"    []          Nothing []
    SECTION nm flags typ flagArgs -> setSection nm       flags typ     flagArgs
    -- ## Attributes (https://sourceware.org/binutils/docs/as/RISC_002dV_002dATTRIBUTE.html)
    ATTRIBUTE _tag _val -> ignoreDire "ATTRIBUTE _tag _val" -- We ignore for now, but it has some alignment infomrations 
    -- ## Emit data
    DirEmit typ val  -> mapM_ (emitValue $ emitSize typ) val
    -- This might be slow if there is a very large zero instruction.
    ZERO size        -> replicateM_ (fromInteger size) $ emitValue 1 (ImmNumber 0)

    where
      -- Alignment simply pushes `mfill` (default 0 or no-op) until
      -- the current pointer is aligned
      alignment :: Integer -> Maybe Integer -> Maybe Integer -> Statefully ()
      alignment align mfill mmaxFill = do
        let fill = maybe 0 fromInteger mfill
        let max = maybe maxBound fromInteger mmaxFill
        isExec <- use $ currSectionTP . flag_exec
        if isExec then alignExec fill max else alignData fill max
          where
            alignExec _fill _max =
              -- Code is always word-aligned. Produce warning if higher is requested:
              if align <= 16 then return () else
                (trace "Warning. Code alignment higher than 16 not supported" $ return ())

            alignData :: Int -> Int -> Statefully ()
            alignData fill maxFill = do
              pointer <- use $ currSectionTP . secData . mdNextData
              let paddingLength = -(pointer `mod` (-fromInteger align))
              sName <- use $ currSectionTP . secName
              --_ <- trace ("Align " <> show sName <> " at " <> show pointer <> " to " <> show align <> " ( need to add " <> show paddingLength <> ").") $ return ()
              when (paddingLength <= maxFill) $ do
                let padding = SConst (toEnum fill) -- ^ one byte of fill
                --_ <- trace ("\tFilling from " <> show pointer <> " to alignment of " <> show align <> " up to " <> show maxFill)  $ return ()
                replicateM_ paddingLength (pushMemVal 1 padding)    
      

      emitString :: String -> Bool -> Statefully ()
      emitString st nullTerminated = do
        pointer <- use $ currSectionTP . secData . mdNextData
        -- _ <- trace ("\tEmit String: "<> st) $ return ()
        mapM_ (\char -> emitValue 1 (ImmNumber $ toEnum $ ord char)) st
        when nullTerminated $ emitValue 1 (ImmNumber 0)
      ignoreDire name =
        (if verb then trace ("Ignored directive: " <> name) else id) return ()
      unimplementedDir = implError $ "Directive not yet implemented: " <> show dir

      -- | Comm and Common are special: They define a symbol whose
      -- value should be defined somwhere else.  Because we don't link
      -- with anything else, we interpret comm as declaring it an
      -- uninitialized value in a differetn, specialized section
      commSetSymbol  :: String                     -- ^ Name
                     -> Integer                    -- ^ Size
                     -> Integer                    -- ^ Alignment
                     -> Statefully ()
      commSetSymbol name size align = do
        -- temporarily change sections to put the uninitialized value somewhere else.
        Section secName sExec sWrite sType sContent sData sMAling <- use currSectionTP
        let commSectionName = ".data.comm" -- how to make this unique?
        setSection commSectionName [] Nothing [] 
        -- Align
        alignment align Nothing Nothing
        -- record position as if there was a label `name`
        memLbl name
        -- Fill uninitialized values
        replicateM_ (fromInteger size) (pushMemVal 1 0)
        -- now go back to the original section. `setSection` should
        -- find the section by name and reset all the content and
        -- flags correctly. We pass the correct flags, but they get
        -- ignored for the old ones
        let flags = [] ++ (if sExec then [Flag_x] else []) ++ (if sWrite then [Flag_w] else [])
        setSection secName flags sType [] -- TODO what about sData SMAlign 
        
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
              , _symType   = firstJust typ   (_symType   symbol) 
              }
        symbolTableTP . at name ?= symbol'

      -- | Pushes an Immediate value to the current memory object
      emitValue :: Int -> Imm -> Statefully ()
      emitValue size val = do
        -- Make Imm into a lazy constant and push into the current object
        pushMemVal size =<< tpImm val

      -- | pushes a memory value to the current memory object
      pushMemVal :: Int -> LazyConst MWord -> Statefully ()
      pushMemVal size lVal = do
        -- add the new value
        currSectionTP . secData . mdData  %= (:|> (lVal, size))
        -- Increase pointer
        currSectionTP . secData . mdNextData %= (+ size)
        
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
setSection name flags secTyp _flagArgs = do
  -- start of a new section closes previous sections. Save ongoing objects and commit blocks
  commitBlock
  --saveObject
  -- Save the current section
  saveSection
  -- Create ampty section in case it doesn't exist
  -- Note `FlagArg`s is ignored
  let initSection = flagSection name flags secTyp mempty
  --
  theSection <- fromMaybe initSection <$> use (sectionsTP . at name)
  currSectionTP .= theSection

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

-- Register shorcuts
-- NOTE these don't match the predefined shortcuts in Compiler/Registers.hs
-- Those are for MicroRAM only.
tp,gp,sp,ra,zero :: Int
tp   = fromEnum  X4 -- thread pointer
gp   = fromEnum  X3 -- global pointer
sp   = fromEnum  X2 -- stack pointer
ra   = fromEnum  X1 -- return address
zero = fromEnum  X0 -- hardwired zero


-- ## Immediates
tpImm :: Imm -> Statefully (LazyConst MWord)
tpImm imm = case imm of
              ImmNumber c -> return $ SConst c
              -- Note we ignore all symbol suffixes - we treat `memcpy@plt` the
              -- same as plain `memcpy`.  Real linkers do the same when
              -- `memcpy` is a local symbol.
              ImmSymbol str _ -> lazyAddrOf <$> getName str         
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
        --  pcrel_lo and pcrel_hi are computed just like lo and hi,
        --  but are relative to the current pc.
        ModPcrel_lo        -> error "pcrel_lo"
        ModPcrel_hi        -> error "pcrel_hi"
        ModGot_pcrel_hi    -> error "ModGot_pcrel_hi   "
        ModTprel_add       -> error "ModTprel_add      "
        ModTprel_lo        -> error "ModTprel_lo       "
        ModTprel_hi        -> error "ModTprel_hi       "
        ModTls_ie_pcrel_hi -> error "ModTls_ie_pcrel_hi"
        ModTls_gd_pcrel_hi -> error "ModTls_gd_pcrel_hi"

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
tpRegReg :: Reg -> Reg -> (Int, Int)
tpRegReg r1 r2 = (tpReg r1, tpReg r2)
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
  -- TODO remove writes to zero
  -- TODO2: simplify reads from 0
  -- TODO3: replace newReg (X32) with zero (X0)
  currBlockContentTP %= flip mappend instrMD -- Instructions are added at the end.
    where
      addMetadata :: MAInstruction Int MWord
                  -> Statefully (MAInstruction Int MWord, Metadata)
      addMetadata instr = do
        funName <- getName =<< use currFunctionTP
        blockName <- getName =<< (maybe "NoName" id) <$> (use currBlockTP)
        line <- return 0 -- Bogus
        return (instr, Metadata funName blockName line False False False False)
               
transpileInstralias  :: AliasInstr  -> Seq (MAInstruction Int MWord)
transpileInstralias  _ =
  -- This instruction should allways trap.  We do so by reading from
  -- address 0 which should be poisoned, and thus executing this
  -- instruction will make the trace invalid.
  Seq.fromList [Iload W8 newReg (LImm 0)]
  
    

transpileInstr64M    :: InstrExt64M -> Seq (MAInstruction Int MWord) 
transpileInstr64M    instr = 
  Seq.fromList $
  case instr of
    MULW  rd rs1 rs2 -> restrictedOperation rd rs1 rs2 Imull
    DIVW  rd rs1 rs2 -> restrictedOperation rd rs1 rs2 (error "Undefined: Signed div 32b")
    DIVUW rd rs1 rs2 -> restrictedOperation rd rs1 rs2 Iudiv
    REMW  rd rs1 rs2 -> restrictedOperation rd rs1 rs2 (error "Undefined: Signed rem 32b")
    REMUW rd rs1 rs2 -> restrictedOperation rd rs1 rs2 Iumod

  where restrictedOperation rd rs1 rs2 op =
          let (rd', rs1', rs2') = tpRegRegReg rd rs1 rs2 in
            [ -- Restrict input 1 to 32b
              Iand rs1' rs1' (LImm $ 2^32 - 1),
              -- Restrict input 2 to 32b
              Iand rs2' rs2' (LImm $ 2^32 - 1),
              -- do the operation
              op rd' rs1' (AReg rs2')
            ] <> restrictAndSignExtendResult rd'
                  
transpileInstr32M :: InstrExt32M -> Seq (MAInstruction Int MWord) 
transpileInstr32M instr =
  Seq.fromList [
  case instr of
    MUL    rd rs1 rs2 -> let (rd', rs1', rs2') = tpRegRegReg rd rs1 rs2 in Imull  rd' rs1' (AReg rs2')
    MULH   rd rs1 rs2 -> let (rd', rs1', rs2') = tpRegRegReg rd rs1 rs2 in Ismulh rd' rs1' (AReg rs2')
    MULHU  rd rs1 rs2 -> let (rd', rs1', rs2') = tpRegRegReg rd rs1 rs2 in Iumulh rd' rs1' (AReg rs2')
    DIVU   rd rs1 rs2 -> let (rd', rs1', rs2') = tpRegRegReg rd rs1 rs2 in Iudiv  rd' rs1' (AReg rs2')
    REMU   rd rs1 rs2 -> let (rd', rs1', rs2') = tpRegRegReg rd rs1 rs2 in Iumod  rd' rs1' (AReg rs2')
    MULHSU rd rs1 rs2 -> let (rd', rs1', rs2') = tpRegRegReg rd rs1 rs2 in error "Undefined: Signed Unsigned ultiplication"
    DIV    rd rs1 rs2 -> let (rd', rs1', rs2') = tpRegRegReg rd rs1 rs2 in error "Undefined: Signed division"
    REM    rd rs1 rs2 -> let (rd', rs1', rs2') = tpRegRegReg rd rs1 rs2 in error "Undefined: Signed reminder"
  ]


    
transpileInstrPseudo :: PseudoInstr -> Statefully (Seq (MAInstruction Int MWord))  
transpileInstrPseudo instr =
  Seq.fromList <$>
  case instr of 
    RetPI -> do
      -- Return from main, is special
      function <- use currFunctionTP
      if function == "main" then
        -- Answers the value in a0 (i.e. X10)
        -- return [Ianswer . AReg $ tpReg X10]
        -- lets try without the special case. TODO: if successfull, delete the branch
        return [Ijmp . AReg $ tpReg X1]
        else 
        return [Ijmp . AReg $ tpReg X1]
    CallPI Nothing off -> do
      off' <- tpAddress off
      -- MicroRam has no restriction on the size of offsets,
      -- So there is no need to use `auipc`
      return [
        -- for debugging
        -- Iext (XTrace (pack $ "CALL: " <> show off) []),
        Imov (tpReg X1) (pcPlus 2),
       Ijmp $ off']
    CallPI (Just rd) off -> do
      off' <- tpAddress off
      -- MicroRam has no restriction on the size of offsets,
      -- So there is no need to use `auipc`
      return [
        -- for debugging
        Iext (XTrace (pack $  "CALL: " <> show off) []),
        Imov (tpReg rd) (pcPlus 2),
        Ijmp $ off']
    TailPI off -> do
      off' <- tpAddress off
      -- From the RiscV Manual, tail calls are just a call that uses
      -- the X6 register (page 140 table 25.3)
      return [
        -- for debugging
        Iext (XTrace (pack $  "TAIL CALL: " <> show off) []),
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
    CmpFlagPI cond r1 r2 ->
      let (r1', r2') = tpRegReg r1 r2 in 
        return [computeCondition cond r1' r2']
    BranchZPI bsp rd off -> pseudoBranch bsp rd off  -- BranchZPseudo Reg Offset
    BranchPI branchPI r1 r2 offset ->
      -- From the RISC-V Instruction Set Manual:
      -- "Note, BGT, BGTU, BLE, and BLEU can be synthesized by
      -- reversing the operands to BLT, BLTU, BGE, and BGEU,
      -- respectively."
      transpileBranch32 (case branchPI of
                            BGT  -> BLT
                            BLE  -> BGE
                            BGTU -> BLTU 
                            BLEU -> BGEU
                        )  r2 r1 offset
    JmpImmPI  JPseudo off -> do
      off' <- tpImm off
      return [Ijmp . LImm $ off']
    JmpImmPI  JLinkPseudo off -> do
      off' <- tpImm off
      return [Imov (tpReg X1) (pcPlus 2),
               Ijmp . LImm $ off']
    JmpRegPI JLinkPseudo r1 ->
      let r1' = tpReg r1 in 
        return [Imov (tpReg X1) (pcPlus 2),
                Ijmp (AReg r1')]
    JmpRegPI  JPseudo r1 -> 
      let r1' = tpReg r1 in 
        return [Ijmp (AReg r1')]
  where
    computeCondition :: CmpFlagPseudo -> Int -> Int -> MAInstruction Int MWord
    computeCondition cond ret r1 =
      case cond of
        SEQZ -> Icmpe ret r1 (LImm 0)
        -- `r1 != 0` is the same as `r1 > 0` (unsigned)
        SNEZ -> Icmpa ret r1 (LImm 0)
        -- `r1 < 0` is the same as `0 > r1` (signed)
        SLTZ -> Icmpg ret (tpReg X0) (AReg r1)
        SGTZ -> Icmpg ret r1 (LImm 0)
    
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
                  ADDIW -> [Iadd rd' rs1' off']<> restrictAndSignExtendResult rd'
                  -- It should be true that the  'off' < 2^5'
                  -- but we do not check for it. 
                  SLLIW -> [Ishl rd' rs1' off']<> restrictAndSignExtendResult rd'
                  SRLIW -> [Iand newReg rs1' (LImm $ 2^32 - 1),
                            Ishr rd' rs1' off']<> restrictAndSignExtendResult rd'
                  SRAIW -> -- TODO There is probably a more optimal way to do this one             
                    [ -- restrict input to 32bits
                      Iand rs1' rs1' (LImm $ 2^32 - 1),
                      -- Get sign bit 
                      Ishr newReg rs1' (LImm 31),
                      -- Extension (32 1's or 0's, followed by 32 0's)
                      Imull newReg newReg (LImm (2^64-2^32)),
                      -- fix highest bits
                      Ior rs1' rs1' (AReg newReg),
                      -- Logical shift
                      Ishr rd' rs1' off',
                      -- fix highest bits
                      Ior rs1' rs1' (AReg newReg)
                    ]
               ) 
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
            ADDW -> [Iadd rd' rs1' (AReg rs2')] <>
                    restrictAndSignExtendResult rd'                             
            SUBW -> [Isub rd' rs1' (AReg rs2')] <>
                    restrictAndSignExtendResult rd'                                
            SLLW -> [Iand newReg rs2' (LImm $ 2^5 - 1), -- restrict input to 5b
                     Ishl rd' rs1' (AReg newReg)] <>
                    restrictAndSignExtendResult rd'                                
            SRLW -> [Iand newReg rs1' (LImm $ 2^32 - 1), -- restrict input to 32b
                     Iand rd'    rs2' (LImm $ 2^5 - 1), -- restrict input to 5b
                     Ishr rd' newReg (AReg rd')] <>
                    restrictAndSignExtendResult rd'                                 
            SRAW -> -- Logical Shift right
                    -- We make the high 32 bits the right sign (i.e. 0 or 1) before and after a logical shift.              
              [ -- restrict input to 32bits
                Iand rs1' rs1' (LImm $ 2^32 - 1),
                -- Get sign bit 
                Ishr newReg rs1' (LImm 31),
                -- Extension (32 1's or 0's, followed by 32 0's)
                Imull newReg newReg (LImm (2^64-2^32)),
                -- fix highest bits
                Ior rs1' rs1' (AReg newReg),
                -- Logical shift
                Ishr rd' rs1' (AReg rs2'),
                -- fix highest bits
                Ior rs1' rs1' (AReg newReg)
              ]
            )
            
            

            
        
    memInstr64 :: MemOp64 -> Reg -> Offset -> Reg -> Statefully [MAInstruction Int MWord]
    memInstr64  mop r1 off r2 = do
      (rd',rs1',off'') <- tpRegRegImm r1 r2 off
      let off' = LImm off''
      return $ case mop of
        -- unsigned load
        LWU -> [Iadd newReg rs1' off',
                 Iload W4 rd' (AReg newReg)]
        -- double mems
        LD  -> [Iadd newReg rs1' off',
                 Iload W8 rd' (AReg newReg)]
        SD  -> [Iadd newReg rs1' off', 
                 Istore W8 (AReg newReg) rd' ]
      
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
    BranchInstr cond src1 src2 off ->
      transpileBranch32 cond src1 src2 off
    MemInstr32 memOp32 reg1 off reg2  -> memOp32Instr memOp32 reg1 off reg2 
    LUI reg imm -> do
      (reg',imm') <- tpRegImm reg imm 
      return [Imov reg' (LImm $ lazyUop luiFunc imm')]
    AUIPC reg off -> do
      (reg',off') <- tpRegImm reg off 
      return [Imov reg' (LImm $ lazyPc + lazyUop luiFunc off')]
    ImmBinop32 binop32I reg1 reg2 imm -> transpileImmBinop32I binop32I reg1 reg2 imm
    RegBinop32 binop32 reg1 reg2 reg3 -> return $ transpileRegBinop32I binop32  reg1 reg2 reg3
    FENCE predOrder succOrder         -> return []
    FENCEI                            -> return []
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
                 Iload W1 rd' (AReg rd')] <>
               signExtend 8 rd'
        LH  -> [Iadd newReg rs1' off', 
                 Iload W2 rd' (AReg newReg)]<>
               signExtend 16 rd'
        LW  -> [Iadd newReg rs1' off', 
                 Iload W4 rd' (AReg newReg)] <>
               signExtend 32 rd'
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
        where
          -- | Sign extend the value in rd
          signExtend :: Int -> Int -> [MAInstruction Int MWord]
          signExtend len rd =
            [-- get the sign
              Ishr newReg rd (LImm $ SConst $ toEnum $ len - 1),
              -- make a sign extension mask (a prefix of o's or 1's)
              Imull newReg newReg (LImm $ 2^64 - 2^len),
              -- add the sign extension bits
              Ior  rd newReg (AReg rd)]
                  
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
            SRA  -> error "Arithmetic right shift not implemented FULL" -- TODO
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
                 SRAI  -> [-- sign bits
                   Ishr newReg rs1' (LImm $ 63),
                   -- extensions (2^off-1)*(2^(64-off))
                   Imull newReg newReg (LImm $ (2 `pow` off' - 1) * (2 `pow` (64-off'))),
                   -- logical extension
                   Ishr rd' rs1' off_imm,
                   -- add extensions
                   Ior rd' rd' (AReg newReg)
                   ]
      
    
    -- build 32-bit constants and uses the U-type format. LUI
    -- places the U-immediate value in the top 20 bits of the
    -- destination register rd, filling in the lowest 12 bits
    -- with zeros. (We don't cehck the immediate for overflow,
    -- but it could technically be larger than 20bits)
    luiFunc :: MWord -> MWord
    luiFunc w = shiftL w 12
    
transpileBranch32
  :: BranchCond
  -> Reg -> Reg -> Imm
  -> StateT TPState Hopefully [Instruction' Int Int (MAOperand Int MWord)]
transpileBranch32 cond src1 src2 off = do
  (src1',src2',off') <- tpRegRegImm src1 src2 off
  let (computCond, neg) = condition cond src1' src2'
  return $ computCond : [(if neg then Icnjmp newReg else Icjmp newReg) 
                          $ LImm off']
    where
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
  _ <- saveSection
  -- Build program
  prog <- toList <$> (use commitedBlocksTP)
  -- Add a premain. We need this to be backwards compatible
  let prog' = (makeNamedBlock (Just premainName) premainCode): prog
  -- Build memory
  sections :: Map.Map String Section <- use sectionsTP
  genv <- makeGEnv sections 
  -- Then build the CompilationUnit
  return $ CompUnit {
    -- Memory is filled in 'RemoveLabels'
    programCU = ProgAndMem prog' [] mempty,
    -- TraceLen is bogus
    traceLen = 0,
    -- We added one new register, which is now the largest
    regData = NumRegisters (newReg+1),
    -- Analysis data is bogus
    aData = AnalysisData mempty mempty,
    -- Name bound is currently bogus, but we should fix it
    -- TODO: fix name bound.
    nameBound = 0,
    intermediateInfo = genv } 
  where premainCode =
          -- bp is a caller saved reg that keeps the return address.
          -- although we use a special case for returns from main. 
          [(Imov ra (pcPlus 4), md),
           -- poison 0
           (IpoisonW (LImm 0) 0, md),
           -- Set the top of the stack.
           (Imov sp (LImm initAddr), md),
           -- Call main
           (Ijmp $ Label mainName, md {mdIsCall = True}),
           -- When main returns, answer (risk stores answer in a0==X10)
           (Ianswer (AReg $ tpReg X10),md {mdIsReturn = True})]

        md = trivialMetadata premainName defaultName
        -- Start stack at 2^32.
        initAddr = 1 `shiftL` 32
  
        makeGEnv :: Map.Map String Section -> Statefully [GlobalVariable MWord]
        makeGEnv m = mapMaybeM sectionVariable (Map.elems m)   
  
        -- | A version of 'mapMaybe' that works with a monadic predicate.
        mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
        mapMaybeM op = foldr f (pure [])
          where f x xs = do x <- op x; case x of Nothing -> xs; Just x -> do xs <- xs; pure $ x:xs

-- Finalize current block and commit it
-- i.e. add it to the list.
commitBlock :: Statefully ()
commitBlock = do
  maybeName <- use currBlockTP
  case maybeName of
    Nothing -> return ()
    Just name' -> do
      name <- getName name' 
      contnt <- use currBlockContentTP
      currBlockContentTP .= mempty
      -- name <- getName =<< use currBlockTP
      let block = makeNamedBlock (Just name) $ toList contnt
      commitedBlocksTP %= (:|> block)
  currBlockTP .= Nothing
  
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









