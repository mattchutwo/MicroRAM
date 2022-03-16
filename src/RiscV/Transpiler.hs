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
import MicroRAM (MWord)
import Compiler.Common
import Compiler.Registers (RegisterData( NumRegisters ))
-- import Compiler.Analysis (AnalysisData)
import Compiler.CompilationUnit

import qualified Data.List as List (partition)
import qualified Data.Map as Map

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

transpiler :: [LineOfRiscV] -> Hopefully (CompilationUnit (GEnv MWord) (MAProgram Metadata regT MWord))
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
      unimplementedDir = implError $ "Directive not yet implemented: " <> show dir

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
      emitValue = undefined


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








    
transpileInstr :: Instr -> Statefully ()
transpileInstr instr = do
  let instrs = case instr of
                 Instr32I instrRV32I     -> transpileInstr32I instrRV32I     
                 Instr64I instrRV64I     -> transpileInstr64I instrRV64I   
                 Instr32M instrExt32M    -> transpileInstr32M instrExt32M  
                 Instr64M instrExt64M    -> transpileInstr64M instrExt64M  
                 InstrPseudo pseudoInstr -> transpileInstrPseudo pseudoInstr
                 InstrAlias aliasInstr   -> transpileInstralias aliasInstr
  instrMD <- addMetadata instrs
  currBlockContentTP %= mappend instrMD
    where
      addMetadata :: Seq (MAInstruction Int MWord)
                  -> Statefully (Seq (MAInstruction Int MWord, Metadata))
      addMetadata = undefined            
      transpileInstr64I    :: InstrRV64I  -> Seq (MAInstruction Int MWord) 
      transpileInstr32M    :: InstrExt32M -> Seq (MAInstruction Int MWord) 
      transpileInstr64M    :: InstrExt64M -> Seq (MAInstruction Int MWord) 
      transpileInstrPseudo :: PseudoInstr -> Seq (MAInstruction Int MWord) 
      transpileInstralias  :: AliasInstr  -> Seq (MAInstruction Int MWord)

      transpileInstr32I :: InstrRV32I
                        -> Seq (MAInstruction Int MWord) 
      transpileInstr32I instr = undefined
        -- case instr of
        --   JAL reg off -> MicroRAM.Ijmp operand2
        --   JALR reg1 reg2 off
        --   BranchInstr BranchCond reg1 reg2 off
        --   MemInstr32 MemOp32 reg1 off reg2
        --   LUI reg imm
        --   AUIPC reg off
        --   ImmBinop32 Binop32I reg1 reg2 imm
        --   RegBinop32 Binop32 reg1 reg2 reg3
        --   FENCE SetOrdering
        --   FENCEI
      transpileInstr64I    instr = undefined
      transpileInstr32M    instr = undefined
      transpileInstr64M    instr = undefined
      transpileInstrPseudo instr = undefined
      transpileInstralias  instr = undefined
      
      

-- Utility functions for the transpiler

finalizeTP :: Statefully (CompilationUnit (GEnv MWord) (MAProgram Metadata regT MWord))
finalizeTP = undefined

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
