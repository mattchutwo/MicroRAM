{-# LANGUAGE TemplateHaskell #-}

module RiscV.Transpiler where -- (transpiler)

import Control.Monad (when)
import Control.Monad.State
import Data.Default (def)
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)
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

import Control.Lens (makeLenses, ix, at, to, (^.), (.=), (%=), (?=), use, over, Lens', _1, _2, _3)



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
  , _sectionsTP :: Map.Map String Section
    
  -- Memory
  , _curObject :: GlobalVariable MWord
  , _denvTP :: GEnv MWord

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
  { _currSectionTP      =  readSection {_secName = "NoneInit" }
  , _currFunctionTP     = "NoneInit"
  , _currBlockTP        = "NoneInit"
  , _currBlockContentTP = Seq.empty
  , _sectionsTP         = Map.empty
  , _atFuncStartTP      = False -- An instruction type needs to be found first.
  , _afterFuncCallTP    = False
  }
cunitFromState :: TPState -> CompilationUnit (GEnv MWord) (MAProgram Metadata regT MWord)
cunitFromState = undefined

  
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
      whenL atFuncStartTP (currFunctionTP .= lbl)
      currBlockTP .= lbl 
    memLbl lbl =
      curObject %= (\obj -> obj {globName = quickName lbl})

transpileDir :: Directive -> Statefully ()
transpileDir dir =
  case dir of
    -- Ignored 
    ALIGN _         -> ignoreDire -- We are ignoring alignment
    P2ALIGN a val m -> ignoreDire -- We are ignoring alignment
    BALIGN _ _      -> ignoreDire -- We are ignoring alignment
    FILE _          -> ignoreDire
    IDENT _         -> ignoreDire  -- just places tags in object files
    ADDRSIG         -> ignoreDire -- We ignore address-significance 
    ADDRSIG_SYM nm  -> ignoreDire -- We ignore address-significance 
    CFIDirectives _ -> ignoreDire -- ignore control-flow integrity
    -- Currently unimplemented 
    Visibility _ _ -> unimplementedDir -- Not in binutils. Remove?
    STRING st      -> unimplementedDir
    ASCIZ  st      -> unimplementedDir
    EQU st val     -> unimplementedDir
    OPTION opt     -> unimplementedDir -- Rarely used
    VARIANT_CC st  -> unimplementedDir -- Not in binutils. Remove?
    SLEB128 val    -> unimplementedDir -- How do we do this?
    ULEB128 val    -> unimplementedDir -- How do we do this?
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
    ATTRIBUTE tag val -> ignoreDire -- We ignore for now, but it has some alignment infomrations 
    -- ## Emit data
    DirEmit emit val  -> undefined
    ZERO size         -> undefined

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

      -- | Creates a section if it doens't exists and starts appending at the end of it
      setSection :: String                     -- ^ Name
                -> [Flag]                      -- ^ 
                -> (Maybe SectionType)         -- ^ 
                -> [FlagArg]                   -- ^ 
                -> Statefully ()               -- ^
      setSection = undefined -- TODO

      


        
        -- Map.findWithDefault (defaultSym name) name <$> use 
        return ()
     
                
    
transpileInstr :: Instr -> Statefully ()
transpileInstr = undefined


-- Utility functions for the transpiler

finalizeTP :: Statefully (CompilationUnit (GEnv MWord) (MAProgram Metadata regT MWord))
finalizeTP = undefined
    

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











-- Old version


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
