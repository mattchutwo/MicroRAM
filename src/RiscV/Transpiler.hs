module RiscV.Transpiler where -- (transpiler)

import Control.Monad (when)
import Control.Monad.State
import Data.Default (def)
import Data.Maybe (listToMaybe, mapMaybe)
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
import qualified Data.Map as Map (empty)

import Debug.Trace (trace)

import RiscV.RiscVAsm

-- | Option
_warningOn :: Bool
_warningOn = True

-- | Unsafe: alerts of a problem, but not quite an error.
warning :: String -> a -> a
warning msg a | _warningOn = trace msg a
              | otherwise = a
              
data Section = Section
  { secName :: String
  -- From the tags, only the options we care about
  , flag_exec :: Bool  -- ^ executable "e" 
  , flag_write :: Bool -- ^ Writable  "w"
  -- | section type
  , secType :: Maybe SectionType
  , secContent :: [LineOfRiscV]
  }
  deriving (Show, Eq, Ord)

execSection, writeSection, readSection :: Section
readSection =   Section { secName = ""
                        , flag_exec = False 
                        , flag_write = False
                        , secType = Nothing
                        , secContent = []
                        }
writeSection =  readSection { flag_write = True }
execSection =   readSection { flag_exec = True }

flagSection :: String -> [Flag] -> Maybe SectionType ->  [LineOfRiscV] -> Section
flagSection name flags maybeType content = Section { secName = name
                                                   , flag_exec = Flag_e `elem` flags
                                                   , flag_write = Flag_w `elem` flags
                                                   , secType = maybeType
                                                   , secContent = content
                                           }

-- | Translating compiler from RiscV Assembly to MicroAssembly
transpiler :: [LineOfRiscV] -> Hopefully (CompilationUnit [GlobalVariable MWord] (MAProgram Metadata regT MWord))
transpiler rvProg = do
  -- First remove pointless directives
  let cleanProg = cleanUP rvProg
  -- Works in several steps:
  -- 1. Filter undeeded sections and separate the
  -- rest in into excutable and data
  -- 2.1 Translate executable code to MicroAssembly
  -- 2.2 Translate data into InitialMemory
  (execs, datas) <- separateSections cleanProg
  prog <- transpileCode execs
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
  when (left /= []) $ assumptError $ "Some lines outside a section: \n" <> show left
  sections <- mapM makeSection sectionParts
  let neededSecs = filterUnneeded sections
  return $ List.partition flag_exec neededSecs
  
  where
    -- | Check if a line is the begining of a section, by testing if
    -- it could create a section
    isSectionStart :: LineOfRiscV -> Bool
    isSectionStart line = not . isError $ makeSection (line, [])
    
    -- | Like Split, but keeps the separators
    splitKeep :: (a -> Bool) -> [a] -> ([a],[(a,[a])])
    splitKeep _f [] = ([],[])
    splitKeep f (x:xs) | f x , (left, rest) <- splitKeep f xs = ([], (x,left):rest)
    splitKeep f (x:xs) | (left, rest) <- splitKeep f xs = (x:left, rest)

    makeSection :: (LineOfRiscV, [LineOfRiscV]) -> Hopefully Section
    makeSection (Directive TEXT , contnt)   = return $ execSection { secName = "text", secContent = contnt }
    makeSection (Directive DATA ,contnt)   = return $ writeSection { secName = "data", secContent = contnt }
    makeSection (Directive RODATA ,contnt) = return $ readSection { secName = "rodata", secContent = contnt }
    makeSection (Directive BSS ,contnt)    = return $ writeSection { secName = "bss", secContent = contnt }
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
transpileCode :: [Section] -> Hopefully (CompilationUnit' () (MAProgram Metadata regT MWord))
transpileCode sections = compUnit . concat <$> mapM transpileSection sections
  where
    compUnit progCode =  CompUnit {programCU = progCode,
                               traceLen  = 0, -- Bogus
                               regData   = NumRegisters 32, -- hardcoded
                               aData     = def, -- Empty
                               nameBound = 0, -- Bogus
                               intermediateInfo = ()}
    transpileSection :: Section -> Hopefully [NamedBlock Metadata r w]
    transpileSection sec | not $ flag_exec sec = assumptError $ "Expected an executable section but " <> secName sec <> " is not." 
    transpileSection sec = transpileCode $ secContent sec

    transpileCode :: [LineOfRiscV] -> Hopefully [NamedBlock Metadata r w]
    transpileCode lines = do
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
  instrsMRAM <- lift $ concat <$> mapM (transpileInstr md) instrs
  return $ NBlock (Just $ quickName name) instrsMRAM
  
  where 

    -- Expects all function type declarations to be done right before the label in question.
    -- throws error if not the case.
    checkFunction :: String -> [Directive] -> StateT String Hopefully String
    checkFunction name dirs =
      case firstJust getFunctType dirs of
        Just nameT -> do
          when (name /= nameT) $ assumptError $ "Type definition name " <> nameT <> " doesn't match name of current block " <> name
          put name
          return name
        Nothing -> get

    getFunctType :: Directive -> Maybe String
    getFunctType (TYPE nameT DTFUNCTION) = Just nameT
    getFunctType _ = Nothing

transpileInstr :: Metadata -> Instr -> Hopefully [(MAInstruction r w, md)]
transpileInstr md instr = undefined
                    
-- | Translates sections of data into a list of global variables
transpileData :: [Section] -> Hopefully [GlobalVariable MWord]
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
firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f
