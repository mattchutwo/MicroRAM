{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compile where

import qualified Data.ByteString.Lazy                  as L
import Data.List

import Frontend.ClangCaller
import Util.Util

import MicroRAM
import Compiler
import Control.Monad(when)
import Compiler.CompilationUnit
import Compiler.Errors
import Compiler.IRs
import Compiler.Metadata
import Debug.PrettyPrint
import LLVMutil.LLVMIO

import Output.Output
import Output.CBORFormat

import PostProcess
import Segments (PublicSegmentMode(..))

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Exit

type CompiledProgram = CompilationResult (AnnotatedProgram Metadata AReg MWord)

main :: IO ()
main = do
  (options, file, len) <- getArgs >>= parseArgs
  fr <- parseOptions file len options
  when (beginning fr <= end fr) $ do
    putStrLn "Nothing to do here. Beginning comes later than end. Did you use -from-mram and -just-llvm? "
    exitSuccess
  -- --------------
  -- Run Frontend
  -- --------------
  when (beginning fr == CLang) $ frontend fr -- writes the resutl to llvmFile
  when (end fr >= LLVMLang) $ exitWith ExitSuccess
  -- --------------
  -- Run Backend
  -- --------------
  microProg::CompiledProgram <- case trLen fr of
                                  Nothing -> do
                                    putStrLn $ "Found no trace, can't compile."
                                    exitWith ExitSuccess
                                  Just trLength -> if (beginning fr >= LLVMLang) then -- Compile or read from file
                                                     callBackend trLength fr
                                                   else
                                                     readMRAMFile trLength fr
  when (ppMRAM fr) $ putStr $ microPrint (pmProg $ lowProg $ programCU microProg)
  saveMramProgram fr microProg
  when (end fr >= MRAMLang) $ exitWith ExitSuccess 
  -- --------------
  -- POST PROCESS
  -- --------------
  postProcessed <- postProcess fr microProg (privSegs fr)
  outputTheResult fr postProcessed
  exitWith ExitSuccess
  
  where -- FRONTEND
        frontend :: FlagRecord -> IO ()
        frontend fr = do
          -- Clang will error out if the output path is in a nonexistent directory.
          createDirectoryIfMissing True $ takeDirectory $ llvmFile fr
          output <- callClang (fr2ClangArgs fr)
          giveInfo fr output

        -- Backend
        callBackend :: Word -> FlagRecord -> IO $ CompiledProgram
        callBackend trLength fr = do  
          giveInfo fr "Running the compiler backend..."
          -- Retrieve program from file
          llvmModule <- llvmParse $ llvmFile fr
          -- compiler options
          let options = CompilerOptions
                { verb = verbose fr
                , allowUndefFun = allowUndefFunctions fr
                , spars = sparsNum fr
                , tainted = modeLeakTainted fr
                , skipRegisterAllocation = skipRegAlloc fr
                , numberRegs = Nothing
                } in 
          -- Then compile
            handleErrorWith (compile options trLength llvmModule)


        saveMramProgram :: Show a => FlagRecord -> a -> IO ()
        saveMramProgram fr microProg =
          case mramFile fr of 
            Just mramFileOut -> do
              giveInfo fr $ "Write MicroRAM program to file : " ++ mramFileOut
              writeFile mramFileOut $ show microProg
            Nothing -> return () 

        -- Read from MRAM file
        readMRAMFile trLength fr = do
          let file = (fileIn fr) 
          giveInfo fr $ "Reading from file " <> file 
          mramProgram :: CompiledProgram <- read <$> readFile file
          return $ mramProgram {traceLen = trLength} 

        -- POST PROCESS
        postProcess :: FlagRecord
                    -> CompilationResult (AnnotatedProgram Metadata Int MWord)
                    -> Maybe Int
                    -> IO (Output Int)
        postProcess fr mramProg privSegsNum = handleErrors $
          postProcess_v
            (verbose fr)
            (modeLeakTainted fr)
            (pubSegMode fr)
            chunkSize
            (not $ verifierMode fr)
            mramProg
            privSegsNum
        outputTheResult :: FlagRecord -> Output AReg -> IO ()
        outputTheResult fr out = do
          let features = (if modeLeakTainted fr then ["leak-tainted"] else [])
                       ++ [] -- Replace list with features
          case fileOut fr of
            Just file -> L.writeFile file $ serialOutput out features
            Nothing   -> putStrLn $ printOutputWithFormat (outFormat fr) out features
        chunkSize = 10

        




-- if verbose
giveInfo :: FlagRecord -> String -> IO ()
giveInfo fr str = when (verbose fr) $ putStrLn $ str

        
handleErrors :: Hopefully x -> IO x
handleErrors hx = case hx of
  Left error -> do putStr $ show error
                   exitWith ExitSuccess -- FAIL
  Right x -> return x

       
data Mode = LeakTaintedMode
  deriving (Eq, Ord)

readMode :: String -> Mode
readMode "leak-tainted" = LeakTaintedMode
readMode m = error $ "Unknown --mode `" <> m <> "`. See --help for valid modes." -- TODO: Better error handling.

data Flag
 = -- General flags
     Verbose
   | Help
   -- Compiler Frontend flags
   | Optimisation Int 
   | LLVMout (Maybe String)
   -- Compiler Backend flags
   | PrivSegs Int
   | JustLLVM        
   | FromLLVM
   | JustMRAM
   | MRAMout (Maybe String)
   | MemSparsity Int
   | AllowUndefFun
   | PrettyPrint
   | PubSegMode String
   | ModeFlag Mode
   | SkipRegisterAllocation
   -- Interpreter flags
   | VerifierMode
   | FromMRAM
   | Output String
   -- About the result
   | DoubleCheck
   | FlatFormat
   | PrettyHex
   deriving(Eq, Ord)

data Stages =
   FullOutput
  | MRAMLang
  | LLVMLang
  | CLang
  deriving (Eq, Ord, Show)


data FlagRecord = FlagRecord
  { verbose :: Bool  
  , fileIn :: String
  , beginning :: Stages
  -- Compiler frontend
  , optim :: Int
  -- Compiler backend
  , privSegs :: Maybe Int
  , trLen :: Maybe Word
  , llvmFile :: String -- Defaults to a temporary one if not wanted.
  , mramFile :: Maybe String
  , sparsNum :: Maybe Int
  , allowUndefFunctions:: Bool
  , ppMRAM :: Bool
  , pubSegMode :: PublicSegmentMode
  , modeLeakTainted :: Bool
  , skipRegAlloc :: Bool
  -- Interpreter
  , verifierMode :: Bool -- VerifierMode
  , fileOut :: Maybe String
  , end :: Stages
  --
  , doubleCheck :: Bool
  , outFormat :: OutFormat
  } deriving (Show)

fr2ClangArgs :: FlagRecord -> ClangArgs
fr2ClangArgs fr = ClangArgs (fileIn fr) (Just $ llvmFile fr) (optim fr) (verbose fr)

defaultFlags :: String -> Maybe Word -> FlagRecord
defaultFlags name len =
  FlagRecord
  { verbose   = False  
  , fileIn    = name
  , beginning = CLang
  -- Compiler frontend
  , optim     = 0
  -- Compiler backend
  , privSegs  = Nothing
  , trLen     = len
  , llvmFile  = "temp/temp.ll"
  , mramFile  = Nothing
  , sparsNum     = Just 2
  , allowUndefFunctions = False
  , ppMRAM = False
  , pubSegMode = PsmFunctionCalls
  , modeLeakTainted = False
  , skipRegAlloc = False
  -- Interpreter
  , verifierMode = False
  , fileOut   = Nothing
  , end       = FullOutput
  --
  , doubleCheck = False
  , outFormat = StdHex
  }
  
parseFlag :: Flag -> FlagRecord -> FlagRecord
parseFlag flag fr =
  case flag of
    Verbose ->                  fr {verbose = True}
    -- Front end flags
    Optimisation n ->           fr {optim = n}
    -- Back end flags 
    LLVMout (Just llvmOut) ->  fr {llvmFile = llvmOut}
    LLVMout Nothing ->         fr {llvmFile = replaceExtension (fileIn fr) ".ll"}
    FromLLVM ->                fr {beginning = min LLVMLang (beginning fr), llvmFile = fileIn fr}
    PrivSegs numSegs ->        fr {privSegs = Just numSegs}
    JustLLVM ->                fr {end = max LLVMLang $ end fr}
    JustMRAM ->                fr {end = max MRAMLang $ end fr}
    MemSparsity s ->           fr {sparsNum = Just s}
    AllowUndefFun ->           fr {allowUndefFunctions = True}
    PrettyPrint ->             fr {ppMRAM = True}
    PubSegMode "none" ->       fr {pubSegMode = PsmNone}
    PubSegMode "function-calls" -> fr {pubSegMode = PsmFunctionCalls}
    PubSegMode "abs-int" ->    fr {pubSegMode = PsmAbsInt}
    PubSegMode x ->            error $ "unsupported public segment mode: " ++ show x
    ModeFlag LeakTaintedMode -> fr {modeLeakTainted = True}
    SkipRegisterAllocation ->   fr {skipRegAlloc = True}
    -- Interpreter flags
    FromMRAM ->                 fr {beginning = MRAMLang} -- In this case we are reading the fileIn
    MRAMout (Just outFile) ->   fr {mramFile = Just outFile}
    MRAMout Nothing ->          fr {mramFile = Just $ replaceExtension (fileIn fr) ".micro"}
    Output outFile ->           fr {fileOut = Just outFile}
    DoubleCheck ->              fr {doubleCheck = True}
    FlatFormat ->               fr {outFormat = max Flat $ outFormat fr}
    PrettyHex ->                fr {outFormat = max PHex $ outFormat fr}
    VerifierMode ->             fr {verifierMode = True}
    _ ->                        fr

parseOptions :: String -> Maybe Word -> [Flag] -> IO FlagRecord
parseOptions filein len flags = do
  name <- return $ filein
  let initFR = defaultFlags name len in
    return $ foldr parseFlag initFR flags
  
options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"]        (NoArg Help)                      "Print this help message"
  , Option []    ["llvm-out"]    (OptArg LLVMout "FILE")           "Save the llvm IR to file"
  , Option []    ["mram-out"]    (OptArg MRAMout "FILE")           "Save the compiled MicroRAM program to file"
  , Option ['O'] ["optimize"]    (OptArg readOpimisation "arg")    "Optimization level of the front end"
  , Option ['o'] ["output"]      (ReqArg Output "FILE")            "Write ouput to file"
  , Option []    ["from-llvm"]   (NoArg FromLLVM)                  "Compile only with the backend. Compiles from an LLVM file."
  , Option []    ["priv-segs"]   (ReqArg (PrivSegs . read) "arg")  "Number of private segments. " 
  , Option []    ["just-llvm"]   (NoArg JustLLVM)                  "Compile only with the frontend. "
  , Option []    ["just-mram"]   (NoArg JustMRAM)                  "Only run the compiler (no interpreter) and output mram. "
  , Option []    ["verifier"]    (NoArg VerifierMode)              "Run in verifier mode  (skips the interpreter). "
  , Option []    ["from-mram","interpreter"]   (NoArg FromMRAM)    "Only run the interpreter from a compiled MicroRAM file."
  , Option ['v'] ["verbose"]     (NoArg Verbose)                   "Chatty compiler"
  , Option []    ["pretty-hex"]  (NoArg PrettyHex)                 "Pretty print the CBOR output. Won't work if writting to file. "
  , Option []    ["flat-hex"]    (NoArg FlatFormat)                "Output in flat CBOR format. Won't work if writting to file. "
  , Option ['c'] ["double-check"](NoArg DoubleCheck)               "check the result"
  , Option ['s'] ["sparsity"]    (ReqArg (\s -> MemSparsity $ read s) "MEM SARSITY")               "check the result"
  , Option []    ["allow-undef"] (NoArg AllowUndefFun)             "Allow declared functions with no body."
  , Option []    ["pretty-print"] (NoArg PrettyPrint)              "Pretty print the MicroRAM program with metadata."
  , Option []    ["pub-seg-mode"] (ReqArg PubSegMode "MODE")       "Public segment generation mode, one of: none, function-calls, abs-int"
  , Option []    ["mode"] (ReqArg (ModeFlag . readMode) "MODE")              "Mode to run the checker in. Valid options include:\n    leak-tainted - Detect an information leak when a tainted value is output."
  , Option []    ["debug-skip-register-allocation"]   (NoArg SkipRegisterAllocation)    "Skip register allocation. Should only be used while debugging."
  ]
  where readOpimisation Nothing = Optimisation 1
        readOpimisation (Just ntxt) = Optimisation (read ntxt)

parseArgs :: [String] -> IO ([Flag], String, Maybe Word)
parseArgs argv = 
  case getOpt Permute options argv of
        (opts,fs:maybeLength,[]) -> do
          -- there can only be one file
          when (Help `elem` opts) $ do
            hPutStrLn stderr (usageInfo header options)
            exitWith ExitSuccess
          myLength <- getLength maybeLength
          return (nub opts, fs, myLength)
        (_,_,errs)      -> do
          hPutStrLn stderr (concat errs ++ usageInfo header options)
          exitWith (ExitFailure 1)

        where header = "Usage: compile file length [arguments] [options]. \n Options: "

              getLength [] = return $ Nothing
              getLength [len] = return $ Just $ read len
              getLength args = do
                hPutStrLn stderr ("The only arguments should be a file name and a trace length. \n" ++
                                  "Arguments provided beyond file name: " ++ show args ++ "\n" ++ usageInfo header options) 
                exitWith (ExitFailure 1)
