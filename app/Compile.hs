{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compile where

import qualified Data.ByteString.Lazy                  as L

import Frontend.ClangCaller
import Util.Util

import MicroRAM
import Compiler hiding (Domain)
import qualified Compiler
import Control.Monad(when)
import Compiler.CompilationUnit
import Compiler.Errors
import Compiler.IRs
import Compiler.Metadata
import Data.Either (partitionEithers)
import RiscV.Backend
import Debug.PrettyPrint
import Prelude hiding (read)
import Text.Read (readMaybe)

#if ! NO_LLVM
import LLVMutil.LLVMIO
#endif

import Output.Output
import Output.CBORFormat

import PostProcess
import Segments (PublicSegmentMode(..))

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath hiding ((<.>))
import System.IO
import System.Exit

type CompiledProgram = CompilationResult (AnnotatedProgram Metadata AReg MWord)

main :: IO ()
main = do
  fr <- getArgs >>= parseArgs
  when (beginning fr <= end fr) $ do
    putStrLn "Nothing to do here. Beginning comes later than end. Did you use -from-mram and -just-llvm? "
    exitSuccess
  -- --------------
  -- Run Frontend
  -- --------------
  when (beginning fr == CLang && null (domains fr)) $ frontend fr -- writes the resutl to llvmFile
  when (end fr >= LLVMLang) $ exitWith ExitSuccess
  -- --------------
  -- Run Backend
  -- --------------
  microProg::CompiledProgram <- case trLen fr of
                                  Nothing -> do
                                    putStrLn $ "Found no trace, can't compile."
                                    exitWith ExitSuccess
                                  Just trLength -> if (beginning fr >= RiscV) then -- Compile or read from file
                                                     if not $ null $ domains fr then
                                                       domainCompiler trLength fr
                                                     else if beginning fr == RiscV then
                                                       riscCompiler trLength fr
                                                     else
                                                       llvmBackend trLength fr
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
        llvmBackend :: Word -> FlagRecord -> IO $ CompiledProgram
        llvmBackend trLength fr = do
          giveInfo fr "Running the llvm compiler backend..."
          -- Retrieve program from file
#if NO_LLVM         
          llvmModule <- ioError $ userError "LLVM not available. Rebuild package without the `no-llvm` flag." 
#else
          llvmModule <- llvmParse $ fileIn fr
#endif 
          -- compiler options
          let options = CompilerOptions
                { verb = verbose fr
                , allowUndefFun = allowUndefFunctions fr
                , spars = sparsNum fr
                , tainted = modeLeakTainted fr
                , skipRegisterAllocation = skipRegAlloc fr
                , numberRegs = Nothing
                , riscvEmulatorEnabled = False
                } in
          -- Then compile
            handleErrorWith (compile options trLength llvmModule)

        -- | Alternative compiler backend going through RiscV 
        riscCompiler :: Word -> FlagRecord -> IO $ CompiledProgram
        riscCompiler trLength fr = do
          giveInfo fr "Running the RiscV compiler backend..."
          riscCode <- readFile $ fileIn fr
          handleErrorWith (riscBackend (verbose fr)
                             (nativeEmulator fr) (fileIn fr) riscCode trLength)

        -- | Multi-domain / multi-input compiler
        domainCompiler :: Word -> FlagRecord -> IO $ CompiledProgram
        domainCompiler trLength fr = do
          giveInfo fr "Running the multi-input compiler backend..."
          ds <- mapM (loadCode (error "llvm input is disabled") readFile) (domains fr)
          let options = CompilerOptions
                { verb = verbose fr
                , allowUndefFun = allowUndefFunctions fr
                , spars = sparsNum fr
                , tainted = modeLeakTainted fr
                , skipRegisterAllocation = skipRegAlloc fr
                , numberRegs = numberRegsFr fr
                , riscvEmulatorEnabled = nativeEmulator fr
                }
          handleErrorWith (compileDomains options trLength ds)

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
          mramProgram :: Maybe CompiledProgram <- readMaybe <$> readFile file
          case mramProgram of
            Nothing ->
              error $ "Malformed MRAM file: " <> file
            Just mramProgram' ->
              return $ mramProgram' {traceLen = trLength}

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

readMode :: String -> Either String Mode
readMode "leak-tainted" = pure LeakTaintedMode
readMode m = Left $ "Unknown --mode `" <> m <> "`. See --help for valid modes."

read :: Read a => String -> String -> Either String a
read msg s = case readMaybe s of
  Nothing -> Left $ "Invalid " <> msg <> ": " <> s <> "\n"
  Just x -> Right x

data Flag
 = -- General flags
     Verbose
   | Help
   -- Compiler Frontend flags
   | Optimisation Int 
   | LLVMout (Maybe String)
   -- Multi-domain input flags
   | Domain String
   | DomainSecret Word Word
   | DomainPrivileged
   | DomainInputRiscv FilePath
   | DomainInputLLVM FilePath
   -- Compiler Backend flags
   | PrivSegs Int
   | JustLLVM        
   | FromLLVM        
   | FromRiscV
   | JustMRAM
   | MRAMout (Maybe String)
   | MemSparsity Int
   | AllowUndefFun
   | PrettyPrint
   | PubSegMode String
   | ModeFlag Mode
   | NativeEmulator
   | NumberRegs Word
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
  | RiscV
  | LLVMLang
  | CLang
  deriving (Eq, Ord, Show)


data FlagRecord = FlagRecord
  { verbose :: Bool  
  , fileIn :: String
  , beginning :: Stages
  , domains :: [Compiler.Domain]
  , curDomain :: Compiler.Domain
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
  , nativeEmulator :: Bool
  , numberRegsFr :: Maybe Word
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

defaultFlags :: FlagRecord
defaultFlags =
  FlagRecord
  { verbose   = False  
  , fileIn    = ""
  , beginning = CLang
  , domains   = []
  , curDomain = defaultDomain
  -- Compiler frontend
  , optim     = 0
  -- Compiler backend
  , privSegs  = Nothing
  , trLen     = Nothing
  , llvmFile  = "temp/temp.ll"
  , mramFile  = Nothing
  , sparsNum     = Just 2
  , allowUndefFunctions = False
  , ppMRAM = False
  , pubSegMode = PsmFunctionCalls
  , modeLeakTainted = False
  , nativeEmulator = False
  , numberRegsFr = Nothing
  , skipRegAlloc = False
  -- Interpreter
  , verifierMode = False
  , fileOut   = Nothing
  , end       = FullOutput
  --
  , doubleCheck = False
  , outFormat = StdHex
  }

defaultDomain :: Compiler.Domain
defaultDomain = Compiler.Domain
  { secretLengths = Nothing
  , privileged = False
  , domainInputs = []
  }

commitCurDomain :: FlagRecord -> FlagRecord
commitCurDomain fr
  | not $ null $ domainInputs $ curDomain fr =
    fr { domains = domains fr ++ [curDomain fr], curDomain = defaultDomain }
  | otherwise =
    fr { curDomain = defaultDomain }

withCurDomain :: (Compiler.Domain -> Compiler.Domain) -> FlagRecord -> FlagRecord
withCurDomain f fr = fr { curDomain = f (curDomain fr) }

addDomainInput :: DomainInput -> Compiler.Domain -> Compiler.Domain
addDomainInput i d = d { domainInputs = domainInputs d ++ [i] }

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
    FromRiscV ->               fr {beginning = RiscV}
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
    NativeEmulator ->           fr {nativeEmulator = True}
    NumberRegs n ->             fr {numberRegsFr = Just n}
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
    -- Multi-domain flags
    Domain _name ->             commitCurDomain fr
    DomainSecret codeSize memSize ->
      withCurDomain (\d -> d { secretLengths = Just (codeSize, memSize) }) fr
    DomainPrivileged ->         withCurDomain (\d -> d { privileged = True }) fr
    DomainInputRiscv path ->    withCurDomain (addDomainInput (InputRISCV path Nothing)) fr
    DomainInputLLVM path ->     withCurDomain (addDomainInput (InputLLVM path Nothing)) fr
    _ ->                        fr

options :: [OptDescr (Either String Flag)]
options =
  [ Option ['h'] ["help"]        (NoArg $ pure Help)                      "Print this help message"
  , Option []    ["llvm-out"]    (OptArg (pure . LLVMout) "FILE")         "Save the llvm IR to file"
  , Option []    ["mram-out"]    (OptArg (pure . MRAMout) "FILE")         "Save the compiled MicroRAM program to file"
  , Option ['O'] ["optimize"]    (OptArg readOpimisation "arg")    "Optimization level of the front end"
  , Option ['o'] ["output"]      (ReqArg (pure . Output) "FILE")          "Write ouput to file"
  , Option []    ["from-llvm"]   (NoArg $ pure FromLLVM)                  "Compile only with the backend. Compiles from an LLVM file."
  , Option ['r'] ["riscv"]       (NoArg $ pure FromRiscV)                 "Compile from RiscV assembly."
  , Option []    ["priv-segs"]   (ReqArg (PrivSegs <.> read "priv-segs") "arg")  "Number of private segments. "
  , Option []    ["just-llvm"]   (NoArg $ pure JustLLVM)                  "Compile only with the frontend. "
  , Option []    ["just-mram"]   (NoArg $ pure JustMRAM)                  "Only run the compiler (no interpreter) and output mram. "
  , Option []    ["verifier"]    (NoArg $ pure VerifierMode)              "Run in verifier mode  (skips the interpreter). "
  , Option []    ["from-mram","interpreter"]   (NoArg $ pure FromMRAM)    "Only run the interpreter from a compiled MicroRAM file."
  , Option ['v'] ["verbose"]     (NoArg $ pure Verbose)                   "Chatty compiler"
  , Option []    ["pretty-hex"]  (NoArg $ pure PrettyHex)                 "Pretty print the CBOR output. Won't work if writting to file. "
  , Option []    ["flat-hex"]    (NoArg $ pure FlatFormat)                "Output in flat CBOR format. Won't work if writting to file. "
  , Option ['c'] ["double-check"](NoArg $ pure DoubleCheck)               "check the result"
  , Option ['s'] ["sparsity"]    (ReqArg (MemSparsity <.> read "sparsity") "MEM SARSITY")  "check the result"
  , Option []    ["allow-undef"] (NoArg $ pure AllowUndefFun)             "Allow declared functions with no body."
  , Option []    ["pretty-print"] (NoArg $ pure PrettyPrint)              "Pretty print the MicroRAM program with metadata."
  , Option []    ["pub-seg-mode"] (ReqArg (pure . PubSegMode) "MODE")     "Public segment generation mode, one of: none, function-calls, abs-int"
  , Option []    ["mode"] (ReqArg (ModeFlag <.> readMode) "MODE")         "Mode to run the checker in. Valid options include:\n    leak-tainted - Detect an information leak when a tainted value is output."
  , Option []    ["debug-emulator"] (NoArg $ pure NativeEmulator)         "Emulate native instructions to check for consistency between native and MRAM machines. Should only be used while debugging."
  , Option ['r'] ["regs"]         (ReqArg (NumberRegs <.> read "regs") "n")  "Define the number of registers."
  , Option []    ["debug-skip-register-allocation"]   (NoArg $ pure SkipRegisterAllocation)  "Skip register allocation. Should only be used while debugging."

  , Option []    ["domain"]      (ReqArg (pure . Domain) "NAME")          "define a new domain called NAME"
  , Option []    ["domain-secret"] (ReqArg readDomainSecret "CODESIZE,MEMSIZE") "set the secret flag, max code size, and max initial memory size for the current domain"
  , Option []    ["domain-privileged"] (NoArg $ pure DomainPrivileged)    "set the privileged flag for the current domain"
  , Option []    ["domain-input-riscv"] (ReqArg (pure . DomainInputRiscv) "PATH") "add a RISC-V assembly file to the current domain"
  , Option []    ["domain-input-llvm"] (ReqArg (pure . DomainInputLLVM) "PATH") "add an LLVM IR file to the current domain"
  ]
  where readOpimisation Nothing = pure $ Optimisation 1
        readOpimisation (Just ntxt) = Optimisation <$> read "optimize" ntxt

        readDomainSecret s = DomainSecret <$> read "domain-secret" a <*> read "domain-secret" b
          where (a, ',':b) = break (== ',') s

parseArgs :: [String] -> IO FlagRecord
parseArgs argv = do
  case getOpt Permute options argv of
        (opts', posArgs, []) -> case partitionEithers opts' of
          ([], opts) -> do
            let fr = commitCurDomain $ foldr parseFlag defaultFlags opts
            addPosArgs fr posArgs
          (errs, _) ->
            usageError errs
        (_,_,errs)      -> usageError errs


  where header = "Usage: compile file length [arguments] [options]. \n Options: "
        usageError errs = do
          hPutStrLn stderr (concat errs ++ usageInfo header options)
          exitWith (ExitFailure 1)

        addPosArgs fr posArgs
          | not $ null $ domains fr = case posArgs of
            [] -> return fr
            [len] -> case read "length" len of
              Left err -> usageError [err]
              Right len' -> return (fr { trLen = Just len' })
            _ -> usageError ["too many positional arguments"]
          | otherwise = case posArgs of
            [file] -> return (fr { fileIn = file })
            [file, len] -> case read "length" len of
              Left err -> usageError [err]
              Right len' ->
                return (fr { fileIn = file, trLen = Just len' })
            [] -> usageError ["file name is required"]
            _ -> usageError ["too many positional arguments"]
