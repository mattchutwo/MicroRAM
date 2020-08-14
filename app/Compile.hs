{-# LANGUAGE TypeOperators #-}
module Compile where

import System.Environment
import System.Console.GetOpt
import System.IO
import System.Exit
import qualified Data.ByteString.Lazy                  as L

import Data.Foldable (toList)
import Data.List (intercalate)
import Data.List
import Data.List.Split (splitOn)


import Lib
import LLVMutil.LLVMutil
import Frontend.ClangCaller


import Util.Util
import MicroRAM.MRAMInterpreter
import MicroRAM.MicroRAM
import Compiler.CompilationUnit
import Compiler.Compiler
import Compiler.Errors
import LLVMutil.LLVMIO
import Compiler.IRs

import Output.Output
import Output.CBORFormat


main = do
  (options, file, len, args) <- getArgs >>= parseArgs
  fr <- parseOptions file len options
  ifio (beginning fr <= end fr) $ do
    putStrLn "Nothing to do here. Beginning comes later than end. Did you use -from-mram and -just-llvm? "
    exitWith ExitSuccess
  -- --------------
  -- Run Frontend
  -- --------------
  ifio (beginning fr == CLang) $ do
    output <- callClang (fr2ClangArgs fr)
    giveInfo fr output
  ifio (end fr >= LLVMLang) $ exitWith ExitSuccess
  -- --------------
  -- Run Backend
  -- --------------
  microProg <-  if (beginning fr >= LLVMLang) then -- Compile or read from file
                  do
                    giveInfo fr "Running the compiler backend..."
                    callBackend fr
                else
                  do
                    flatprog <- readFile $ fileIn fr
                    return $ read flatprog
  -- Maybe save the MicroRAM file
  case mramFile fr of 
    Just mramFileOut -> do
      giveInfo fr $ "Write MicroRAM program to file : " ++ mramFileOut
      writeFile mramFileOut $ show microProg
    Nothing -> return ()
  -- Maybe end here and output public output as CBOR
  ifio (end fr >= MRAMLang) $ do
    giveInfo fr $ "Output public info."
    output fr $ compUnit2Output microProg -- return public output 
    exitWith ExitSuccess -- Verifier mod ends here
  -- --------------
  -- Interpreter
  -- --------------
  giveInfo fr $ "Running the interpreter with inputs: " ++ show args
  mem <- return $ buildInitMem args
  secretOut <- return $ fullOutput mem microProg
  output fr $ secretOut
  ifio (doubleCheck fr) $ print "Nothing to check"
  
  where output :: FlagRecord -> Output Name -> IO ()
        output fr out = case fileOut fr of
                           Just file -> L.writeFile file $ serialOutput out
                           Nothing   -> putStrLn $ printOutputWithFormat (outFormat fr) out

        -- if verbose
        giveInfo fr str = ifio (verbose fr) $ putStrLn $ str
            
 
      
callBackend :: FlagRecord -> IO $ CompilationUnit (Program Name Word)
callBackend fr = do  
  -- Retrieve program from file
  llvmModule <- llvmParse $ llvmFile fr
  -- Then compile
  compiledProg <- handleErrorWith (compile (trLen fr) llvmModule)
  return compiledProg

-- | Extracts the prefix: checkName file
splitExtension :: String -> (String,String)
splitExtension file = mapTuple reverse $ splitPrefixExt (reverse file)
  where mapTuple f (a, b) = (f a, f b)

        splitPrefixExt :: String -> (String,String)
        splitPrefixExt file = case splitPrefixExtRec file of
                                (ls,[]) -> ([],ls) -- this case has no extension!
                                other -> other
        

        splitPrefixExtRec :: String -> (String,String)
        splitPrefixExtRec [] = ([],[])
        splitPrefixExtRec (x:xs) =
          if x == '.' then ([],xs) else
            let (ext', file) = splitPrefixExt xs in (x:ext', file)

stripExtension :: String -> String
stripExtension file = let (_,bareFile) = splitExtension file in bareFile

replaceExtension :: String -> String -> String
replaceExtension file ext = (stripExtension file) ++ ext
           
        

removeSuffix :: String -> Maybe String
removeSuffix file =
  if isSuffixOf ".c" file then
    Just $ take ((length file) - 2) file
  else
    Nothing

data Flag
 = -- General flags
     Verbose
   | Help
   -- Compiler Frontend flags
   | Optimisation Int 
   | LLVMout (Maybe String)
   -- Compiler Backend flags
   | JustLLVM        
   | FromLLVM
   | JustMRAM
   | MRAMout (Maybe String)
   -- Interpreter flags
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
  , trLen :: Word
  , beginning :: Stages
  -- Compiler frontend
  , optim :: Int
  -- Compiler backend
  , llvmFile :: String -- Defaults to a temporary one if not wanted.
  , mramFile :: Maybe String
  -- Interpreter
  , fileOut :: Maybe String
  , end :: Stages
  --
  , doubleCheck :: Bool
  , outFormat :: OutFormat
  } deriving (Show)

fr2ClangArgs :: FlagRecord -> ClangArgs
fr2ClangArgs fr = ClangArgs (fileIn fr) (Just $ llvmFile fr) (optim fr) (verbose fr)

defaultFlags :: String -> Word -> FlagRecord
defaultFlags name len=
  FlagRecord
    False
    name
    len
    CLang
    --
    0
    --
    "temp/temp.ll" -- Default we use to temporarily store compilation FIXME!
    Nothing
    --
    Nothing
    FullOutput
    --
    False
    StdHex

parseFlag :: Flag -> FlagRecord -> FlagRecord
parseFlag (Verbose) fr = fr {verbose = True}
-- Front end flags
parseFlag (Optimisation n) fr = fr {optim = n}
-- Back end flags 
parseFlag (LLVMout (Just llvmOut)) fr = fr {llvmFile = llvmOut}
parseFlag (LLVMout Nothing) fr = fr {llvmFile = replaceExtension (fileIn fr) ".ll"}

parseFlag (FromLLVM) fr = fr {beginning = LLVMLang, llvmFile = fileIn fr} -- In this case we are reading the fileIn
parseFlag (JustLLVM) fr = fr {end = max LLVMLang $ end fr}

parseFlag (JustMRAM) fr = fr {end = max MRAMLang $ end fr}
-- Interpreter flags
parseFlag (FromMRAM) fr = fr {beginning = MRAMLang} -- In this case we are reading the fileIn
parseFlag (MRAMout (Just outFile)) fr = fr {mramFile = Just outFile}
parseFlag (MRAMout Nothing) fr = fr {mramFile = Just $ replaceExtension (fileIn fr) ".micro"}

parseFlag (Output outFile) fr = fr {fileOut = Just outFile}

parseFlag (DoubleCheck) fr = fr {doubleCheck = True}

parseFlag FlatFormat fr = fr {outFormat = max Flat $ outFormat fr}
parseFlag PrettyHex fr = fr {outFormat = max PHex $ outFormat fr}

parseFlag _ fr = fr

parseOptions :: String -> Word -> [Flag] -> IO FlagRecord
parseOptions filein len flags = do
  name <- return $ filein
  let initFR = defaultFlags name len in
    return $ foldr parseFlag initFR flags
  
options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"]        (NoArg Help)               "Print this help message"
  , Option []    ["llvm-out"]    (OptArg LLVMout "FILE")           "Save the llvm IR to file"
  , Option []    ["mram-out"]    (OptArg MRAMout "FILE")           "Save the compiled MicroRAM program to file"
  , Option ['O'] ["optimize"]    (OptArg readOpimisation "arg")    "Optimization level of the front end"
  , Option ['o'] ["output"]      (ReqArg Output "FILE")            "Write ouput to file"
  , Option []    ["from-llvm"]   (NoArg FromLLVM)           "Compile only with the backend. Compiles from an LLVM file."
  , Option []    ["just-llvm"]   (NoArg JustLLVM)           "Compile only with the frontend. "
  , Option []    ["just-mram","verifier"]   (NoArg JustMRAM)           "Only run the compiler (no interpreter). "
  , Option []    ["from-mram"]   (NoArg FromMRAM)           "Only run the interpreter from a compiled MicroRAM file."
  , Option ['v'] ["verbose"]     (NoArg Verbose)            "Chatty compiler"
  , Option []    ["pretty-hex"]  (NoArg PrettyHex)               "Pretty print the CBOR output. Won't work if writting to file. "
  , Option []    ["flat-hex"]    (NoArg FlatFormat)               "Output in flat CBOR format. Won't work if writting to file. "
  , Option ['c'] ["double-check"](NoArg DoubleCheck)               "check the result"
  ]
  where readOpimisation Nothing = Optimisation 1
        readOpimisation (Just ntxt) = Optimisation (read ntxt)


parseArgs :: [String] -> IO ([Flag], String, Word, [String])
parseArgs argv = 
  case getOpt Permute options argv of

        (opts,(fs:len:args),[]) -> do
          -- there can only be one file
          ifio (Help `elem` opts) $ do
            hPutStrLn stderr (usageInfo header options)
            exitWith ExitSuccess
          return (nub opts, fs, read len, args)
        (_,args,[]) -> do
          hPutStrLn stderr ("Need input file and trace length. \n Arguments provided: " ++ show args ++ "\n" ++ usageInfo header options) 
          exitWith (ExitFailure 1)
        (_,_,errs)      -> do
          hPutStrLn stderr (concat errs ++ usageInfo header options)
          exitWith (ExitFailure 1)

        where header = "Usage: compile file length [arguments] [options]. \n Options: "

ifio :: Bool -> IO () -> IO ()
ifio cond thing = if cond then thing else return ()
