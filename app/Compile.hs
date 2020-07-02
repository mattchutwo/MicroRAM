module Compile where

import Data.Foldable (toList)
import Data.List (intercalate)
import System.Environment

import System.Console.GetOpt
import System.IO
import System.Exit
import Data.List
import Data.List.Split (splitOn)


import Lib
import LLVMutil.LLVMutil
import Compiler.Compiler


import Compiler.Compiler
import LLVMutil.LLVMIO
import Frontend.ClangCaller


main = do
  (options, file) <- getArgs >>= parseFlags
  fr <- parseOptions file options
  ifio (not $ fromLLVM fr) $ do
    output <- callClang (fr2ClangArgs fr)
    ifio (verbose fr) $ putStrLn $ output
  ifio (not $ justLLVM fr) $ callBackend fr

callBackend :: FlagRecord -> IO ()
callBackend fr = do  
  -- Retrieve program from file
  llvmModule <- llvmParse $ llvmFile fr
  -- Then compile
  case compile $ llvmModule of
    Left error -> do
      hPutStrLn stderr ("Backend compilation error: " ++ show error)
      exitWith (ExitFailure 1)
    Right prog -> writeFile (fileOut fr) $ show prog                
                
  
-- | Extracts the prefix: checkName file 
removeSuffix :: String -> Maybe String
removeSuffix file =
  if isSuffixOf ".c" file then
    Just $ take ((length file) - 2) file
  else
    Nothing


data Flag
 = LLVMout String
   | Optimisation Int
   | Output String
   | FromLLVM
   | JustLLVM
   | Verbose
   | Help
   deriving(Eq, Ord)

data FlagRecord = FlagRecord
  { fileIn :: String
  , optim :: Int
  , fileOut :: String
  , llvmFile :: String -- Defaults to a temporary one if not wanted.
  , fromLLVM :: Bool
  , justLLVM :: Bool
  , verbose :: Bool
  }

fr2ClangArgs :: FlagRecord -> ClangArgs
fr2ClangArgs fr = ClangArgs (fileIn fr) (Just $ llvmFile fr) (optim fr) (verbose fr)

defaultFlags :: String -> String -> String -> FlagRecord
defaultFlags name suffixIn suffixOut =
  FlagRecord
    (name++suffixIn)
    0
    (name ++ suffixOut)
    "temp/temp.ll"
    False
    False
    False

parseFlag :: Flag -> FlagRecord -> FlagRecord
parseFlag (LLVMout llvmOut) fr = fr {llvmFile = llvmOut} 
parseFlag (Optimisation n) fr = fr {optim = n}
parseFlag (Output outFile) fr = fr {fileOut = outFile}
parseFlag (FromLLVM) fr = fr {fromLLVM = True}
parseFlag (JustLLVM) fr = fr {justLLVM = True}
parseFlag (Verbose) fr = fr {verbose = True}
parseFlag _ fr = fr

parseOptions :: String -> [Flag] -> IO FlagRecord
parseOptions filein flags = do
  name <- stripExtension filein
  let initFR = defaultFlags name suffixIn suffixOut in
    return $ foldr parseFlag initFR flags 
  where suffixIn = if FromLLVM `elem` flags then ".ll" else ".c"
        suffixOut = if JustLLVM `elem` flags then ".ll" else ".micro"

        stripExtension fileIn = do
          case splitOn "." fileIn of
            [name] -> return name
            [name, extension] -> return name
            _ -> ioError (userError $ "File name can't be parsed: " ++ fileIn)



options :: [OptDescr Flag]
options =
  [ Option []    ["llvm-out"]    (ReqArg LLVMout "FILE")           "Save the llvm IR to a file"
  , Option ['O'] ["optimize"]    (OptArg readOpimisation "arg")   "Optimization level of the front end"
  , Option ['o'] ["output"]      (ReqArg Output "FILE")           "Output to file"
  , Option []    ["from-llvm"]   (NoArg FromLLVM)           "Compile only with the backend. Compiles from an LLVM file."
  , Option []    ["just-llvm"]   (NoArg JustLLVM)           "Compile only with the frontend. "
  , Option ['v'] ["verbose"]     (NoArg Verbose)            "Chatty compiler"
  , Option ['h'] ["help"]        (NoArg Help)               "Print this help message"
  ]
  where readOpimisation Nothing = Optimisation 1
        readOpimisation (Just ntxt) = Optimisation (read ntxt)


parseFlags :: [String] -> IO ([Flag], String)
parseFlags argv = 
  case getOpt Permute options argv of

        (args,fs,[]) -> do
          -- there can only be one file
          ifio (length fs /= 1) $ hPutStrLn stderr ("Need exactly one input file. \n Files provided:" ++ show fs ++ "\n" ++ usageInfo header options)
          ifio (Help `elem` args) $ do
            hPutStrLn stderr (usageInfo header options)
            exitWith ExitSuccess
          return (nub args, fs !! 0) -- We checked and there is only one

        (_,_,errs)      -> do
            hPutStrLn stderr (concat errs ++ usageInfo header options)
            exitWith (ExitFailure 1)

        where header = "Usage: compile file [options]"


ifio cond thing = if cond then thing else return ()
