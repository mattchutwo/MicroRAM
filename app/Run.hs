{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeOperators #-}
module Run where

import Data.Foldable (toList)
import Data.List (intercalate)
import System.Environment

import Lib

import Util.Util
import LLVMutil.LLVMutil
import Compiler.Compiler

import Compiler.IRs (Name)
import MicroRAM.MicroRAM
import Compiler.CompilationUnit (CompilationUnit(..))
import MicroRAM.MRAMInterpreter (execAnswer, buildInitMem, emptyInitMem)

import Data.List

main = do
  args <- getArgs
  case args of
    [] -> putStrLn $ "No filename provided"
    [file] -> 
      case removeSuffix file of
        Nothing -> putStrLn $
                   "Input file needs to be a .micro file: " ++ (show file)
        Just name -> do
          compUnit <- compUnitFromFile file
          putStrLn $ "Running program " ++ file ++ " for " ++
            (show $ traceLen compUnit) ++ " steps."
          --mramArgs <- return $ parseArgs progArgs
          result <- return $ execAnswer compUnit
          putStrLn $ "Result: " ++ show result
          

--parseArgs :: [String] -> [Word]
--parseArgs = map read

compUnitFromFile :: FilePath -> IO $ CompilationUnit (Program Name MWord)
compUnitFromFile file = do
  contents <- readFile file -- Get text from file
  return $ read contents -- Parse them       
            
microFromFile :: FilePath -> IO (Program Name MWord)
microFromFile file = do
  contents <- readFile file -- Get text from file
  CompUnit prog _ _ _ _ <- return $ read contents -- Parse them 
  return prog

fromText :: String -> Either () (Program Name MWord)
fromText = read

-- | Extracts the prefix: checkName file 
removeSuffix :: String -> Maybe String
removeSuffix file =
  if isSuffixOf suffix file then
    Just $ take ((length file) - length suffix) file
  else
    Nothing
  where suffix = ".micro"
