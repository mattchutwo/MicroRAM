{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeOperators #-}
module Run where

import System.Environment

import Util.Util

import Compiler.IRs (Name)
import MicroRAM
import Compiler.CompilationUnit (CompilationResult, CompilationUnit(..))
import MicroRAM.MRAMInterpreter (execAnswer)

import Data.List

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn $ "No filename provided"
    [file] -> 
      case removeSuffix file of
        Nothing -> putStrLn $
                   "Input file needs to be a .micro file: " ++ (show file)
        Just _name -> do
          compUnit <- compUnitFromFile file
          putStrLn $ "Running program " ++ file ++ " for " ++
            (show $ traceLen compUnit) ++ " steps."
          --mramArgs <- return $ parseArgs progArgs
          result <- return $ execAnswer compUnit
          putStrLn $ "Result: " ++ show result
    _ -> putStrLn "Error: More than one argument provided." 
          

--parseArgs :: [String] -> [Word]
--parseArgs = map read

compUnitFromFile :: FilePath -> IO $ CompilationResult (Program Name MWord)
compUnitFromFile file = do
  contents <- readFile file -- Get text from file
  return $ read contents -- Parse them       
            
microFromFile :: FilePath -> IO (Program Name MWord)
microFromFile file = do
  contents <- readFile file -- Get text from file
  CompUnit prog _ _ _ _ () <- return $ read contents -- Parse them 
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
