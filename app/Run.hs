module Run where

import Data.Foldable (toList)
import Data.List (intercalate)
import System.Environment

import Lib
import LLVMutil.LLVMutil
import Compiler.Compiler

import Compiler.IRs (Name)
import MicroRAM.MicroRAM
import Compiler.CompilationUnit (CompilationUnit(..))
import MicroRAM.MRAMInterpreter (execAnswer, initMem, emptyInitMem)

import Data.List

main = do
  args <- getArgs
  case args of
    [] -> putStrLn $ "No filename provided"
    [file] -> do
      prog <- microFromFile file
      putStrLn $ show prog
    (file:bound:progArgs) ->
      case removeSuffix file of
        Nothing -> putStrLn $
                   "Input file needs to be a .micro file: " ++ (show file)
        Just name -> do
          mramProgram <- microFromFile file
          putStrLn $ "With arguments " ++ show progArgs
          putStrLn $ "Running program " ++ file ++ " for " ++ bound ++ " steps."
          --mramArgs <- return $ parseArgs progArgs
          result <- return $ execAnswer mramProgram (read bound) (initMem progArgs)
          putStrLn $ "Result: " ++ show result
          

--parseArgs :: [String] -> [Word]
--parseArgs = map read
          
microFromFile :: FilePath -> IO ((Program Name Word)) 
microFromFile file = do
  contents <- readFile file -- Get text from file
  CompUnit prog _ _ <- return $ read contents -- Parse them 
  return prog

fromText :: String -> Either () (Program Name Word)
fromText = read

-- | Extracts the prefix: checkName file 
removeSuffix :: String -> Maybe String
removeSuffix file =
  if isSuffixOf suffix file then
    Just $ take ((length file) - length suffix) file
  else
    Nothing
  where suffix = ".micro"
