module Run where

import Data.Foldable (toList)
import Data.List (intercalate)
import System.Environment

import Lib
import LLVMutil.LLVMutil
import Compiler.Compiler


import Compiler.IRs (Name)
import MicroRAM.MicroRAM
import MicroRAM.MRAMInterpreter (execAnswer)



{-import MicroRAM.MRAMInterpreter
import qualified LLVM.AST as LLVM
import qualified MicroRAM.MicroRAM as MRAM
import MicroRAM.MRAMInterpreter
import LLVM.AST (Named(..))
import qualified LLVM.AST.Constant as LLVM.Constant
import qualified LLVM.AST.IntegerPredicate as IntPred
import GHC.Word as Word
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Short as Short
import qualified Data.String as String
import qualified LLVM.AST.Linkage
import qualified LLVM.AST.Visibility
import qualified LLVM.AST.CallingConvention
-}


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
          putStrLn $ "Running program " ++ file ++ " for " ++ bound ++ " steps."
          mramArgs <- return $ parseArgs progArgs
          result <- return $ execAnswer mramProgram (read bound) mramArgs
          putStrLn $ "Result: " ++ show result
          

parseArgs :: [String] -> [Word]
parseArgs = map read
          
microFromFile :: FilePath -> IO ((Program Name Word)) 
microFromFile file = do
  contents <- readFile file
  return $ read contents

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
