module Compile where

import Data.Foldable (toList)
import Data.List (intercalate)
import System.Environment

import Lib
import LLVMutil.LLVMutil
import Compiler.Compiler


import Compiler.Compiler
{-
import MicroRAM.MicroRAM
import MicroRAM.MRAMInterpreter
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

import LLVMutil.LLVMIO

import Frontend.ClangCaller

import Data.List

main = do
  args <- getArgs
  case args of
    [] -> putStrLn $ "No filename provided"
    [file] -> case removeSuffix file of
                Nothing -> putStrLn $
                           "Input file needs to be a .c file: " ++ (show file)
                Just name -> do 
                  mramProg <- fullcompile file "temp/temp.ll" 
                  case mramProg of
                    Left error -> putStrLn $ "Compiling error: " ++ show error
                    Right prog -> writeFile (name ++ ".micro") $ show prog
                
-- | Extracts the prefix: checkName file 
removeSuffix :: String -> Maybe String
removeSuffix file =
  if isSuffixOf ".c" file then
    Just $ take ((length file) - 2) file
  else
    Nothing

fullcompile name llvmFile =
  let cargs = ClangArgs name (Just llvmFile) 0 in
    do
      callClang cargs
      llvmModule <- llvmParse llvmFile
      return $ compile llvmModule


-- For now we immport a list of basic blocks  
{-main = do
    args <- getArgs
    case args of
      [flag,filename] ->
        case flag of
          -- We add this case TOMPORARILY to only compile straight code (no branching, no labels no functions)
         {- "basicblock" -> do
            blocks <- read <$> readFile filename
            let compiled = compileStraight blocks in 
              putStrLn $ show compiled -}
          _ -> putStrLn $ "Wrong flag " ++ (show flag)
      [filename] -> do
        prog <- read <$> readFile filename
        let compiled = compile prog in 
          putStrLn $ show compiled
      _ -> putStrLn $ "Wrong number of arguments: " ++ (show (length args))
-}

