{-# LANGUAGE OverloadedStrings #-}
module CompileBlock where

import Data.Foldable (toList)
import Data.List (intercalate)
import System.Environment

import Lib
import LLVMutil.LLVMutil
import Compiler.Compiler
import MicroRAM.MicroRAM
import MicroRAM.MRAMInterpreter


import Compiler.Compiler
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


-- | dropInitReturn : removes the first three and last instructions.
-- The initializations and return instructions are not needed for a single block
dropInitReturn = tail . tail . tail . init

-- For now we immport a list of basic blocks  
main = do
    args <- getArgs
    case args of 
      [flag,filename] ->
        case flag of
          "basicblock" -> do
            prog <- read <$> readFile filename
            let compiled = compile prog in 
              putStrLn $ show compiled
          _ -> putStrLn $ "Wrong flag " ++ (show flag)
      [filename] -> do
        blocks <- read <$> readFile filename
        let prog = fromBlocks blocks in
          let compiled = compile prog in 
            putStrLn $ show (dropInitReturn <$> compiled)
      _ -> putStrLn $ "Wrong number of arguments: " ++ (show (length args))
