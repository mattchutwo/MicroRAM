{-# LANGUAGE OverloadedStrings #-}
module Compile where

import Data.Foldable (toList)
import Data.List (intercalate)
import System.Environment

import Lib
import Compiler.LLVMutil
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

-- ** Definitinos
  
-- ## Blocks
{- label
  :: LLVM.Name
     -> [Named LLVM.Instruction]
     -> Named LLVM.Terminator
     -> LLVM.BasicBlock
label name insts term =
  LLVM.BasicBlock name insts term

-- # Example 0
{- Compute x^2 + 3x - 10 (For now x = 42)
-}
bb0 :: [LLVM.BasicBlock]
bb0 = [
  label "main0" [
      "a":=  ("a" .+ 42)     -- r0 = 42
      ,"c":= ("a" .* "a")    -- r2 = r0 * r0
      ,"b":= ("a" .* 3)      -- r1 = 3 * r0
      ,"a":= ("b" .+ "c")    -- r0 = r1 + r2
      ,"a":= ("a" .- 10)     -- r0 = r0 - 10
      ]
    (Do $ LLVM.Ret Nothing [])]
main = putStrLn $ show bb0

-}
-- For now we immport a list of basic blocks  
main = do
    args <- getArgs
    case args of 
      [filename] -> do
        blocks <- read <$> readFile filename
        let prog = fromBlocks blocks in
          let compiled = compile prog in 
            putStrLn $ show compiled
      _ -> putStrLn "Wrong number of arguments"


