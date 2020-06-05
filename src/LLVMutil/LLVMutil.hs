{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module LLVMutil.LLVMutil where

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
--import Data.Bits.Extras
--import Data.Sequence as Seq

--import qualified Test.QuickCheck as QC





-- Setup an interpreter to run the output
{- We assume the output to be in the first register
   We fix 16 registers
   There is no termination instruction... so we run forever
   The interpreter must take a input for "fuel"
-}

-- Build arbitrary words 
w32 :: Integral a => a -> Word.Word32
w32 = fromIntegral

wz = w32 (0::Int)

-- types
ty = LLVM.VoidType



-- Operand class to make notation easier
instance String.IsString LLVM.Operand where
  fromString  a = LLVM.LocalReference ty (LLVM.Name (Short.toShort (C8.pack a)))

int2op n = LLVM.ConstantOperand (LLVM.Constant.Int wz n)

instance Num LLVM.Operand where
  a + b = int2op 0 --Yes this is all boggus...
  a - b = int2op 0
  a * b = int2op 0
  negate b = int2op 0
  abs b = int2op 0
  signum b = int2op 0
  fromInteger n = int2op n
  {-
class Operandy a where
  toOperand :: a -> LLVM.Operand

--   Constants
instance Operandy Integer where
  toOperand a = LLVM.ConstantOperand (LLVM.Constant.Int wz a)

--   Regs
instance Operandy String where
  toOperand a = LLVM.LocalReference ty (LLVM.Name (Short.toShort (C8.pack a)))
-}



-- LLVM instructions


-- ## Arith
(.+),(.-),(.*),(./) :: LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
(.+) o1 o2 = LLVM.Add False False o1 o2 []
(.-) o1 o2 = LLVM.Sub False False o1 o2 []
(.*) o1 o2 = LLVM.Mul False False o1 o2 []
(./) o1 o2 = LLVM.SDiv False o1 o2 []

-- ## Logic
(.&), (.|), xor:: LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
(.&) o1 o2 = LLVM.And o1 o2 []
(.|) o1 o2 = LLVM.Or o1 o2 []
xor o1 o2 = LLVM.Xor o1 o2 []


-- ## Comparisons
(.==),(.>),(.><),(.>=),(.<),(.<=)::
  LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
(.==) o1 o2 = LLVM.ICmp IntPred.EQ o1 o2 []
(.><) o1 o2 = LLVM.ICmp IntPred.NE o1 o2 []
(.>) o1 o2 = LLVM.ICmp IntPred.SGT o1 o2 []
(.>=) o1 o2 = LLVM.ICmp IntPred.SGE o1 o2 []
(.<) o1 o2 = LLVM.ICmp IntPred.SLT o1 o2 []
(.<=) o1 o2 = LLVM.ICmp IntPred.SLE o1 o2 []



-- ## Memory
alloc:: LLVM.Instruction
alloc = (LLVM.Alloca ty Nothing wz [])

allocN:: LLVM.Operand -> LLVM.Instruction
allocN n = (LLVM.Alloca ty (Just n) wz [])

load:: LLVM.Operand -> LLVM.Instruction
load o1 = (LLVM.Load False o1 Nothing wz [])


-- ** Modules
str2short :: String -> Short.ShortByteString
str2short = Short.toShort . C8.pack

trivialShort :: Short.ShortByteString
trivialShort = str2short ""

defaultModule :: [LLVM.Definition] -> LLVM.Module
defaultModule defs = 
  LLVM.Module trivialShort trivialShort Nothing Nothing defs

fromGlobals :: [LLVM.Global] -> LLVM.Module
fromGlobals globs = defaultModule $ map LLVM.GlobalDefinition globs

-- ** Functions

defaultFunction :: LLVM.Name -> [LLVM.BasicBlock] -> LLVM.Global
defaultFunction name blocks = LLVM.Function
  LLVM.AST.Linkage.Private
  LLVM.AST.Visibility.Default
  Nothing                      -- dll Storage Class
  LLVM.AST.CallingConvention.C -- Callikng convention
  []                           -- Return attributes TODO
  LLVM.VoidType                -- Return Type
  name
  ([], False)                  -- Parameters
  []                           -- function attributes
  Nothing   -- section
  Nothing   -- comdat
  8         -- alignment
  Nothing   -- Garbage collect.
  Nothing   -- prefix 
  blocks
  Nothing   -- personality Function
  []        -- metadata

-- ** Most trivial examples only have some basic blocks

fromBlocks :: [LLVM.BasicBlock] -> LLVM.Module
fromBlocks blocks = fromGlobals [defaultFunction "main" blocks]


-- ** From simple functions
