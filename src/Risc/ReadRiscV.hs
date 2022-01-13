module Risc.ReadRiscV where 


import Data.Bits
import Data.Word
-- import parsec

-- data Instr
--   = BranchInstr !BranchInstr
--   deriving (Show, Eq, Ord)

-- data BranchCond
--   = BEQ
--   | BNE
--   | BLT
--   | BLTU
--   | BGE
--   | BGEU
--   deriving (Show, Eq, Ord)

-- data BranchInstr =
--   Branch !Word12
--          !BranchCond
--          !Register
--          !Register
--   deriving (Show, Eq, Ord)

-- newtype Word12 = Word12 Word16 deriving (Show, Eq, Ord)
-- -- Register 0 is hardwired to the constant 0.
-- data Register
--   = X0
--   | X1
--   | X2
--   | X3
--   | X4
--   | X5
--   | X6
--   | X7
--   | X8
--   | X9
--   | X10
--   | X11
--   | X12
--   | X13
--   | X14
--   | X15
--   | X16
--   | X17
--   | X18
--   | X19
--   | X20
--   | X21
--   | X22
--   | X23
--   | X24
--   | X25
--   | X26
--   | X27
--   | X28
--   | X29
--   | X30
--   | X31
--   deriving (Show, Eq, Ord)
