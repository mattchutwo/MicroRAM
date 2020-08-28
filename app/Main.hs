{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Foldable (toList)
import Data.List (intercalate)
import System.Environment

import Lib
import Compiler.Registers
import Compiler
import MicroRAM
import MicroRAM.MRAMInterpreter

prog1 = [Iadd 0 1 (Const 1), -- x=1
         Iadd 2 0 (Const 0), -- z=x
         Iadd 0 0 (Reg 1),  -- x=x+y
         Iadd 1 2 (Const 0),
       Ijmp (Const 1)]

-- Hardcode 16 registers for now
k = 16
-- No input or advice
input, advice :: [Word]
input = []
advice = []

dumpState :: (Foldable (RMap mreg)) => State mreg -> String
dumpState s = intercalate "; " [
    "pc " ++ show (pc s),
    "regs " ++ intercalate " " (map show $ toList $ regs s),
    "flag " ++ (if flag s then "1" else "0")
    ]


dumpOperand :: Show mreg => Operand mreg Word -> String
dumpOperand (Reg r) = show r ++ " reg"
dumpOperand (Const i) = show i ++ " imm"
-- dumpOperand o = error $ "unsupported operand kind: " ++ show o

dumpInstr :: Show mreg => Instruction mreg Word -> String
dumpInstr i = intercalate " " $ case i of
    Imov dest src -> ["mov", show dest, "0", dumpOperand src]
    Iadd dest src1 src2 -> ["add", show dest, show src1, dumpOperand src2]
    Isub dest src1 src2 -> ["sub", show dest, show src1, dumpOperand src2]
    Imull dest src1 src2 -> ["mull", show dest, show src1, dumpOperand src2]
    i -> error $ "unsupported instr kind: " ++ show i

main :: IO ()
main = undefined
{-do
  args <- getArgs
    case args of
      [file] -> do
        prog <- read <$> readFile file
        mapM_ (putStrLn . dumpInstr) (prog :: [Instruction mreg Word])
      [file, steps] -> do
        prog <- read <$> readFile file
        let s = init_state k [] [] in
            mapM_ (putStrLn . dumpState) $ runFrom prog s (read steps)
      [file, steps, input] -> do
        prog <- read <$> readFile file
        let s = set_reg 0 (read input) $ init_state k [] []
        mapM_ (putStrLn . dumpState) $ runFrom prog s (read steps)
      _ -> putStrLn "Wrong number of arguments"
-}
