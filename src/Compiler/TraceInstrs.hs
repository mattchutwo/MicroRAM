module Compiler.TraceInstrs
  ( maybeTrace,
    maybeTraceIR,
    regName,
    optRegName,
  ) where

import qualified Data.ByteString.Short as Short
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Text as Text

import Compiler.LazyConstants
import Compiler.Common
import Compiler.IRs

import MicroRAM (MWord) 
import qualified MicroRAM as MRAM

enableTrace :: Bool
enableTrace = False

maybeTrace :: String -> [op] -> [MRAM.Instruction' a b op]
maybeTrace _ _ | not enableTrace = []
maybeTrace name ops = [MRAM.Iext (Text.pack $ "trace_" ++ name) ops]

maybeTraceIR :: String -> [MAOperand VReg w] -> [MIRInstr () w]
maybeTraceIR name ops = map (\i -> MirM i ()) $ maybeTrace name ops

regName :: VReg -> MAOperand VReg MWord
regName (Name n) = LImm $ SConst $ read $ BSU.toString $ Short.fromShort n
regName (NewName w) = LImm $ SConst $ fromIntegral w

optRegName :: Maybe VReg -> MAOperand VReg MWord
optRegName (Just r) = regName r
optRegName Nothing = LImm $ SConst $ 111111
