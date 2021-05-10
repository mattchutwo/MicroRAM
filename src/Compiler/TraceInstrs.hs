module Compiler.TraceInstrs
  ( maybeTrace,
    maybeTraceIR,
    regName,
    optRegName,
  ) where

--import qualified Data.ByteString.Short as Short
--import qualified Data.ByteString.UTF8 as BSU
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
maybeTrace name ops = [MRAM.Iext (MRAM.XTrace (Text.pack $ name) ops)]

maybeTraceIR :: md -> String -> [MAOperand VReg w] -> [MIRInstr md w]
maybeTraceIR md name ops = map (\i -> MirM i md) $ maybeTrace name ops

regName :: VReg -> MAOperand VReg MWord
regName (Name w _) = LImm $ SConst $ fromIntegral w  -- ^ Debug name is lost. Text names were never supported.

optRegName :: Maybe VReg -> MAOperand VReg MWord
optRegName (Just r) = regName r
optRegName Nothing = LImm $ SConst $ 111111
