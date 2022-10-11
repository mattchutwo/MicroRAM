 {-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RiscV.Backend (riscBackend) where

import Compiler.CompilationUnit
import Compiler.Errors
import Compiler.Metadata
import Compiler.RemoveLabels
import Debug.Trace
import Compiler.IRs
import MicroRAM

import RiscV.Parser
import RiscV.Transpiler
import RiscV.Intrinsics

riscBackend :: Bool   -- ^ verbose
            -> String -- ^ program name
            -> String -- ^ RiscV Code as string
            -> Word    -- ^ The trace length
            -> Hopefully (CompilationResult (AnnotatedProgram Metadata Int MWord))
riscBackend verb progName code trLen = do
  -- Parse the RiscV code
  parsedRisc <- riscvParser progName code
  -- build intrinsics and get their name mapping
  let firstUnusedName = intrinsicsFstUnusedName
  let nameMap = intrinsicsMap
  -- Transpile to MicroAssembly
  masm <- addRiscvPremain <$> transpiler verb firstUnusedName nameMap parsedRisc
  -- add intrinsics
  masmHigh <- justCompile addIntrinsicsHigh masm
  masmLow  <- justCompile addIntrinsicsLow masm
  -- Compile to MicroRAM
  mramHigh <- removeLabels False masmHigh
  mramLow <- removeLabels False masmLow
  return $ mramLow { traceLen = trLen, programCU = MultiProg (programCU mramHigh) (programCU mramLow) }
  
