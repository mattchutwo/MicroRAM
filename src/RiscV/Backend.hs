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

riscBackend :: Bool   -- ^ verbose
            -> String -- ^ program name
            -> String -- ^ RiscV Code as string
            -> Word    -- ^ The trace length
            -> Hopefully (CompilationResult (AnnotatedProgram Metadata Int MWord))
riscBackend verb progName code trLen = do
  -- Parse the RiscV code
  parsedRisc <- riscvParser progName code
  -- Transpile to MicroAssembly
  masm <- transpiler verb parsedRisc
  -- Compile to MicroRAM
  mram <- removeLabels False masm
  return $ mram { traceLen = trLen, programCU = MultiProg (programCU mram) (programCU mram) }
  
