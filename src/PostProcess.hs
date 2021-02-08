{-
Module      : Choosing Segments
Description : 
Maintainer  : santiago@galois.com
Stability   : experimental

This module combines all the process done after compilation:
-- Segments
-- Interpretation
-- Output formating.


-}

module PostProcess  where

import Compiler.Analysis
import Compiler.CompilationUnit
import Compiler.Errors
import Compiler.Registers

import Control.Monad

import MicroRAM
import MicroRAM.MRAMInterpreter

import Output.Output

import Segments

postProcess_v :: Regs reg => Bool -> Int -> Bool -> CompilationResult (Program reg MWord) -> Hopefully (Output reg)
postProcess_v verb chunkSize private =
  segment
  >=> (doIf private (buildTrace verb chunkSize))
  >=> segProg2Output

  where doIf :: Monad m => Bool -> (a -> m a) -> a -> m a
        doIf cond f = if cond then f else return


buildTrace :: Regs reg => Bool -> Int -> SegmentedProgram reg -> Hopefully (SegmentedProgram reg)
buildTrace verb chunkSize segProg = do
  flatTrace <- return $ run_v verb $ compiled segProg
  return $ chooseSegment' chunkSize flatTrace segProg
          
segProg2Output :: Regs reg => SegmentedProgram reg -> Hopefully (Output reg)
segProg2Output (SegmentedProgram comp segs _segMap segTra) =
  return $ case segTra of
             Nothing -> publ
             Just trace -> mkOutputPrivate (traceOut trace) publ
  where publ = compUnit2Output segs comp
        traceOut trace = outputTrace (numRegs $ params publ) trace 
  
