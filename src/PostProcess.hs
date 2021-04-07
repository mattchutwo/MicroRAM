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

import Compiler.Analysis (sparsityData)
import Compiler.CompilationUnit
import Compiler.Errors
import Compiler.Registers
import Compiler.IRs
import Compiler.Metadata

import Control.Monad

import qualified Data.Map as Map

import MicroRAM
import MicroRAM.MRAMInterpreter

import Output.Output

import Segments
import Segments.ChooseSegments (TraceChunk(..))

import Sparsity.Sparsity (Sparsity)

postProcess_v :: Regs reg
              => Bool
              -> Int
              -> Bool
              -> CompilationResult (AnnotatedProgram Metadata reg MWord)
              -> Maybe Int
              -> Hopefully (Output reg)
postProcess_v verb chunkSize private comp privSegs =
  (segment chunkSize privSegs)
  >=> (doIf private (buildTrace verb chunkSize spar))
  >=> (doIf private recoverAdvice)
  >=> segProg2Output $
  comp
  
  where spar = sparsityData . aData $ comp

        doIf :: Monad m => Bool -> (a -> m a) -> a -> m a
        doIf cond f = if cond then f else return


buildTrace :: Regs reg => Bool -> Int -> Sparsity -> SegmentedProgram reg -> Hopefully (SegmentedProgram reg)
buildTrace verb chunkSize spar segProg = do
  flatTrace <- return $ run_v verb $ compiled segProg
  chooseSegment' chunkSize spar flatTrace segProg

recoverAdvice :: SegmentedProgram reg -> Hopefully (SegmentedProgram reg)
recoverAdvice segProg = do
  adv <- return $ case segTrace segProg of
                      Nothing -> Nothing -- No trace, no advice
                      Just tr -> Just $ adviceSt $ foldOverChunks emptyAdvice tr gatherAdvice
  return $ segProg {segAdvice = adv}
    
  where gatherAdvice :: AdviceState -> ExecutionState reg -> AdviceState
        gatherAdvice (AdviceState adv cyc) exSt =
          if null (advice exSt) then
            (AdviceState adv (cyc + 1))
          else
            AdviceState (Map.insert cyc (advice exSt) adv) (cyc + 1) 

        foldOverChunks :: st -> [TraceChunk reg] -> (st -> ExecutionState reg -> st) -> st
        foldOverChunks st trace f = foldl (foldOverChunkInside f) st trace  
        foldOverChunkInside :: (st -> ExecutionState reg -> st) -> st -> TraceChunk reg -> st
        foldOverChunkInside f st' chunk = foldl f st' (chunkStates chunk)
        
data AdviceState = AdviceState {adviceSt :: Map.Map MWord [Advice], cycleSt :: MWord}
emptyAdvice :: AdviceState
emptyAdvice = AdviceState Map.empty 0
  
segProg2Output :: Regs reg => SegmentedProgram reg -> Hopefully (Output reg)
segProg2Output (SegmentedProgram comp pubSegs privSegs _segMap segTra segAdv) =
  case (segTra, segAdv) of
    (Nothing, Nothing) -> return publ
    (Just trace, Just adv)  -> return $ mkOutputPrivate (traceOut trace) adv publ
    _ -> assumptError $ "Trace and advice BOTH needed to create private output. Found \n Trace: " ++ (show segTra) ++ "\n advice: " ++ (show segAdv) 
  where publ = compUnit2Output (pubSegs ++ privSegs) comp
        traceOut trace = outputTrace (numRegs $ params publ) trace 
  
