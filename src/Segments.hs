{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-
Module      : Choosing Segments
Description : 
Maintainer  : santiago@galois.com
Stability   : experimental

-}

module Segments (segment, SegmentedProgram(..), chooseSegment') where

import Compiler.Analysis
import Compiler.CompilationUnit
import Compiler.Errors
import Compiler.IRs
import Compiler.Metadata
import Compiler.Registers

import qualified Data.Map as Map 
import qualified Data.Vector as V (fromList)
import MicroRAM
import MicroRAM.MRAMInterpreter

import Segments.Segmenting
import Segments.ChooseSegments

import Sparsity.Sparsity (Sparsity)
 
data SegmentedProgram reg = SegmentedProgram  { compiled :: CompilationResult (Program reg MWord) -- No METADATA
                                              , pubSegments :: [Segment reg MWord]
                                              , privSegments :: [Segment reg MWord]
                                              , segTrace :: Maybe [TraceChunk reg]
                                              , segAdvice :: Maybe (Map.Map MWord [Advice])}

segment :: Int -> CompilationResult (AnnotatedProgram Metadata reg MWord) -> Hopefully (SegmentedProgram reg)
segment privSize compRes = do
  let funCount = functionUsage $ aData compRes 
  segs <- segmentProgram funCount $ (lowProg . programCU) compRes
  privateSegments <- return $ mkPrivateSegments (traceLen compRes) privSize -- Should we substract the public segments?  
  return $ SegmentedProgram compResNoMD segs privateSegments Nothing Nothing
  where compResNoMD = compRes {programCU = (map fst) <$> programCU compRes}

chooseSegment' :: (Show reg, Regs reg) => Int -> Sparsity -> Trace reg -> SegmentedProgram reg -> Hopefully (SegmentedProgram reg) 
chooseSegment' privSize spar trace segProg = do
  let prog = lowProg . programCU . compiled $ segProg
  let segs = (V.fromList $ pubSegments segProg)
  chunks <- chooseSegments privSize spar prog trace segs
  let segmentsTrace = maximum (map chunkSeg chunks)
  let numSegments = length (pubSegments segProg) + length (privSegments segProg) 
  -- Check if there are enough segments:
  if (numSegments) >= segmentsTrace then 
    return $ segProg {segTrace = Just chunks}
  else
    assumptError $ "Trace is not long enough. Execution uses: " ++ (show segmentsTrace) ++ " segments, but only " ++ show numSegments ++ " where generated."
     
mkPrivateSegments :: Word -> Int -> [Segment reg MWord]
mkPrivateSegments len size = replicate (fromEnum len `div` size) (Segment [] [] size [] True True) 

