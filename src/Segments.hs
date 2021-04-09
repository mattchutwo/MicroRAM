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

segment :: (Show reg) => Bool -> Int -> Maybe Int -> CompilationResult (AnnotatedProgram Metadata reg MWord) -> Hopefully (SegmentedProgram reg)
segment producePublic privSize privSegs compRes = do
  let funCount = functionUsage $ aData compRes 
  segs <- segmentProgram funCount $ (pmProg . lowProg . programCU) compRes
  let pubSegs = if producePublic then segs else []
  let privateSegments = if producePublic then
        mkPrivateSegments (traceLen compRes) privSize privSegs
        else
        mkPrivateSegments (traceLen compRes) (fromEnum $ traceLen compRes) privSegs
  return $ SegmentedProgram compResNoMD pubSegs privateSegments Nothing Nothing
  where compResNoMD = compRes {programCU = fmap (fmap (map fst)) (programCU compRes)}

chooseSegment' :: (Show reg, Regs reg) => Int -> Sparsity -> Trace reg -> SegmentedProgram reg -> Hopefully (SegmentedProgram reg) 
chooseSegment' privSize spar trace segProg = do
  let prog = pmProg . lowProg . programCU . compiled $ segProg
  let segs = (V.fromList $ pubSegments segProg)
  chunks <- chooseSegments privSize spar prog trace segs
  let segmentsTrace = maximum (map chunkSeg chunks)
  let numSegments = length (pubSegments segProg) + length (privSegments segProg) 
  -- Check if there are enough segments:
  if (numSegments) >= segmentsTrace then 
    return $ segProg {segTrace = Just chunks}
  else
    assumptError $ "Trace is not long enough. Execution uses: " ++ (show segmentsTrace) ++ " segments, but only " ++ show numSegments ++ " where generated."

mkPrivateSegments :: Word -> Int -> Maybe Int -> [Segment reg MWord]
mkPrivateSegments len size privSegs = replicate howMany (Segment [] [] size [] True True)
  where howMany = case privSegs of
                    Just numSegs -> numSegs
                    Nothing ->  fromEnum len `div` size
   

