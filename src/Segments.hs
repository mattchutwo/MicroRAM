{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-
Module      : Choosing Segments
Description : 
Maintainer  : santiago@galois.com
Stability   : experimental

-}

module Segments (segment, SegmentedProgram(..), chooseSegment') where

import Compiler.CompilationUnit
import Compiler.Errors
import Compiler.Registers

import qualified Data.Map as Map 

import MicroRAM
import MicroRAM.MRAMInterpreter

import Segments.Segmenting
import Segments.ChooseSegments


 
data SegmentedProgram reg = SegmentedProgram  { compiled :: CompilationResult (Program reg MWord)
                                              , pubSegments :: [Segment reg MWord]
                                              , privSegments :: [Segment reg MWord]
                                              , segMap  :: Map.Map MWord [Int]
                                              , segTrace :: Maybe [TraceChunk reg]
                                              , segAdvice :: Maybe (Map.Map MWord [Advice])}

segment :: Int -> CompilationResult (Program reg MWord) -> Hopefully (SegmentedProgram reg)
segment privSize compRes = do 
  (segs, segMap') <- segmentProgram $ (lowProg . programCU) compRes
  privateSegments <- return $ mkPrivateSegments (traceLen compRes) privSize -- Should we substract the public segments?  
  return $ SegmentedProgram compRes segs privateSegments segMap' Nothing Nothing

chooseSegment' :: Regs reg => Int -> Trace reg -> SegmentedProgram reg -> Hopefully (SegmentedProgram reg) 
chooseSegment' privSize trace segProg =
  -- Check if there are enough segments:
  if (numSegments) >= segmentsTrace then 
    return $ segProg {segTrace = Just chunks}
  else
    assumptError $ "Trace is not long enough. Execution uses: " ++ (show segmentsTrace) ++ " segments, but only " ++ show numSegments ++ " where generated."
    
  where chunks = chooseSegments privSize trace (segMap segProg) (pubSegments segProg)
        segmentsTrace = maximum (map chunkSeg chunks)
        numSegments = length (pubSegments segProg) + length (privSegments segProg) 
mkPrivateSegments :: Word -> Int -> [Segment reg MWord]
mkPrivateSegments len size = replicate (fromEnum len `div` size) (Segment [] [] size [] True True) 
