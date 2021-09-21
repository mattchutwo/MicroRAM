{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-
Module      : Choosing Segments
Description : 
Maintainer  : santiago@galois.com
Stability   : experimental

-}

module Segments (segment, SegmentedProgram(..), PublicSegmentMode(..), chooseSegment') where

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
import Segments.AbsInt

import Sparsity.Sparsity (Sparsity)

data SegmentedProgram reg = SegmentedProgram  { compiled :: CompilationResult (Program reg MWord) -- No METADATA
                                              , pubSegments :: [Segment reg MWord]
                                              , privSegments :: [Segment reg MWord]
                                              , segTrace :: Maybe [TraceChunk reg]
                                              , segAdvice :: Maybe (Map.Map MWord [Advice])}

data PublicSegmentMode =
  -- | Generate no public segments, only secret ones.
    PsmNone
  -- | Generate public segments for each function, depending on the number of
  -- function calls.
  | PsmFunctionCalls
  -- | Generate public segments based on abstract interpretation of the input
  -- program.
  | PsmAbsInt
  deriving (Show, Eq)

segment :: (Regs reg, Show reg) => PublicSegmentMode -> Int -> Maybe Int -> CompilationResult (AnnotatedProgram Metadata reg MWord) -> Hopefully (SegmentedProgram reg)
segment pubSegMode privSize privSegs compRes = do
  let funCount = functionUsage $ aData compRes 
  pubSegs <- case pubSegMode of
    PsmNone -> return []
    PsmFunctionCalls -> segmentProgram funCount $ (pmProg . lowProg . programCU) compRes
    PsmAbsInt -> segmentProgramWithAbsInt (lowProg $ programCU compRes)
  let privateSegments = if pubSegMode /= PsmNone then
        mkPrivateSegments (traceLen compRes) privSize privSegs
        else
        mkPrivateSegments (traceLen compRes) (fromEnum $ traceLen compRes) privSegs
  return $ SegmentedProgram compResNoMD pubSegs privateSegments Nothing Nothing
  where compResNoMD = compRes {programCU = fmap (fmap (map fst)) (programCU compRes)}

chooseSegment' :: (Show reg, Regs reg) => PublicSegmentMode -> Int -> Sparsity -> Trace reg -> SegmentedProgram reg -> Hopefully (SegmentedProgram reg)
chooseSegment' pubSegMode privSize spar trace segProg = do
  let prog = pmProg . lowProg . programCU . compiled $ segProg
  let segs = (V.fromList $ pubSegments segProg)
  chunks <- chooseSegments privSize' spar prog trace segs
  let segmentsTrace = maximum (map chunkSeg chunks)
  let numSegments = length (pubSegments segProg) + length (privSegments segProg) 
  -- Check if there are enough segments:
  if (numSegments) >= segmentsTrace then 
    return $ segProg {segTrace = Just chunks}
  else
    assumptError $ "Trace is not long enough. Execution uses: " ++ (show segmentsTrace) ++ " segments, but only " ++ show numSegments ++ " where generated."

  where privSize' = if pubSegMode /= PsmNone then privSize else (fromEnum $ traceLen $ compiled segProg)

mkPrivateSegments :: Word -> Int -> Maybe Int -> [Segment reg MWord]
mkPrivateSegments len size privSegs = replicate howMany (Segment [] [] size [] True True)
  where howMany = case privSegs of
                    Just numSegs -> numSegs
                    Nothing ->  fromEnum len `div` size
   

