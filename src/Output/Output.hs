{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Output
Description : Data structures for information passed down to Circuit generator
Maintainer  : santiago@galois.com
Stability   : experimental

Data structures passed down to Circuit generator

-}

module Output.Output where
import qualified Data.Map as Map
import GHC.Generics

import qualified Debug.Trace as Trace(trace)

import Compiler.CompilationUnit
import Compiler.Registers
import Compiler.Analysis

import MicroRAM.MRAMInterpreter
import MicroRAM

import Segments.Segmenting
import Segments.ChooseSegments

import Sparsity.Sparsity

import Util.Util


-- * Output structures
  
-- | Output
-- This is what the front end passes to the circuit generator.
-- It's generally divided in two parts:
--
-- * Public Output
-- Public output is generated "statically" (without knowing the input). It can be obtained
-- by the verifier and the prover and has the following elements:
-- 1. Program
-- 2. Parameters
--    * Number of registers
--    * Trace length
--    * Sparcity
--
-- Secret Output
-- Public output is generated "statically" (without knowing the input). It can be obtained
-- by the verifier and the prover and has the following elements:
-- 1. Trace
-- 2. Advice
-- 3. Initial Memory
--   
-- Notice `trace`, `advice` and `initMem` throw execptions (when called on PublicOutput)
data TraceChunkOut reg = TraceChunkOut {
  chunkSegOut :: Int
  , chunkStatesOut :: [StateOut]}
  deriving (Eq, Show, Generic)
  
data Output reg  =
  SecretOutput
  { program :: Program reg MWord
  , segmentsOut :: [SegmentOut]
  , params :: CircuitParameters
  , initMem :: InitialMem
  , traceOut :: [TraceChunkOut reg]
  , adviceOut :: Map.Map MWord [Advice]
  }
  | PublicOutput
  { program :: Program reg MWord
  , segmentsOut :: [SegmentOut]
  , params :: CircuitParameters
  , initMem :: InitialMem
  } deriving (Eq, Show, Generic)

data SegmentOut
  = SegmentOut {constraintsOut :: [Constraints],
                segLenOut :: Int,
                segSucOut :: [Int],
                fromNetworkOut :: Bool,
                toNetworkOut :: Bool}
  deriving (Eq, Show, Generic)
mkSegmentOut :: Segment reg MWord -> SegmentOut
mkSegmentOut (Segment _ constr len suc fromNet toNet) = SegmentOut constr len suc fromNet toNet

-- | Convert between the two outputs
mkOutputPublic :: Output reg -> Output reg
mkOutputPublic (SecretOutput a b c d _ _) = PublicOutput a b c d
mkOutputPublic (PublicOutput a b c d) = PublicOutput a b c d

mkOutputPrivate :: [TraceChunkOut reg] -> Map.Map MWord [Advice] -> Output reg -> Output reg
mkOutputPrivate trace adv (PublicOutput a b c d ) = SecretOutput a b c d trace adv
mkOutputPrivate trace adv (SecretOutput a b c d _ _) = SecretOutput a b c d trace adv



-- ** Program
-- The program output is just `Program Word MWord`


-- ** Parameters
type SparcityInfo = Word

data CircuitParameters = CircuitParameters
  { numRegs :: Word
  , traceLength :: Word
  , sparcity :: Map.Map InstrKind SparcityInfo
  } deriving (Eq, Show, Generic)
  
 
-- ** Traces 

-- *** State

-- | State with only the parts passed to the output.

data StateOut = StateOut
  { flagOut :: Bool
  , pcOut   :: MWord
  , regsOut :: [MWord]
--  , adviceOut :: [Advice]
  } deriving (Eq, Show, Generic)

-- | Compiler is allowed to concretise.
-- This is assuming all registers are initialized to 0.
-- FIXME: showld we pass the maybe type to the circuit generator? 
concretize :: Maybe MWord -> MWord
concretize (Just w) = w
concretize Nothing = 0

state2out :: Regs mreg => Word -> ExecutionState mreg -> StateOut
state2out bound (ExecutionState pc regs _ _ _advice flag _ _ _) =
  StateOut flag pc (map concretize $ regToList bound regs) -- Advice is ignored





-- * Producing Outputs

-- | Convert the output of the compiler (Compilation Unit) into Output
buildCircuitParameters
  :: Foldable t =>
     Word
  -> RegisterData
  -> t AnalysisPiece
  -> Word
  -> CircuitParameters
buildCircuitParameters trLen regData aData regNum = -- Ok regNum can be removed if InfinityRegs doesn't show up here. 
  CircuitParameters (regData2output regData) trLen (analyData2sparc aData)
  where regData2output (NumRegisters n) = fromIntegral n
        regData2output InfinityRegs = regNum -- FIX ME: Throw exception?

        analyData2sparc = foldr joinSparcData Map.empty  
        joinSparcData (SparsityData sparc2) spar1 =
          let sparc2' = Map.map fromIntegral sparc2 in  -- Make into Words
            Map.unionWith min sparc2' spar1

compUnit2Output :: Regs reg => [Segment reg MWord] -> CompilationResult (Program reg MWord) -> Output reg
compUnit2Output segs (CompUnit p trLen regData aData initMem _) =
  let regNum = getRegNum regData
      circParams = buildCircuitParameters trLen regData aData regNum
      segsOut = map mkSegmentOut segs in
  PublicOutput (lowProg p) segsOut circParams initMem

-- | Convert the Full output of the compiler (Compilation Unit) AND the interpreter
-- (Trace, Advice) into Output (a Private one).
-- The input Trace should be an infinite stream which we truncate by the given length.

-- secretOutput :: Regs reg => Trace reg -> CompilationResult (Program reg MWord) -> Output reg
-- secretOutput tr (CompUnit p trLen regData aData initM _) =
--   let regNum = getRegNum regData in
--   let circParams = buildCircuitParameters trLen regData aData regNum in
--     SecretOutput (lowProg p) circParams
--     -- initMem
--     initM
--     -- Trace (trace should be trimmed already)
--     (outputTrace trLen tr (numRegs circParams))
--     -- Advice
--     (outputAdvice trLen tr)

--   where outputAdvice len tr = foldr joinAdvice Map.empty (takeEnum len $ zip [0..] tr)
--         joinAdvice (i,state) adviceMap = case advice state of
--                                        [] -> adviceMap
--                                        ls -> Map.insert i ls adviceMap
 
outputStates
  :: (Enum a1, Regs mreg) => a1 -> Word -> [ExecutionState mreg] -> [StateOut]
outputStates len regBound tr = takeEnum len $ map (state2out regBound) tr

outputTraceChunk :: (Regs reg) => Word -> TraceChunk reg -> TraceChunkOut reg
outputTraceChunk regBound (TraceChunk location trace) = 
  TraceChunkOut location (map (state2out regBound) trace)
outputTrace :: (Regs reg) => Word -> [TraceChunk reg] -> [TraceChunkOut reg]
outputTrace regBound tr = map (outputTraceChunk regBound) tr 


  
getRegNum :: RegisterData -> Word
getRegNum InfinityRegs = 0
getRegNum (NumRegisters n) = toEnum n 

-- | We only look at what registers are assigned too

countRegs :: Regs regT => Program regT MWord -> Word
countRegs p =
  --(Trace.trace $ "lenght of list is: " ++ show (map getRegAssign p))
  maximum $ map getRegAssign p
  where getRegAssign (Iand reg1 _reg2 _  ) =  toWord reg1  
        getRegAssign (Ior reg1 _reg2 _   ) =  toWord reg1 
        getRegAssign (Ixor reg1 _reg2 _  ) =  toWord reg1 
        getRegAssign (Inot reg1 _       ) =  toWord reg1 
        getRegAssign (Iadd reg1 _reg2 _  ) =  toWord reg1 
        getRegAssign (Isub reg1 _reg2 _  ) =  toWord reg1 
        getRegAssign (Imull reg1 _reg2 _ ) =  toWord reg1 
        getRegAssign (Iumulh reg1 _reg2 _) =  toWord reg1 
        getRegAssign (Ismulh reg1 _reg2 _) =  toWord reg1 
        getRegAssign (Iudiv reg1 _reg2 _ ) =  toWord reg1 
        getRegAssign (Iumod reg1 _reg2 _ ) =  toWord reg1 
        getRegAssign (Ishl reg1 _reg2 _  ) =  toWord reg1 
        getRegAssign (Ishr reg1 _reg2 _  ) =  toWord reg1 
        getRegAssign (Icmpe reg1 _ _      ) =  toWord reg1 
        getRegAssign (Icmpa reg1 _ _      ) =  toWord reg1 
        getRegAssign (Icmpae reg1 _ _     ) =  toWord reg1 
        getRegAssign (Icmpg reg1 _ _      ) =  toWord reg1 
        getRegAssign (Icmpge reg1 _ _     ) =  toWord reg1 
        getRegAssign (Imov reg1 _       ) =  toWord reg1 
        getRegAssign (Icmov reg1 _ _      ) =  toWord reg1 
        getRegAssign (Istore _ _ reg1   ) =  toWord reg1 
        getRegAssign (Iload _ reg1 _    ) =  toWord reg1 
        getRegAssign (Iread reg1 _      ) =  toWord reg1 
        getRegAssign _ =  0 

{-
b = countRegs [Iload (NewName 0) (Const 1),Iload (Name "0") (Reg (NewName 0)),Iadd (NewName 0) (NewName 0) (Const 1),Iload (Name "1") (Reg (NewName 0)),Iadd (NewName 0) (NewName 0) (Const 1),Imov (NewName 2) (Const 28),Iadd (NewName 0) (NewName 0) (Const 1),Istore (Reg (NewName 0)) (NewName 2),Iadd (NewName 0) (NewName 0) (Const 1),Istore (Reg (NewName 0)) (NewName 1),Imov (NewName 1) (Reg (NewName 0)),Isub (NewName 0) (NewName 0) (Const 0),Imov (Name "1") (Reg (NewName 0)),Iadd (NewName 0) (NewName 0) (Const 1),Imov (Name "2") (Reg (NewName 0)),Iadd (NewName 0) (NewName 0) (Const 1),Imov (NewName 2) (Const 0),Istore (Reg (Name "1")) (NewName 2),Imov (NewName 3) (Const 21),Istore (Reg (Name "2")) (NewName 3),Iload (Name "3") (Reg (Name "2")),Iload (Name "4") (Reg (Name "2")),Iadd (Name "5") (Name "3") (Reg (Name "4")),Imov (NewName 2) (Reg (Name "5")),Imov (NewName 0) (Reg (NewName 1)),Isub (NewName 1) (NewName 1) (Const 1),Iload (NewName 1) (Reg (NewName 1)),Ijmp (Reg (NewName 1)),Ianswer (Reg (NewName 2))]
-}
