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

import Compiler.Sparsity
import Compiler.CompilationUnit
import Compiler.Registers
import Compiler.CompilationUnit
import Compiler.Analysis

import MicroRAM.MRAMInterpreter
import MicroRAM.MicroRAM
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

data Output reg  =
  SecretOutput
  { program :: Program reg Word
  , params :: CircuitParameters
  , trace :: [StateOut]
  , adviceOut :: Map.Map Word [Advice]
  , initMem :: [Word]
  }
  | PublicOutput
  { program :: Program reg Word
  , params :: CircuitParameters
  } deriving (Eq, Show, Generic)

-- | Convert between the two outputs
mkOutputPublic :: Output reg -> Output reg
mkOutputPublic (SecretOutput a b _ _ _) = PublicOutput a b
mkOutputPublic (PublicOutput a b) = PublicOutput a b




-- ** Program
-- The program output is just `Program Word Word`


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
  , pcOut   :: Word 
  , regsOut :: [Word]
  } deriving (Eq, Show, Generic)

state2out :: Regs mreg => Word -> State mreg -> StateOut
state2out bound (State pc regs _ _ flag _ _) = StateOut flag pc (regToList bound regs)





-- * Producing Outputs

-- | Convert the output of the compiler (Compilation Unit) into Output
buildCircuitParameters trLen regData aData =
  CircuitParameters (regData2output regData) trLen (analyData2sparc aData)
  where regData2output (NumRegisters n) = fromIntegral n
        regData2output InfinityRegs = 8 -- FIX ME: Throw exception?

        analyData2sparc = foldr joinSparcData Map.empty  
        joinSparcData (SparsityData sparc2) spar1 =
          let sparc2' = Map.map fromIntegral sparc2 in  -- Make into Words
            Map.unionWith min sparc2' spar1

compUnit2Output :: CompilationUnit (Program reg Word) -> Output reg
compUnit2Output (CompUnit p trLen regData aData) =
  let circParams = buildCircuitParameters trLen regData aData in
  PublicOutput p circParams 

-- | Convert the Full output of the compiler (Compilation Unit) AND the interpreter
-- (Trace, Advice) into Output (a Private one).
-- The input Trace should be an infinite stream which we truncate by the given length.
secretOutput :: Regs reg => Trace reg -> [Word] -> CompilationUnit (Program reg Word) -> Output reg
secretOutput tr initM (CompUnit p trLen regData aData) =
  let circParams = buildCircuitParameters trLen regData aData in
    SecretOutput p circParams
    -- Trace
    (outputTrace trLen tr (numRegs circParams))
    -- Advice
    (outputAdvice trLen tr)
    -- initMem
    initM

  where outputTrace len tr regBound = takeW len $ map (state2out regBound) tr

        outputAdvice len tr = foldr joinAdvice Map.empty (takeW len $ zip [0..] tr)
        joinAdvice (i,state) adviceMap = case advice state of
                                       [] -> adviceMap
                                       ls -> Map.insert i ls adviceMap



fullOutput :: Regs reg => [Word] -> CompilationUnit (Program reg Word) -> Output reg
fullOutput initM compUnit =
  secretOutput (run initM $ programCU compUnit) initM compUnit 
