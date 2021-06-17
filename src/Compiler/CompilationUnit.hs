{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
module Compiler.CompilationUnit where

{-
Module      : Compilation unit
Description : records for compilation. Carry Code and extra data.
Maintainer  : santiago@galois.com
Stability   : experimental


Wrapper for compilation programs that add compiler metadata to
compilations units.

-}

import Data.Default (def)
import qualified Data.Map as Map

import Util.Util
import MicroRAM (MWord)

import Compiler.Analysis
--import Compiler.Common
import Compiler.LazyConstants
import Compiler.Registers
import Compiler.Tainted
import Data.Vector (Vector)


-- | The Compilation Unit
data CompilationUnit' a prog = CompUnit
  { programCU :: prog
  , traceLen :: Word
  , regData :: RegisterData
  , aData   :: AnalysisData
  , intermediateInfo :: a -- Other stuff we carry during compilation, but starts and ends as `()`
  }
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- | Multiple variants of the compiled program.
data MultiProg prog = MultiProg
  -- | "High-level" variant, used for the first interpreter pass.  Contains
  -- extension instructions (`Iext` + `Iextval`) in their original forms,
  -- partly for instrumentation purposes.
  { highProg :: prog
  -- | "Low-level" variant, used for the second interpreter pass.  Does not
  -- contain extension instructions, with the exception of `Iextadvise`, which
  -- gets serialized as a plain `Iadvise` instead.
  , lowProg :: prog
  }
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data ProgAndMem prog = ProgAndMem { pmProg :: prog, pmMem :: InitialMem }
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

type CompilationUnit a prog = CompilationUnit' a (ProgAndMem prog)

type CompilationResult prog = CompilationUnit' () (MultiProg (ProgAndMem prog))

prog2unit :: Word -> prog -> CompilationUnit () prog
prog2unit len p = CompUnit (ProgAndMem p []) len InfinityRegs def ()

-- * Lifting operators

-- | Just compile, lift passes that only deal with code.
justCompile :: Monad m =>
  (progS -> m progT)
  -> CompilationUnit a progS
  -> m $ CompilationUnit a progT
justCompile pass p = mapM (mapM pass) p

-- | Informed Compilation: passes that use analysis data but only change code
informedCompile :: Monad m =>
  (progS -> AnalysisData -> m progT)
  -> CompilationUnit a progS
  -> m $ CompilationUnit a progT
informedCompile pass cUnit = do
  p' <- pass (pmProg $ programCU cUnit) (aData cUnit)
  return $ cUnit {programCU = (programCU cUnit) { pmProg = p' } }
  
-- | Just Analyse, lift passes that don't modify the code.
justAnalyse :: Monad m  =>
  (prog  -> m AnalysisPiece)
  -> CompilationUnit a prog
  -> m $ CompilationUnit a prog
justAnalyse analysis cUnit = do
  a' <-  analysis (pmProg $ programCU cUnit)
  return $ cUnit {aData = addAnalysisPiece a' $ aData cUnit}

data InitMemSegment = InitMemSegment
  { isSecret :: Bool
  , isReadOnly :: Bool
  , isHeapInit :: Bool
  , location :: MWord
  , segmentLen :: MWord
  , content :: Maybe [MWord]
  , labels :: Maybe [Vector Label] -- [Word16] -- TODO: Type level stuff to enable tainted things.
  } deriving (Eq, Ord, Read, Show)

type InitialMem = [InitMemSegment]

-- | For creating an initial memory befor code labels and globals have been resolved.
type LazyInitSegment = (Maybe [LazyConst String MWord], InitMemSegment)
type LazyInitialMem = [LazyInitSegment] 


flatInitMem :: InitialMem -> Map.Map MWord MWord
flatInitMem = foldr initSegment Map.empty
  where initSegment :: InitMemSegment -> Map.Map MWord MWord -> Map.Map MWord MWord
        initSegment (InitMemSegment _ _ _ _ _ Nothing _) = id
        initSegment (InitMemSegment _ _ _ loc _ (Just content) _) =
          Map.union $ Map.fromList $
          -- Map with the new content
          zip [loc..] content

flatInitTaintedMem :: InitialMem -> Map.Map MWord (Vector Label)
flatInitTaintedMem = foldr initSegment Map.empty
  where initSegment (InitMemSegment _ _ _ _ _ _ Nothing) = id
        initSegment (InitMemSegment _ _ _ loc _ _ (Just labels)) =
          Map.union $ Map.fromList $
          -- Map with the new content
          zip [loc..] labels

lengthInitMem :: InitialMem -> MWord
lengthInitMem = foldl (\tip seg -> max tip (segTip seg)) 0
  where segTip (InitMemSegment _ _ heapInit loc len _ _)
          | heapInit = 0
          | otherwise = loc + len
