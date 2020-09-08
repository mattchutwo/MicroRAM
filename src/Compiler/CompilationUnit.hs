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

import qualified Data.Map as Map

import Util.Util
import MicroRAM (MWord)

import Compiler.Analysis
--import Compiler.Common
import Compiler.LazyConstants
import Compiler.Registers


-- | The Compilation Unit
data CompilationUnit a prog = CompUnit
  { programCU :: prog
  , traceLen :: Word
  , regData :: RegisterData
  , aData   :: AnalysisData
  , initM   :: InitialMem
  , intermediateInfo :: a -- Other stuff we carry during compilation, but starts and ends as `()`
  }
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)
type CompilationResult prog = CompilationUnit () prog

prog2unit :: Word -> prog -> CompilationUnit () prog
prog2unit len p = CompUnit p len InfinityRegs [] [] ()

-- * Lifting operators

-- | Just compile, lift passes that only deal with code.
justCompile :: Monad m =>
  (progS -> m progT)
  -> CompilationUnit a progS
  -> m $ CompilationUnit a progT
justCompile pass p = mapM pass p

-- | Informed Compilation: passes that use analysis data but only change code
informedCompile :: Monad m =>
  (progS -> AnalysisData -> m progT)
  -> CompilationUnit a progS
  -> m $ CompilationUnit a progT
informedCompile pass cUnit = do
  p' <- pass (programCU cUnit) (aData cUnit)
  return $ cUnit {programCU = p'}
  
-- | Just Analyse, lift passes that don't modify the code.
justAnalyse :: Monad m  =>
  (prog  -> m AnalysisPiece)
  -> CompilationUnit a prog
  -> m $ CompilationUnit a prog
justAnalyse analysis cUnit = do
  a' <-  analysis (programCU cUnit)
  return $ cUnit {aData = a' : aData cUnit}


-- TODO: Should we move this to a separate file (e.g. Compiler/InitMem.hs) ?

data InitMemSegment = InitMemSegment
  { isSecret :: Bool
  , isReadOnly :: Bool
  , location :: MWord
  , segmentLen :: MWord
  , content :: Maybe [MWord]
  } deriving (Eq, Ord, Read, Show)

type InitialMem = [InitMemSegment]

-- | For creating an initial memory befor code labels and globals have been resolved.
type LazyInitSegment = (Maybe [LazyConst String MWord], InitMemSegment)
type LazyInitialMem = [LazyInitSegment] 


flatInitMem :: InitialMem -> Map.Map MWord MWord
flatInitMem = foldr initSegment Map.empty
  where initSegment :: InitMemSegment -> Map.Map MWord MWord -> Map.Map MWord MWord
        initSegment (InitMemSegment _ _ _ _ Nothing) = id
        initSegment (InitMemSegment _ _ loc _ (Just content)) =
          Map.union $ Map.fromList $
          -- Map with the new content
          zip [loc..] content

lengthInitMem :: InitialMem -> MWord
lengthInitMem = foldl (\tip seg -> max tip (segTip seg)) 0
  where segTip (InitMemSegment _ _ loc len _) = loc + len
