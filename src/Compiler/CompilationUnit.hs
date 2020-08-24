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

import Compiler.Registers
import Compiler.Analysis

-- | The Compilation Unit
data CompilationUnit prog = CompUnit
  { programCU :: prog
  , traceLen :: Word
  , regData :: RegisterData
  , aData   :: AnalysisData
  , initM   :: InitialMem
  }
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

prog2unit :: Word -> prog -> CompilationUnit prog
prog2unit len p = CompUnit p len InfinityRegs [] []

-- * Lifting operators

-- | Just compile, lift passes that only deal with code.
justCompile :: Monad m =>
  (progS -> m progT)
  -> CompilationUnit progS
  -> m $ CompilationUnit progT
justCompile pass p = mapM pass p

-- | Informed Compilation: passes that use analysis data but only change code
informedCompile :: Monad m =>
  (progS -> AnalysisData -> m progT)
  -> CompilationUnit progS
  -> m $ CompilationUnit progT
informedCompile pass (CompUnit prog trLen rdata adata initMem) = do
  p' <- pass prog adata
  return $ CompUnit p' trLen rdata adata initMem
  
-- | Just Analyse, lift passes that don't modify the code.
justAnalyse :: Monad m  =>
  (prog  -> m AnalysisPiece)
  -> CompilationUnit prog
  -> m $ CompilationUnit prog
justAnalyse analysis (CompUnit prog trLen rdata adata initMem) = do
  a' <-  analysis prog
  return $ CompUnit prog trLen rdata (a' : adata) initMem


-- TODO: Should we move this to a separate file (e.g. Compiler/InitMem.hs) ?

data InitMemSegment = InitMemSegment
  { isSecret :: Bool
  , isReadOnly :: Bool
  , location :: Word
  , segmentLen :: Word
  , content :: Maybe [Word]
  } deriving (Eq, Ord, Read, Show)

type InitialMem = [InitMemSegment]


flatInitMem :: InitialMem -> Map.Map Word Word
flatInitMem = foldr initSegment Map.empty
  where initSegment :: InitMemSegment -> Map.Map Word Word -> Map.Map Word Word
        initSegment (InitMemSegment _ _ _ _ Nothing) = id
        initSegment (InitMemSegment _ _ loc _ (Just content)) =
          Map.union $ Map.fromList $
          -- Map with the new content
          zip [loc..] content

lengthInitMem :: InitialMem -> Word
lengthInitMem = foldl (\tip seg -> max tip (segTip seg)) 0
  where segTip (InitMemSegment _ _ loc len _) = loc + len
