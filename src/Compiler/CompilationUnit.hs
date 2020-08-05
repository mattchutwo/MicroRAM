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

import Util.Util

import Compiler.Registers
import Compiler.Analysis

-- | The Compilation Unit
data CompilationUnit prog = CompUnit
  { program :: prog
  , regData :: RegisterData
  , aData   :: AnalysisData
  }
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

prog2unit :: prog -> CompilationUnit prog
prog2unit p = CompUnit p InfinityRegs [] 

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
informedCompile pass (CompUnit prog rdata adata) = do
  p' <- pass prog adata
  return $ CompUnit p' rdata adata
  
-- | Just Analyse, lift passes that don't modify the code.
justAnalyse :: Monad m  =>
  (prog  -> m AnalysisPiece)
  -> CompilationUnit prog
  -> m $ CompilationUnit prog
justAnalyse analysis (CompUnit prog rdata adata) = do
  a' <-  analysis prog
  return $ CompUnit prog rdata (a' : adata)


