{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Analysis
Description : The data type of analysis information
Maintainer  : santiago@galois.com
Stability   : experimental

Metadata is produced by the compiler and passed down to the witness generator.

-}
module Compiler.Analysis where

import qualified Data.Map as Map
import Sparsity.Sparsity

data AnalysisPiece =
  SparsityData Sparsity
  deriving (Eq, Ord, Read, Show)

type AnalysisData = [AnalysisPiece]

-- Onl;y the first entry of sparsity will be considered. 
getSparsity :: AnalysisData -> Sparsity 
getSparsity [] = Map.empty
getSparsity (SparsityData sparc: _) = sparc
--getSparsity (_: datas) = getSparsity datas
