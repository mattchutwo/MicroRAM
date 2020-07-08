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
module Compiler.Analysis
    ( AnalysisData(..)
    ) where

data AnalysisData = ADempty
  deriving (Eq, Ord, Read, Show)
