{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Metadata
Description : Metadata for instructions during compilation 
Maintainer  : santiago@galois.com
Stability   : prototype

-}

module Compiler.Metadata
    ( Metadata(..),
      defaultMetadata,
      trivialMetadata,
    ) where

import Compiler.Common (Name(..), defaultName)

data Metadata = Metadata {
    mdFunction       :: Name   -- Name of the function containing instruction
  , mdBlock          :: Name   -- Name of the Block containing instruction
  , mdLine           :: Int      -- Line of source instruction
  , mdFunctionStart :: Bool      -- If instruction is at the start of a function (only added at stacking)
  , mdReturnCall :: Bool         -- If instruction happens right after a function call jump
  , mdIsCall :: Bool        -- ^ This `Ijmp` is a function call
  , mdIsReturn :: Bool      -- ^ This `Ijmp` is a return from the current function
  } deriving (Show, Read)

defaultMetadata :: Metadata
defaultMetadata =
  Metadata {
  mdFunction = defaultName
  , mdBlock = defaultName
  , mdLine = 0
  , mdFunctionStart = False
  , mdReturnCall = False
  , mdIsCall = False
  , mdIsReturn = False
  }

trivialMetadata :: Name -> Name -> Metadata
trivialMetadata fname bname = defaultMetadata {mdFunction = fname, mdBlock = bname}
