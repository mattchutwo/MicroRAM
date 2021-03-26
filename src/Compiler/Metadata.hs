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

--import Compiler.Common

data Metadata = Metadata {
    mdFunction       :: String   -- Name of the function containing instruction
  , mdBlock          :: String   -- Name of the Block containing instruction
  , mdLine           :: Int      -- Line of source instruction
  , mdFunctionStart :: Bool      -- If instruction is at the start of a function (only added at stacking)
  , mdReturnCall :: Bool         -- If instruction happens right after a function call jump
  } deriving (Show, Read)
  
defaultMetadata :: Metadata
defaultMetadata =
  Metadata {
  mdFunction = ""
  , mdBlock = ""
  , mdLine = 0
  , mdFunctionStart = False
  , mdReturnCall = False
  }

trivialMetadata :: String -> String -> Metadata
trivialMetadata fname bname = defaultMetadata {mdFunction = fname, mdBlock = bname}
