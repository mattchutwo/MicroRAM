{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Metadata
Description : Metadata for instructions during compilation 
Maintainer  : santiago@galois.com
Stability   : prototype

-}

module Compiler.Metadata
    ( Metadata(..),
      trivialMetadata,
    ) where

--import Compiler.Common

data Metadata = Metadata {
    mdFunction       :: String   -- Name of the function containing instruction
  , mdBlock          :: String   -- Name of the Block containing instruction
  , mdLine           :: Int      -- Line of source instruction
  , mdReturnCall :: Bool         -- If instruction happens right after a function call jump
  }
  
defaultMetadata :: Metadata
defaultMetadata =
  Metadata {
  mdFunction = ""
  , mdBlock = ""
  , mdLine = 0
  , mdReturnCall = False
  }

trivialMetadata :: String -> String -> Metadata
trivialMetadata fname bname = defaultMetadata {mdFunction = fname, mdBlock = bname}
