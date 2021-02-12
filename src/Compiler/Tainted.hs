
module Compiler.Tainted where

import Data.Word (Word16)

import MicroRAM

type Label = Word16

untainted :: Label
untainted = maxBound

concretizeLabel :: Maybe Label -> Label
concretizeLabel (Just w) = w
concretizeLabel Nothing = untainted

toLabel :: MWord -> Label
toLabel = fromIntegral

