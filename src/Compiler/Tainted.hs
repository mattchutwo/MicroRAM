{-# LANGUAGE FlexibleContexts #-}

module Compiler.Tainted where

import Control.Monad.Error.Class (MonadError)
import Data.Word (Word8)

import Compiler.Errors
import MicroRAM

type Label = Word8 -- Labels only uses the lower 2 bits.
-- Lattice of labels:
--
--  label0   label1    label2
--        \     |      /
--         \    |     /
--          \   |    /
--            bottom
--
-- C definition:
--
--  typedef enum label {
--      label0 = 0,
--      label1 = 1,
--      label2 = 2,
--      bottom = 3,
--  } label;

untainted :: Label
untainted = 3

meet :: Label -> Label -> Label
meet l1 l2 = 
  if l1 < untainted then 
    if l1 == l2 then
      l1
    else
      untainted
  else
    untainted

canFlowTo :: Label -> Label -> Bool
canFlowTo l1 _  | l1 == untainted = True
canFlowTo l1 l2 | l1 == l2        = True
canFlowTo _  _                    = False

-- | Checks that a label is in bounds or throws an exception.
checkLabel :: MonadError CmplError m => Maybe Label -> m ()
checkLabel (Just l) | l <= untainted = return ()
checkLabel (Just l)                  = assumptError ("Invalid label: " <> show l)
checkLabel Nothing                   = return ()


concretizeLabel :: Maybe Label -> Label
concretizeLabel (Just w) = w
concretizeLabel Nothing = untainted

toLabel :: MonadError CmplError m => MWord -> m (Maybe Label)
toLabel w = do
  let l = Just $ fromIntegral w
  checkLabel l
  return l

