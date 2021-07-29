{-# LANGUAGE FlexibleContexts #-}

module Compiler.Tainted where

import Control.Monad.Error.Class (MonadError)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
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
checkLabels :: (Foldable f, MonadError CmplError m) => Maybe (f Label) -> m ()
checkLabels (Just ls) = mapM_ checkLabel ls
checkLabels Nothing   = return ()

-- | Checks that a label is in bounds or throws an exception.
checkLabel :: MonadError CmplError m => Label -> m ()
checkLabel l | l <= untainted = return ()
checkLabel l                  = assumptError ("Invalid label: " <> show l)


replicateWord :: Label -> Vector Label
replicateWord = Vec.replicate wordBytes

untaintedWord :: Vector Label
untaintedWord = replicateWord untainted

padUntaintedWord :: Vector Label -> Vector Label
padUntaintedWord ls = ls <> Vec.replicate (wordBytes - Vec.length ls) untainted

concretizeLabel :: Maybe (Vector Label) -> [Label] -- Vector Label
concretizeLabel (Just w) = Vec.toList w
concretizeLabel Nothing = Vec.toList untaintedWord

toLabel :: MonadError CmplError m => MWord -> m Label
toLabel w = do
  let l = fromIntegral w
  checkLabel l
  return l

