{-# LANGUAGE FlexibleContexts #-}

module Compiler.Tainted where

import Control.Monad.Error.Class (MonadError)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Word (Word8)

import Compiler.Errors
import MicroRAM

type Label = Word8 -- Labels only uses the lower 2 bits.

maybeTainted :: Label
maybeTainted = 2

maybeTaintedWord :: Vector Label
maybeTaintedWord = replicateWord maybeTainted

bottom :: Label
bottom = 3

bottomWord :: Vector Label
bottomWord = replicateWord bottom

-- join MaybeTainted _ = MaybeTainted
-- join _ MaybeTainted = MaybeTainted
-- join Label0 Label1  = MaybeTainted
-- join Label1 Label0  = MaybeTainted
-- join Label0 Label0  = Label0
-- join Bottom Label0  = Label0
-- join Label0 Bottom  = Label0
-- join Label1 Label1  = Label1
-- join Bottom Label1  = Label1
-- join Label1 Bottom  = Label1
-- join Bottom Bottom  = Bottom
join :: Label -> Label -> Label
join l1 l2
  | l1 == maybeTainted = maybeTainted
  | l2 == maybeTainted = maybeTainted
  | l1 == bottom       = l2
  | l2 == bottom       = l1
  | l1 /= l2           = maybeTainted
  | l1 == l2           = l1
  | otherwise          = error "unreachable"

-- | Checks that a label is in bounds or throws an exception.
checkLabelsBound :: (Foldable f, MonadError CmplError m) => Maybe (f Label) -> m ()
checkLabelsBound (Just ls) = mapM_ checkLabelBound ls
checkLabelsBound Nothing   = return ()

-- | Checks that a label is in bounds or throws an exception.
checkLabelBound :: MonadError CmplError m => Label -> m ()
checkLabelBound l | l <= bottom = return ()
checkLabelBound l               = assumptError ("Invalid label: " <> show l)

replicateWord :: Label -> Vector Label
replicateWord = Vec.replicate wordBytes

padBottomWord :: Vector Label -> Vector Label
padBottomWord ls = ls <> Vec.replicate (wordBytes - Vec.length ls) bottom

toLabel :: MonadError CmplError m => MWord -> m Label
toLabel w = do
  let l = fromIntegral w
  checkLabelBound l
  return l


-- canFlowTo :: Label -> Label -> Bool
-- canFlowTo _ MaybeTainted = True
-- canFlowTo MaybeTainted _ = True -- ????
-- canFlowTo Bottom _       = True
-- canFlowTo l1 l2          = l1 == l2
canFlowTo :: Label -> Label -> Bool
canFlowTo _  l2 | l2 == maybeTainted = True
canFlowTo l1 _  | l1 == maybeTainted = True -- ????
canFlowTo l1 _  | l1 == bottom       = True
canFlowTo l1 l2 | l1 == l2           = True
canFlowTo _  _                       = False





-- OLD


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
-- 
-- meet :: Label -> Label -> Label
-- meet l1 l2 = 
--   if l1 < untainted then 
--     if l1 == l2 then
--       l1
--     else
--       untainted
--   else
--     untainted
-- 
-- concretizeLabel :: Maybe (Vector Label) -> [Label] -- Vector Label
-- concretizeLabel (Just w) = Vec.toList w
-- concretizeLabel Nothing = Vec.toList untaintedWord

