{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Extension
Description : Lowers extension instructions to a form suitable for output
Maintainer  : santiago@galois.com
Stability   : prototype
-}

module Compiler.Extension
    ( lowerExtensionInstrs,
    ) where

import Control.Monad.State
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text

import Compiler.Errors
import Compiler.IRs

import MicroRAM (Instruction'(..))
import           MicroRAM (MWord)

type Statefully a = StateT Word Hopefully a

lowerExtInstr :: Text -> Maybe VReg -> [MAOperand VReg MWord] ->
  Statefully [RTLInstr () MWord]
lowerExtInstr "trace" _ _ = return []
lowerExtInstr "tracestr" _ _ = return []
lowerExtInstr name _ _ | "trace_" `Text.isPrefixOf` name = return []
lowerExtInstr "malloc" (Just dest) ops =
  return [MRI (Iextadvise "malloc" dest ops) ()]
lowerExtInstr "free" _ _ = return []
lowerExtInstr "advise_poison" (Just dest) ops =
  return [MRI (Iextadvise "advise_poison" dest ops) ()]
lowerExtInstr name optDest ops = assumptError $
  "unsupported extension instruction: " ++ show name ++ " " ++
    intercalate ", " (maybe [] (\x -> [show x]) optDest ++ map show ops)

lowerInstr :: RTLInstr () MWord -> Statefully [RTLInstr () MWord]
lowerInstr (MRI (Iext name ops) ()) = lowerExtInstr name Nothing ops
lowerInstr (MRI (Iextval name dest ops) ()) = lowerExtInstr name (Just dest) ops
lowerInstr i = return [i]

-- TODO: share this basic rewriting infrastructure between here and Legalize
lowerInstrs :: [RTLInstr () MWord] -> Statefully [RTLInstr () MWord]
lowerInstrs instrs = concat <$> mapM lowerInstr instrs

lowerBlock :: BB Name (RTLInstr () MWord) -> Statefully (BB Name (RTLInstr () MWord))
lowerBlock (BB name body term dag) =
  BB name <$> lowerInstrs body <*> lowerInstrs term <*> pure dag

lowerFunc :: RFunction () MWord -> Hopefully (RFunction () MWord)
lowerFunc f = do
  (blocks', nextReg') <- runStateT (mapM lowerBlock $ funcBlocks f) (funcNextReg f)
  return $ f { funcBlocks = blocks', funcNextReg = nextReg' }

lowerExtensionInstrs :: Rprog () MWord -> Hopefully (Rprog () MWord)
lowerExtensionInstrs p = do
  code' <- mapM lowerFunc $ code p
  return $ p { code = code' }


