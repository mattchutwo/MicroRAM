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
import Compiler.Common (Name)
import Compiler.IRs

import MicroRAM (Instruction'(..))
import           MicroRAM (MWord)

type Statefully a = StateT Word Hopefully a

lowerExtInstr :: Text -> Maybe VReg -> md -> [MAOperand VReg MWord] ->
  Statefully [RTLInstr md MWord]
lowerExtInstr "trace" _ _ _ = return []
lowerExtInstr "tracestr" _ _ _ = return []
lowerExtInstr name _ _ _ | "trace_" `Text.isPrefixOf` name = return []
lowerExtInstr "malloc" (Just dest) md ops =
  return [MRI (Iextadvise "malloc" dest ops) md]
lowerExtInstr "free" _ _ _ = return []
lowerExtInstr "advise_poison" (Just dest) md ops =
  return [MRI (Iextadvise "advise_poison" dest ops) md]
lowerExtInstr name optDest _md ops = assumptError $
  "unsupported extension instruction: " ++ show name ++ " " ++
    intercalate ", " (maybe [] (\x -> [show x]) optDest ++ map show ops)

lowerInstr :: RTLInstr md MWord -> Statefully [RTLInstr md MWord]
lowerInstr (MRI (Iext name ops) md) = lowerExtInstr name Nothing md ops
lowerInstr (MRI (Iextval name dest ops) md) = lowerExtInstr name (Just dest) md ops
lowerInstr i = return [i]

-- TODO: share this basic rewriting infrastructure between here and Legalize
lowerInstrs :: [RTLInstr md MWord] -> Statefully [RTLInstr md MWord]
lowerInstrs instrs = concat <$> mapM lowerInstr instrs

lowerBlock :: BB Name (RTLInstr md MWord) -> Statefully (BB Name (RTLInstr md MWord))
lowerBlock (BB name body term dag) =
  BB name <$> lowerInstrs body <*> lowerInstrs term <*> pure dag

lowerFunc :: RFunction md MWord -> Hopefully (RFunction md MWord)
lowerFunc f = do
  (blocks', nextReg') <- runStateT (mapM lowerBlock $ funcBlocks f) (funcNextReg f)
  return $ f { funcBlocks = blocks', funcNextReg = nextReg' }

lowerExtensionInstrs :: Rprog md MWord -> Hopefully (Rprog md MWord)
lowerExtensionInstrs p = do
  code' <- mapM lowerFunc $ code p
  return $ p { code = code' }


