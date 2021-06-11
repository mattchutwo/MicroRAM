{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
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

import Compiler.Errors
import Compiler.Common (Name)
import Compiler.IRs

import MicroRAM (Instruction'(..), ExtInstr(..), ExtValInstr(..), pattern WWord, MWord)

type Statefully a = StateT Word Hopefully a

lowerInstr :: MIRInstr md MWord -> Statefully [MIRInstr md MWord]
lowerInstr (MirM (Iext ext) md) = case ext of
  XTrace _ _ -> return []
  XTraceStr _ -> return []
  XTraceExec _ _ -> return []
  XFree _ -> return []
  XAccessValid _ _ -> return []
  XAccessInvalid _ _ -> return []
  XStoreUnchecked ptr val -> return [MirM (Istore WWord ptr val) md]
lowerInstr (MirM (Iextval dest ext) md) = case ext of
  XLoadUnchecked ptr -> return [MirM (Iload WWord dest ptr) md]
-- Note that `Iextadvise` instructions are not handled here.  Those are left
-- unchanged until the very end, then they get serialized as `Iadvise`,
-- discarding the extension details.  This ensures the extension info is
-- available during the second (lower level) interpreter pass, so the
-- interpreter knows which kind of advice to give.
lowerInstr i = return [i]

-- TODO: share this basic rewriting infrastructure between here and Legalize
lowerInstrs :: [MIRInstr md MWord] -> Statefully [MIRInstr md MWord]
lowerInstrs instrs = concat <$> mapM lowerInstr instrs

lowerBlock :: BB Name (MIRInstr md MWord) -> Statefully (BB Name (MIRInstr md MWord))
lowerBlock (BB name body term dag) =
  BB name <$> lowerInstrs body <*> lowerInstrs term <*> pure dag

lowerFunc :: MIRFunction md MWord -> Statefully (MIRFunction md MWord)
lowerFunc f = do
  blocks' <- mapM lowerBlock $ funcBlocks f
  return $ f { funcBlocks = blocks'}

lowerExtensionInstrs :: (MIRprog md MWord, Word) -> Hopefully (MIRprog md MWord, Word)
lowerExtensionInstrs (p, nextReg) = do
  (code', nextReg') <- runStateT (mapM lowerFunc $ code p) nextReg  
  return (p { code = code' }, nextReg')


