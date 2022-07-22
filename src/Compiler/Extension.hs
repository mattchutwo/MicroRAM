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
      lowerInstr'
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
  -- TODO: Are these right?
  XSnapshot -> return []
  XCheck _ -> return []
lowerInstr (MirM (Iextval dest ext) md) = case ext of
  XLoadUnchecked ptr -> return [MirM (Iload WWord dest ptr) md]
-- Note that `Iextadvise` instructions are not handled here.  Those are left
-- unchanged until the very end, then they get serialized as `Iadvise`,
-- discarding the extension details.  This ensures the extension info is
-- available during the second (lower level) interpreter pass, so the
-- interpreter knows which kind of advice to give.
lowerInstr i = return [i]

-- | This version is used by the RISC V
-- It mimics `lowerInstr`, but the types don't match in a non-trivial way.
-- We'll need to be a bit more clever to deduplicate.
-- In this case, we need to use an extra `Imov`, to translate the unchecked load/store
-- We use a temporary register to acomodate any possible values.
lowerInstr'
  :: regT
  -> Instruction' regT regT op2
  -> [Instruction' regT regT op2]
lowerInstr' tempReg (Iext ext) = case ext of
  XTrace _ _ ->  []
  XTraceStr _ ->  []
  XTraceExec _ _ ->  []
  XFree _ ->  []
  XAccessValid _ _ ->  []
  XAccessInvalid _ _ ->  []
  XStoreUnchecked ptr val ->  [Imov tempReg val, Istore WWord ptr tempReg]
  -- TODO: Are these right?
  XSnapshot -> []
  XCheck _ -> []
lowerInstr' _ (Iextval dest ext) = case ext of
  XLoadUnchecked ptr ->  [Iload WWord dest ptr]
lowerInstr' _ i =  [i]

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


