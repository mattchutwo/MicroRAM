{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Lay Arguments
Description : Lay Arguments
Maintainer  : 
Stability   : 

Lays arguments in the activation stack. This
pass goes from RTL -> LTL

After this pass, function arguments are passed in an abstract
stack slots. For this pass there are two types of slots:

- Outgoing: where the caller saves the arguments.
- Incoming: where the callee finds the arguments.

There is third slot, Local, where registers can be spilled.
Those are introduced during register allocation.

-}
module Compiler.LayArgs
    ( layArgs
    ) where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad ((>=>))

import           Compiler.Errors

import           Compiler.Common
--import           Compiler.CompilationUnit
import           Compiler.IRs
import           Compiler.Metadata
--import           Compiler.Registers
import           MicroRAM (MWord, Instruction'(..))
import           Util.Util


-- | translates the program from RTL to LTL and
-- sets function arguments in an abstract stack slots
layArgs :: Rprog Metadata MWord -> WithNextReg Hopefully (Lprog Metadata Name MWord)
layArgs rtlProg = modifyCode (mapM layArgsFun) =<< ltlProg 
  where
    -- | Convert to LTL
    ltlProg :: WithNextReg Hopefully (Lprog Metadata Name MWord)
    ltlProg = lift $ rtlToLtl rtlProg

-- | Sets function arguments in an abstract stack slots in two steps:
-- 1. setCalleeArgsFun: lays Outgoing arguments on the stack
-- 2. initializeFunctionArgs: retrieves Incoming arguments from the stack
layArgsFun :: LFunction Metadata Name MWord -> WithNextReg Hopefully (LFunction Metadata Name MWord)
layArgsFun = setCalleeArgsFun >=> initializeFunctionArgs


-- | lays Outgoing arguments on the stack
setCalleeArgsFun :: LFunction Metadata Name MWord -> WithNextReg Hopefully (LFunction Metadata Name MWord)
setCalleeArgsFun fun = do 
  funBody' <- mapM setCalleeArgsBlock $ funBody fun
  return $ fun {funBody = funBody' } 
  where
    setCalleeArgsBlock :: BB Name (LTLInstr Metadata Name MWord) -> WithNextReg Hopefully $ BB Name (LTLInstr Metadata Name MWord)
    setCalleeArgsBlock (BB nm insts tinsts dag) = do
      insts'  <- concat <$> mapM setCalleeArgsInstr insts
      tinsts' <- concat <$> mapM setCalleeArgsInstr tinsts  
      return $ BB nm insts' tinsts' dag
    setCalleeArgsInstr :: LTLInstr Metadata Name MWord -> WithNextReg Hopefully [LTLInstr Metadata Name MWord]
    setCalleeArgsInstr instr@(MRI _ _) = return [instr]
    setCalleeArgsInstr (IRI (LCall ty mreg op typs args) md) = do
      settingInstructions <- (setArgs md args)
      -- The functIon doesn't carry arguments, but retains the types for reference
      -- types *might* be using during stacking to remove arguments from the stack.
      return $ settingInstructions ++ [IRI (LCall ty mreg op typs []) md]
    setCalleeArgsInstr instr@(IRI _ _) = return [instr]

    setArgs :: md -> [MAOperand Name MWord] -> WithNextReg Hopefully [LTLInstr md Name MWord]
    setArgs md args = concat <$> mapM (setArg md) (zip (reverse args) [0..]) -- Arguments are set backwards  

    setArg :: md -> (MAOperand Name MWord, MWord) -> WithNextReg Hopefully [LTLInstr md Name MWord]
    setArg md (AReg r, i) = return [IRI (Lsetstack r Outgoing i Tint) md]
    setArg md (notReg, i) = do
      fresh <- newLocalName "fresh"
      return [MRI (MicroRAM.Imov fresh notReg) md, IRI (Lsetstack fresh Outgoing i Tint) md]


-- | Retrieves Incoming arguments from the stack
initializeFunctionArgs :: LFunction Metadata VReg MWord -> WithNextReg Hopefully (LFunction Metadata Name MWord)
initializeFunctionArgs lf@(LFunction fname typ typs argNms stackSize blocks) = do
  bname <- newLocalName (dbName fname <> "_Args")
  let instrs = getStackInstrs bname
  -- Don't insert a block if it'd be empty.
  if null instrs then
    return $ lf
  else do
    let b = BB bname instrs [] daginfo
    return $ LFunction fname typ typs argNms stackSize $ b : blocks
  where
    getStackInstrs :: Name ->  [LTLInstr Metadata VReg MWord]
    getStackInstrs bname = map (\(typ, (argNm, i)) -> 
                                let inst = Lgetstack Incoming i typ argNm
                                    md = trivialMetadata fname bname in
                                  IRI inst md 
                            ) $ zip typs $ zip argNms [0..]

    daginfo :: [Name]
    daginfo = case blocks of
      ((BB name _ _ _):_) -> [name]
      _ -> []

