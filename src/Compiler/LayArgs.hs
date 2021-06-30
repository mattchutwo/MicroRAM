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

Lays argument sin the activation stack. This
pass goes from RTL -> LTL


-}
module Compiler.LayArgs
    ( layArgs
    ) where

import           Control.Monad.State (get, put) -- runState, runStateT, modify', State, StateT)
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


layArgs :: Rprog Metadata MWord -> WithNextReg Hopefully (Lprog Metadata Name MWord)
layArgs rtlProg = modifyCode (mapM layArgsFun) =<< ltlProg 
  where
    -- | Convert to LTL
    ltlProg :: WithNextReg Hopefully (Lprog Metadata Name MWord)
    ltlProg = lift $ rtlToLtl rtlProg

-- | 
layArgsFun :: LFunction Metadata Name MWord -> WithNextReg Hopefully (LFunction Metadata Name MWord)
layArgsFun = setCalleeArgsFun >=> initializeFunctionArgs

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
      return $ settingInstructions ++ [IRI (LCall ty mreg op typs args) md]
    setCalleeArgsInstr instr@(IRI _ _) = return [instr]

    setArgs :: md -> [MAOperand Name MWord] -> WithNextReg Hopefully [LTLInstr md Name MWord]
    setArgs md args = concat <$> mapM (setArg md) (zip (reverse args) [0..]) -- Arguments are set backwards  

    setArg :: md -> (MAOperand Name MWord, MWord) -> WithNextReg Hopefully [LTLInstr md Name MWord]
    setArg md (AReg r, i) = return [IRI (Lsetstack r Outgoing i Tint) md]
    setArg md (notReg, i) = do
      fresh <- newLocalName "fresh"
      return [MRI (MicroRAM.Imov fresh notReg) md, IRI (Lsetstack fresh Outgoing i Tint) md]


-- Initialize function arguments according to the calling convention.
-- Currently, this loads arguments from the stack with `Lgetstack Incoming 0 _ (Name "0")`.
initializeFunctionArgs :: LFunction Metadata VReg MWord -> WithNextReg Hopefully (LFunction Metadata Name MWord)
initializeFunctionArgs (LFunction fname typ typs argNms stackSize blocks) = do
  nextName <- get
  put (nextName + 1)
  let bname = bname' nextName 
  let instrs = getStackInstrs bname
  let b = BB bname instrs [] daginfo
  return $ LFunction fname typ typs argNms stackSize $ b : blocks
  where
    getStackInstrs :: Name ->  [LTLInstr Metadata VReg MWord]
    getStackInstrs bname = map (\(typ, (argNm, i)) -> 
                                let inst = Lgetstack Incoming i typ argNm
                                    md = trivialMetadata fname bname in
                                  IRI inst md 
                            ) $ zip typs $ zip argNms [0..]

    bname' id = Name id $ ((dbName fname) <> "_args")

    daginfo :: [Name]
    daginfo = case blocks of
      ((BB name _ _ _):_) -> [name]
      _ -> []

