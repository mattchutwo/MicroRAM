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

import           Compiler.Errors

import           Compiler.Common
--import           Compiler.CompilationUnit
import           Compiler.IRs
import           Compiler.Metadata
--import           Compiler.Registers
import           MicroRAM (MWord)
--import           Util.Util


layArgs :: Rprog Metadata MWord -> WithNextReg Hopefully (Lprog Metadata Name MWord)
layArgs rtlProg = modifyCode (mapM layArgsFun) =<< ltlProg 
  where
    -- | Convert to LTL
    ltlProg :: WithNextReg Hopefully (Lprog Metadata Name MWord)
    ltlProg = lift $ rtlToLtl rtlProg

-- | 
layArgsFun :: LFunction Metadata Name MWord -> WithNextReg Hopefully (LFunction Metadata Name MWord)
layArgsFun fun = initializeFunctionArgs fun


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

