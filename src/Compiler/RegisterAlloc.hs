{-# LANGUAGE TypeOperators #-}
{-|
Module      : Register allocation
Description : RTL -> LTL
Maintainer  : 
Stability   : 


Description

-}
module Compiler.RegisterAlloc
    ( registerAlloc, --compileStraight
    ) where

import           Data.Map (Map)

import           Compiler.CompileErrors
import           Compiler.IRs

type Registers = [String]

registerAlloc :: Rprog () Word -> Hopefully $ Lprog () Int Word
registerAlloc = mapM $ registerAllocFunc registers
  where
    numRegisters = 8
    registers = map show [1..numRegisters]


registerAllocFunc :: Registers -> RFunction () Word -> Hopefully $ LFunction () Int Word
registerAllocFunc registers (Function name typ typs blocks) = do
  liveness <- livenessAnalysis blocks

  blocks' <- linearScan registers liveness blocks
  
  return $ Function name typ typs blocks'

type LivenessResult = () -- Map VReg [(Loc, Loc)]

livenessAnalysis :: [block] -> Hopefully LivenessResult
livenessAnalysis blocks = error "TODO"

linearScan :: Registers -> LivenessResult -> [block] -> Hopefully [rblock]
linearScan = error "TODO"

