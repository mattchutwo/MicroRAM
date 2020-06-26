{-# LANGUAGE TupleSections #-}
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

import           Control.Applicative (liftA2)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           Compiler.CompileErrors
import           Compiler.IRs

type Registers = [String]

registerAlloc :: Rprog () Word -> Hopefully $ Lprog () Int Word
registerAlloc = mapM $ registerAllocFunc registers
  where
    numRegisters = 8
    registers = map show [1..numRegisters]


registerAllocFunc :: Registers -> RFunction () Word -> Hopefully $ LFunction () Int Word
registerAllocFunc registers (Function name typ typs blocks') = do
  let blocks = concatMap flattenBasicBlock blocks'

  -- Call helper?
  registerAllocFunc' blocks

  where
    registerAllocFunc' blocks = do

      liveness <- livenessAnalysis blocks

      let interferenceGraph = computeInterferenceGraph liveness

      (registerMapping, spills) <- registerColoring registers liveness blocks

      -- Unflatten basic block?
      
      -- return $ Function name typ typs blocks'
      return $ error "TODO"


-- Each returned basic block will have one instruction. 
-- Instruction identifiers are not guaranteed to be in order.
-- The first instruction will always have identifier 0.
flattenBasicBlock :: BB Name inst -> [BB (Name, Int) inst]
flattenBasicBlock (BB name instrs dag) = flatten $ zip instrs [0..]
  where
    flatten [] = []
    flatten [(instr, iid)] = 
      let dag' = map (, 0) dag in
      [BB (name, iid) [instr] dag']
    flatten ((instr, iid):instrs) = BB (name, iid) [instr] [(name, iid+1)]:flatten instrs


-- unflattenBasicBlock :: [BB Name inst] -> [BB (Name, Int) inst]


-- If the target instruction is Nothing, it's a return edge.
type LivenessResult instname = Map (instname, Maybe instname) (Set VReg) -- Map VReg [(Loc, Loc)]

livenessAnalysis :: [block] -> Hopefully (LivenessResult name)
livenessAnalysis blocks = error "TODO"


-- Returns the set of edges of the interference graph. Edges are tuples between two vertices, where the smaller vertex is first.
-- JP: Take spilled vars too?
computeInterferenceGraph :: LivenessResult instname -> Set (VReg, VReg)
computeInterferenceGraph = Map.foldr (\regs acc -> foldr insertEdge acc $ combinations $ Set.toList regs) mempty
  where
    insertEdge (r1, r2) acc | r1 > r2 = Set.insert (r2, r1) acc
    insertEdge edge acc               = Set.insert edge acc

    combinations []       = mempty
    combinations (r:regs) = liftA2 (,) (pure r) regs <> combinations regs


-- registerColoring :: Registers -> LivenessResult -> [block] -> Hopefully (Map VReg Reg, Set VReg)
registerColoring = error "TODO"

