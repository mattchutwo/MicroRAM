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
      trivialRegisterAlloc, -- FIXME : remove when reg alloc completed
    ) where

import           Control.Applicative (liftA2)
import           Data.Graph.Undirected (Graph(..))
import qualified Data.Graph.Undirected as Graph
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           Compiler.CompileErrors
import           Compiler.IRs
-- import qualified MicroRAM.MicroRAM as MRAM




type Registers = [String]

registerAlloc :: Rprog () Word -> Hopefully $ Lprog () VReg Word
registerAlloc = mapM $ registerAllocFunc registers
  where
    numRegisters = 8
    registers = map show [1..numRegisters]


registerAllocFunc :: Registers -> RFunction () Word -> Hopefully $ LFunction () Int Word
registerAllocFunc registers (Function name typ typs blocks') = do
  let blocks = concatMap flattenBasicBlock blocks'

  rtlBlocks <- registerAllocFunc' blocks -- mempty

  return $ error "TODO" rtlBlocks

  -- Unflatten basic block?
      

  where
    registerAllocFunc' blocks = do -- _spilled = do

      liveness <- livenessAnalysis blocks

      let interferenceGraph = computeInterferenceGraph liveness

      -- TODO: we could coalesce

      -- Sort registers by spill cost (lowest cost first).
      let sortedTemporaries = sortTemporaries liveness blocks

      let registerMappingOrSpilled = Graph.color registers sortedTemporaries interferenceGraph

      case registerMappingOrSpilled of
        Left spill ->
        Right coloring ->

      -- return $ Function name typ typs blocks'
      return $ error "TODO"

    -- Sort registers by spill cost (lowest cost first).
    sortTemporaries :: LivenessResult instname -> [block] -> [VReg]
    sortTemporaries liveness _blocks = 
      -- TODO: actually compute a spill cost.
      Set.toList $ Set.unions liveness


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


-- unflattenBasicBlock :: [BB (Name, Int) inst] -> [BB Name inst]


-- If the target instruction is Nothing, it's a return edge.
type LivenessResult instname = Map (instname, Maybe instname) (Set VReg) -- Map VReg [(Loc, Loc)]

livenessAnalysis :: [block] -> Hopefully (LivenessResult name)
livenessAnalysis blocks = error "TODO"


-- -- Returns the set of edges of the interference graph. Edges are tuples between two vertices, where the smaller vertex is first.
-- -- JP: Take spilled vars too?
--
-- Returns the interference graph. 
computeInterferenceGraph :: LivenessResult instname -> Graph VReg () -- Set (VReg, VReg)
computeInterferenceGraph liveness = 
  let edges = Map.foldr (\regs acc -> foldr insertEdge acc $ combinations $ Set.toList regs) mempty liveness in
  Graph.fromEdges $ fmap (\(r1, r2) -> (r1,r2,())) $ Set.toList edges

  where
    insertEdge (r1, r2) acc | r1 > r2 = Set.insert (r2, r1) acc
    insertEdge edge acc               = Set.insert edge acc

    combinations []       = mempty
    combinations (r:regs) = liftA2 (,) (pure r) regs <> combinations regs







-- * Triviall allocation: we provide a pass that erases the code. Usefull for early testing.
-- FIXME: remove this once registerAlloc is implemented and can be tested!

trivialRegisterAlloc :: Rprog () Word -> Hopefully $ Lprog () VReg Word
trivialRegisterAlloc = rtlToLtl

