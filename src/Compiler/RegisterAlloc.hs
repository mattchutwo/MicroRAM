{-# LANGUAGE GADTs #-}
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
import qualified MicroRAM.MicroRAM as MRAM



type Registers = [VReg]

registerAlloc :: Rprog () Word -> Hopefully $ Lprog () VReg Word
registerAlloc rprog = do
  -- Convert to ltl.
  lprog <- rtlToLtl rprog

  -- JP: Load arguments from stack? 
  -- Replace `Name "0"` with `Lgetstack Incoming 0 _ _`, ...

  -- Run register allocation.
  code <- mapM (registerAllocFunc registers) $ irProgCode lprog

  return $ lprog {irProgCode = code}


  where
    numRegisters = 8
    -- First two registers are reserved.
    registers = map NewName [2..numRegisters]


-- registerAllocFunc :: Registers -> RFunction () Word -> Hopefully $ LFunction () Int Word
-- registerAllocFunc registers (Function name typ typs blocks') = do

registerAllocFunc :: Registers -> LFunction () VReg Word -> Hopefully $ LFunction () VReg Word
registerAllocFunc registers (LFunction mdata name typ typs stackSize blocks') = do
  let blocks = concatMap flattenBasicBlock blocks'

  rtlBlocks <- registerAllocFunc' blocks -- mempty

  -- Unflatten basic block?
      
  -- return $ Function name typ typs blocks'
  return $ error "TODO" rtlBlocks

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
          error "TODO"
        Right coloring ->
          applyColoring coloring blocks


    -- Sort registers by spill cost (lowest cost first).
    sortTemporaries :: LivenessResult instname -> [block] -> [VReg]
    sortTemporaries liveness _blocks = 
      -- TODO: actually compute a spill cost.
      Set.toList $ Set.unions liveness

-- JP: lens/uniplate would make this easier.
applyColoring :: Map VReg VReg -> [BB name (LTLInstr mdata VReg wrdT)] -> Hopefully [BB name (LTLInstr mdata VReg wrdT)]
applyColoring coloring = mapM applyBasicBlock
  where
    applyBasicBlock :: BB name (LTLInstr mdata VReg wrdT) -> Hopefully (BB name (LTLInstr mdata VReg wrdT))
    applyBasicBlock (BB name insts dag) = BB name <$> mapM applyIRInstruction insts <*> pure dag

    applyIRInstruction :: LTLInstr mdata VReg wrdT -> Hopefully (LTLInstr mdata VReg wrdT)
    applyIRInstruction (MRI inst mdata) = MRI <$> applyMRIInstruction inst <*> pure mdata
    applyIRInstruction (IRI inst mdata) = IRI <$> applyLTLInstruction inst <*> pure mdata

    applyLTLInstruction :: LTLInstr' VReg mdata (MRAM.MAOperand VReg wrdT) -> Hopefully (LTLInstr' VReg mdata (MRAM.MAOperand VReg wrdT))
    applyLTLInstruction (Lgetstack s w t r1) = Lgetstack s w t <$> applyVReg r1
    applyLTLInstruction (Lsetstack r1 s w t) = Lsetstack <$> applyVReg r1 <*> pure s <*> pure w <*> pure t
    applyLTLInstruction (LCall t mr op ts ops) = LCall t <$> mr' <*> applyOperand op <*> pure ts <*> mapM applyOperand ops
      where mr' = maybe (pure Nothing) (\r -> Just <$> applyVReg r) mr
    applyLTLInstruction (LRet mo) = LRet <$> maybe (pure Nothing) (\o -> Just <$> applyOperand o) mo
    applyLTLInstruction (LAlloc mr t op) = LAlloc <$> mr' <*> pure t <*> applyOperand op
      where mr' = maybe (pure Nothing) (\r -> Just <$> applyVReg r) mr

    applyMRIInstruction :: MRAM.MAInstruction VReg wrdT -> Hopefully (MRAM.MAInstruction VReg wrdT)
    applyMRIInstruction (MRAM.Iand r1 r2 op) = MRAM.Iand <$> applyVReg r1 <*> applyVReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Ior r1 r2 op) = MRAM.Ior <$> applyVReg r1 <*> applyVReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Ixor r1 r2 op) = MRAM.Ixor <$> applyVReg r1 <*> applyVReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Inot r1 op) = MRAM.Inot <$> applyVReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Iadd r1 r2 op) = MRAM.Iadd <$> applyVReg r1 <*> applyVReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Isub r1 r2 op) = MRAM.Isub <$> applyVReg r1 <*> applyVReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Imull r1 r2 op) = MRAM.Imull <$> applyVReg r1 <*> applyVReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Iumulh r1 r2 op) = MRAM.Iumulh <$> applyVReg r1 <*> applyVReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Ismulh r1 r2 op) = MRAM.Ismulh <$> applyVReg r1 <*> applyVReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Iudiv r1 r2 op) = MRAM.Iudiv <$> applyVReg r1 <*> applyVReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Iumod r1 r2 op) = MRAM.Iumod <$> applyVReg r1 <*> applyVReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Ishl r1 r2 op) = MRAM.Ishl <$> applyVReg r1 <*> applyVReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Ishr r1 r2 op) = MRAM.Ishr <$> applyVReg r1 <*> applyVReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Icmpe r1 op) = MRAM.Icmpe <$> applyVReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Icmpa r1 op) = MRAM.Icmpa <$> applyVReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Icmpae r1 op) = MRAM.Icmpae <$> applyVReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Icmpg r1 op) = MRAM.Icmpg <$> applyVReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Icmpge r1 op) = MRAM.Icmpge <$> applyVReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Imov r1 op) = MRAM.Imov <$> applyVReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Icmov r1 op) = MRAM.Icmov <$> applyVReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Ijmp op) = MRAM.Ijmp <$> applyOperand op
    applyMRIInstruction (MRAM.Icjmp op) = MRAM.Icjmp <$> applyOperand op
    applyMRIInstruction (MRAM.Icnjmp op) = MRAM.Icnjmp <$> applyOperand op
    applyMRIInstruction (MRAM.Istore op r1) = MRAM.Istore <$> applyOperand op <*> applyVReg r1
    applyMRIInstruction (MRAM.Iload r1 op) = MRAM.Iload <$> applyVReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Iread r1 op) = MRAM.Iread <$> applyVReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Ianswer op) = MRAM.Ianswer <$> applyOperand op

    applyVReg r | Just r' <- Map.lookup r coloring = return r'
    applyVReg r                                    = otherError $ "Unknown register assignment for: " <> show r

    applyOperand :: MRAM.MAOperand VReg wrdT -> Hopefully (MRAM.MAOperand VReg wrdT)
    applyOperand (MRAM.Reg r)   = MRAM.Reg <$> applyVReg r
    applyOperand (MRAM.Const w) = return $ MRAM.Const w
    applyOperand (MRAM.Label s) = return $ MRAM.Label s
    applyOperand MRAM.HereLabel = return $ MRAM.HereLabel


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

