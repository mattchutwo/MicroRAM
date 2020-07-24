{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Control.Monad.State (runStateT, StateT, get, modify')
import           Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Short as BSS
import           Data.Graph.Undirected (Graph(..))
import qualified Data.Graph.Undirected as Graph
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           Compiler.CompileErrors
import           Compiler.IRs
import           Compiler.RegisterAlloc.Internal
import           Compiler.RegisterAlloc.Liveness
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
    -- Available registers.
    -- First two registers are reserved.
    registers = map NewName [2..numRegisters]

-- Register allocator state.
data RAState = RAState {
    raNextRegister :: Word
  , raNextStackPosition :: Word
  , raNextInstructionForBlock :: Map Name Int
  -- , raRegisterStackPosition :: Map VReg Word
  }

-- Assumes that instructions are in SSA form.
registerAllocFunc :: Registers -> LFunction () VReg Word -> Hopefully $ LFunction () VReg Word
registerAllocFunc registers (LFunction mdata name typ typs stackSize' blocks') = do

  (rtlBlocks, rast) <- flip runStateT (RAState 0 0 mempty) $ do
    blocks <- mapM flattenBasicBlock blocks'

    registerAllocFunc' $ concat blocks
  
  let stackSize = stackSize' + raNextStackPosition rast

  -- Unflatten basic block?
      
  -- return $ Function name typ typs blocks'
  return $ error "TODO" rtlBlocks

  where
    -- registerAllocFunc' :: [BB name inst] -> StateT RAState Hopefully [BB name inst]
    registerAllocFunc' blocks = do -- _spilled = do

      liveness <- lift $ livenessAnalysis blocks

      let interferenceGraph = computeInterferenceGraph liveness

      -- TODO: we could coalesce

      -- Sort registers by spill cost (lowest cost first).
      let sortedTemporaries = sortTemporaries liveness blocks

      let registerMappingOrSpilled = Graph.color registers sortedTemporaries interferenceGraph

      case registerMappingOrSpilled of
        Left spillReg -> do
          -- Get next stack position and increment it.
          pos <- raNextStackPosition <$> get
          modify' $ \(RAState r s m) -> RAState r (s+1) m

          -- Spill register.
          spillRegister spillReg pos blocks

        Right coloring ->
          lift $ applyColoring coloring blocks


    -- Sort registers by spill cost (highest cost first).
    sortTemporaries :: LivenessResult instname -> [block] -> [VReg]
    sortTemporaries liveness _blocks = 
      -- TODO: actually compute a spill cost.
      Set.toList $ Set.unions liveness

spillRegister :: forall name mdata wrdT . (Monoid mdata, name ~ (Name, Int)) => VReg -> Word -> [BB name (LTLInstr mdata VReg wrdT)] -> StateT RAState Hopefully [BB name (LTLInstr mdata VReg wrdT)]
spillRegister spillReg pos blocks = do
  blocks' <- mapM spillBlock blocks
  return $ concat blocks'
  where
    spillBlock :: BB name (LTLInstr mdata VReg wrdT) -> StateT RAState Hopefully [BB name (LTLInstr mdata VReg wrdT)]
    spillBlock (BB (name, iid) insts dag) = do
      insts' <- mapM spillIRInstruction insts
      flatten name iid dag $ concat insts'

    flatten :: Name -> Int -> DAGinfo name -> [LTLInstr mdata VReg wrdT] -> StateT RAState Hopefully [BB name (LTLInstr mdata VReg wrdT)]
    flatten name iid dag [] = return []
    flatten name iid dag [inst] = return [BB (name, iid) [inst] dag]
    flatten name iid dag (inst:insts) = do
      iid' <- getNextInstructionId name
      ((BB (name, iid) [inst] [(name, iid')]):) <$> flatten name iid' dag insts


    -- Assumes SSA.
    spillIRInstruction :: LTLInstr mdata VReg wrdT -> StateT RAState Hopefully [LTLInstr mdata VReg wrdT]
    spillIRInstruction instr | Set.member spillReg (writeRegisters instr) = do
      -- Generate new reg.
      reg <- generateNewRegister

      -- Replace spillReg with reg.
      let instr' = substituteRegisters (Map.singleton spillReg reg) instr

      -- Append push to stack instruction.
      let ty = getTyForRegister spillReg instr
      let push = IRI (Lsetstack reg Local pos ty) mempty

      return [instr', push]

    spillIRInstruction instr | Set.member spillReg (readRegisters instr) = do
      -- Generate new reg.
      reg <- generateNewRegister

      -- Replace spillReg with reg.
      let instr' = substituteRegisters (Map.singleton spillReg reg) instr

      -- Prepend load from stack instruction.
      let ty = getTyForRegister spillReg instr
      let load = IRI (Lgetstack Local pos ty reg) mempty
      
      return [load, instr']

    spillIRInstruction instr = return [instr]

    generateNewRegister = do
      reg <- (\i -> Name $ "_reg_alloc" <> BSS.toShort (BSC.pack $ show $ raNextRegister i)) <$> get
      modify' $ \(RAState c s m) -> RAState (c+1) s m
      return reg

    getTyForRegister reg instr = Tint -- TODO: How do we get the Ty?


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
flattenBasicBlock :: BB Name inst -> StateT RAState Hopefully [BB (Name, Int) inst]
flattenBasicBlock (BB name instrs dag) = flatten instrs
  where
    flatten [] = return []
    flatten [instr] = do
      iid <- getNextInstructionId name
      let dag' = map (, 0) dag
      return [BB (name, iid) [instr] dag']
    flatten (instr:instrs) = do
      iid <- getNextInstructionId name
      (BB (name, iid) [instr] [(name, iid+1)]:) <$> flatten instrs

getNextInstructionId :: Name -> StateT RAState Hopefully Int
getNextInstructionId name = do
  iidm <- (Map.lookup name . raNextInstructionForBlock) <$> get
  let iid = maybe 0 id iidm
  modify' $ \(RAState c p m) -> RAState c p $ Map.insert name (iid+1) m
  return iid
  

-- flattenBasicBlock :: BB Name inst -> [BB (Name, Int) inst]
-- flattenBasicBlock (BB name instrs dag) = flatten $ zip instrs [0..]
--   where
--     flatten [] = []
--     flatten [(instr, iid)] = 
--       let dag' = map (, 0) dag in
--       [BB (name, iid) [instr] dag']
--     flatten ((instr, iid):instrs) = BB (name, iid) [instr] [(name, iid+1)]:flatten instrs


-- unflattenBasicBlock :: [BB (Name, Int) inst] -> [BB Name inst]



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

