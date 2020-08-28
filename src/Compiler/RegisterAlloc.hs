{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Register allocation
Description : Register allocation
Maintainer  : 
Stability   : 


Register allocation

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

import           Data.Default
import           Data.Graph.Undirected (Graph)
import qualified Data.Graph.Undirected as Graph
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           Compiler.Errors
import           Compiler.IRs
import           Compiler.RegisterAlloc.Internal
import           Compiler.RegisterAlloc.Liveness
import           MicroRAM (MWord)
import qualified MicroRAM as MRAM
import           Util.Util

data RegisterAllocOptions = RegisterAllocOptions {
    _registerAllocNumRegisters :: Word
  }

instance Default RegisterAllocOptions where
  def = RegisterAllocOptions 8

type Registers = [VReg]



registerAlloc :: RegisterAllocOptions -> Rprog () MWord -> Hopefully $ Lprog () VReg MWord
registerAlloc (RegisterAllocOptions numRegisters) rprog = do
  -- Convert to ltl.
  lprog <- rtlToLtl rprog

  -- JP: Load arguments from stack? 
  -- Replace `Name "0"` with `Lgetstack Incoming 0 _ _`, ...
  let code' = map initializeFunctionArgs $ code lprog

  -- Run register allocation.
  code <- mapM (registerAllocFunc registers) code'

  return $ lprog {code = code}

  where
    -- Available registers.
    -- First two registers are reserved.
    registers = map NewName [2..numRegisters-1]

-- Register allocator state.
data RAState = RAState {
    raNextRegister :: Word
  , raNextStackPosition :: Word
  , raNextInstructionForBlock :: Map Name Int
  -- , raRegisterStackPosition :: Map VReg Word
  }

-- Initialize function arguments according to the calling convention.
-- Currently, this loads arguments from the stack with `Lgetstack Incoming 0 _ (Name "0")`.
initializeFunctionArgs :: LFunction () VReg MWord -> LFunction () VReg MWord
initializeFunctionArgs (LFunction fname mdata typ typs stackSize blocks) = 
    let b = BB bname insts [] daginfo in
    LFunction fname mdata typ typs stackSize $ b:blocks
  where
    insts = map (\(typ, i) -> 
        let inst = Lgetstack Incoming i typ (Name $ wordToBSS i) in
        IRI inst mempty
      ) $ zip typs [0..]

    bname = Name $ BSS.toShort $ BSC.pack (fname <> "_args")

    daginfo = case blocks of
      ((BB name _ _ _):_) -> [name]
      _ -> []

    -- wordToBSS = BSS.pack . pure . (+48) -- c2w . intToDigit
    wordToBSS = BSS.toShort . BSC.pack . show -- TODO: Double check this.


-- Assumes that instructions are in SSA form.
registerAllocFunc :: Registers -> LFunction () VReg MWord -> Hopefully $ LFunction () VReg MWord
registerAllocFunc registers (LFunction mdata name typ typs stackSize' blocks') = do

  (rtlBlocks, rast) <- flip runStateT (RAState 0 0 mempty) $ do
    blocks <- mapM flattenBasicBlock blocks'

    registerAllocFunc' $ concat blocks
  
  let stackSize = stackSize' + raNextStackPosition rast

  -- Unflatten basic block?
  let blocks = unflattenBasicBlock rtlBlocks
      
  -- return $ Function name typ typs blocks'
  return $ LFunction mdata name typ typs stackSize blocks

  where
    -- -- These are for arguments that are passed through registers.
    -- -- If we change our calling convention, this will need to be updated appropriately. 
    -- argRegisters =
    --     let numArgs = length typs in
    --     Set.fromList $ map (Name . BSS.pack . pure . c2w . intToDigit) [0..(numArgs-1)]

    extractRegisters :: BB name (LTLInstr mdata VReg wrdT) -> Set VReg
    extractRegisters (BB _ insts insts' _) = Set.unions $ map (\i -> readRegisters i <> writeRegisters i) (insts' ++ insts)


    -- registerAllocFunc' :: [BB name inst] -> StateT RAState Hopefully [BB name inst]
    registerAllocFunc' blocks = do -- _spilled = do
      let allRegisters = Set.toList $ Set.unions $ map extractRegisters blocks

      liveness <- lift $ livenessAnalysis blocks

      let interferenceGraph = computeInterferenceGraph liveness allRegisters -- argRegisters

      -- TODO: we could coalesce

      -- -- Sort registers by spill cost (lowest cost first).
      -- let sortedTemporaries = sortTemporaries liveness blocks

      let registerMappingOrSpilled = Graph.color registers interferenceGraph

      case registerMappingOrSpilled of
        Left spillReg -> do
          -- Get next stack position and increment it.
          pos <- raNextStackPosition <$> get
          modify' $ \(RAState r s m) -> RAState r (s+1) m

          -- Spill register.
          let isArg = False -- Set.member spillReg argRegisters
          blocks' <- spillRegister spillReg isArg pos blocks

          -- Try again after spilling.
          registerAllocFunc' blocks'

        Right coloring ->
          lift $ applyColoring coloring blocks


    -- -- Sort registers by spill cost (highest cost first).
    -- -- sortTemporaries :: LivenessResult instname -> [block] -> [VReg]
    -- sortTemporaries _liveness blocks = 
    --   -- TODO: actually compute a spill cost.
    --   -- Set.toList $ Set.unions liveness
    --   Set.toList $ Set.unions $ map (\(BB _ insts insts' _) -> 
    --       Set.unions $ map (\i -> Set.union (readRegisters i) (writeRegisters i)) $ insts ++ insts'
    --     ) blocks



spillRegister :: forall name mdata wrdT . (Monoid mdata, name ~ (Name, Int)) => VReg -> Bool -> Word -> [BB name (LTLInstr mdata VReg wrdT)] -> StateT RAState Hopefully [BB name (LTLInstr mdata VReg wrdT)]
spillRegister spillReg isArg pos blocks = do
  blocks' <- mapM spillBlock blocks
  return $ concat blocks'
  where
    spillBlock :: BB name (LTLInstr mdata VReg wrdT) -> StateT RAState Hopefully [BB name (LTLInstr mdata VReg wrdT)]
    spillBlock (BB (name, iid) insts tInsts dag) = do
      insts' <- concat <$> mapM spillIRInstruction insts
      tInsts' <- concat <$> mapM spillIRInstruction tInsts

      -- If the spilled register is an argument, prepend a push to the stack.
      let insts'' = if isArg then  
              let ty = getTyForRegister spillReg Nothing in
              let push = IRI (Lsetstack spillReg Local pos ty) mempty in
              push:insts'
            else
              insts'
      flatten name iid dag insts'' tInsts'

    flatten :: Name -> Int -> DAGinfo name -> [LTLInstr mdata VReg wrdT] -> [LTLInstr mdata VReg wrdT] -> StateT RAState Hopefully [BB name (LTLInstr mdata VReg wrdT)]
    flatten _name _iid _dag [] [] = return []
    flatten name iid dag [] [inst] = do
      return [BB (name, iid) [] [inst] dag]
    flatten name iid dag [] (inst':insts') = do
      iid' <- getNextInstructionId name
      ((BB (name, iid) [] [inst'] [(name, iid')]):) <$> flatten name iid' dag [] insts'
    flatten name iid dag [inst] [] = return [BB (name, iid) [inst] [] dag]
    flatten name iid dag (inst:insts) insts' = do
      iid' <- getNextInstructionId name
      ((BB (name, iid) [inst] [] [(name, iid')]):) <$> flatten name iid' dag insts insts'


    -- Assumes SSA.
    spillIRInstruction :: LTLInstr mdata VReg wrdT -> StateT RAState Hopefully [LTLInstr mdata VReg wrdT]
    spillIRInstruction instr | Set.member spillReg (writeRegisters instr) = do
      -- Generate new reg.
      reg <- generateNewRegister

      -- Replace spillReg with reg.
      let instr' = substituteRegisters (Map.singleton spillReg reg) instr

      -- Append push to stack instruction.
      let ty = getTyForRegister spillReg $ Just instr
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

    getTyForRegister _reg _instr = Tint -- TODO: How do we get the Ty?


-- JP: lens/uniplate would make this easier.
applyColoring :: Map VReg VReg -> [BB name (LTLInstr mdata VReg wrdT)] -> Hopefully [BB name (LTLInstr mdata VReg wrdT)]
applyColoring coloring = mapM applyBasicBlock
  where
    applyBasicBlock :: BB name (LTLInstr mdata VReg wrdT) -> Hopefully (BB name (LTLInstr mdata VReg wrdT))
    applyBasicBlock (BB name insts insts' dag) = BB name <$> mapM applyIRInstruction insts <*> mapM applyIRInstruction insts' <*> pure dag

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
    applyOperand (MRAM.Glob s) = return $ MRAM.Glob s
    applyOperand MRAM.HereLabel = return $ MRAM.HereLabel


-- Each returned basic block will have one instruction. 
-- Instruction identifiers are not guaranteed to be in order.
-- The first instruction will always have identifier 0.
flattenBasicBlock :: BB Name inst -> StateT RAState Hopefully [BB (Name, Int) inst]
flattenBasicBlock (BB name instrs instrs' dag) = flatten instrs instrs'
  where
    flatten [] [] = return []
    flatten [] [instr] = do
      iid <- getNextInstructionId name
      let dag' = map (, 0) dag
      return [BB (name, iid) [] [instr] dag']
    flatten [] (instr':instrs') = do
      iid <- getNextInstructionId name
      (BB (name, iid) [] [instr'] [(name, iid+1)]:) <$> flatten [] instrs'
    flatten [instr] [] = do
      iid <- getNextInstructionId name
      let dag' = map (, 0) dag
      return [BB (name, iid) [instr] [] dag']
    flatten (instr:instrs) instrs' = do
      iid <- getNextInstructionId name
      (BB (name, iid) [instr] [] [(name, iid+1)]:) <$> flatten instrs instrs'

    -- flatten [] = return []
    -- flatten [instr] = do
    --   iid <- getNextInstructionId name
    --   let dag' = map (, 0) dag
    --   return [BB (name, iid) [instr] dag']
    -- flatten (instr:instrs) = do
    --   iid <- getNextInstructionId name
    --   (BB (name, iid) [instr] [(name, iid+1)]:) <$> flatten instrs

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


unflattenBasicBlock :: [BB (Name, Int) inst] -> [BB Name inst]
unflattenBasicBlock bbs' = 
  let bbs = NonEmpty.groupWith (\(BB (name,_) _ _ _) -> name) bbs' in
  map (\bbs ->
      let (BB (name, _) _ _ dag') = NonEmpty.last bbs in
      let dag = map fst dag' in
      let (insts, insts') = unzip $ map (\(BB _ insts insts' _) -> (insts, insts')) $ NonEmpty.toList bbs in

      BB name (concat insts) (concat insts') dag
    ) bbs


-- -- Returns the set of edges of the interference graph. Edges are tuples between two vertices, where the smaller vertex is first.
-- -- JP: Take spilled vars too?
--
-- Returns the interference graph. 
computeInterferenceGraph :: LivenessResult instname -> [VReg] -> Graph VReg () -- Set (VReg, VReg)
computeInterferenceGraph liveness allRegs = -- argRegs = 
  -- let cs = argRegs : Map.elems liveness in
  let edges = Map.foldr (\regs acc -> foldr insertEdge acc $ combinations $ Set.toList regs) mempty liveness in
  let g = Graph.fromEdges $ fmap (\(r1, r2) -> (r1,r2,())) $ Set.toList edges in
  foldr (\reg g -> Graph.insertVertex reg g) g allRegs

  where
    insertEdge (r1, r2) acc | r1 > r2 = Set.insert (r2, r1) acc
    insertEdge edge acc               = Set.insert edge acc

    combinations []       = mempty
    combinations (r:regs) = liftA2 (,) (pure r) regs <> combinations regs







-- * Triviall allocation: we provide a pass that erases the code. Usefull for early testing.
-- FIXME: remove this once registerAlloc is implemented and can be tested!

trivialRegisterAlloc :: Rprog () MWord -> Hopefully $ Lprog () VReg MWord
trivialRegisterAlloc = rtlToLtl




-- -- * TRIVIAL PHI REMOVIAL
-- -- ALL OF THIS SHOULD GO ONCE THE REAL REGISTER ALLOCATION IS READY
-- -- TODO: delete everything bellow this.
-- 
-- {- | Moves all `Phi`s back to the other block as Imov instruction
-- Example:
-- If you have `RPhi r2 (r1, block0)` we remove that instruction and add the following at the end of  `block0`: `Imove r2 r1`.
-- 
-- It works in two steps:
-- 1.  Remove all the phi's and store the info of what register needs to move where, in what block.
-- 2. Add, at the end of every block, the necessary move instructions calculated in the last step.
-- 
-- -}
-- -- | Map2 is a mpa that takes two keys and returns one value
-- type Map2 t1 t2 t3 = Map.Map t1 (Map.Map t2 t3) 
-- 
-- replacePhi :: Rprog () MWord -> Rprog () MWord
-- replacePhi prog = addPhiMoves $ abstractPhi prog
-- 
-- -- | removes all occurrences of Phi, and stores the move information in a map
-- -- (Name,Name) represents the name of the function and the name of the block.
-- type AbstractPhiProgram = (Rprog () MWord, Map2 Name Name [(VReg, MRAM.MAOperand VReg MWord)])
-- abstractPhi :: Rprog () MWord -> AbstractPhiProgram
-- abstractPhi (IRprog tenv globs code) =
--   let (phiMap, code') = abstractPhiCode code in
--     (IRprog tenv globs code', phiMap)
-- 
--   where abstractPhiCode code = foldr abstractPhiFunc (Map.empty, []) code
--  
--         abstractPhiFunc (Function name ret args code) (phiMap, funcs) =
--           let (phiMapF, code') = foldr abstractPhiBlock (Map.empty, [])  code in
--             (Map.insert name phiMapF phiMap, Function name ret args code' : funcs)
-- 
--         abstractPhiBlock (BB name code term dagd) (phiMap, blocks) =
--           let (phiMapB, code') = abstractPhiBody code in
--             (Map.unionWith (++) phiMapB phiMap, BB name code' term dagd : blocks)
-- 
--         abstractPhiBody (IRI (RPhi vreg phiBacks) mdata: codeTl) =
--           let (phiMapB, code') = abstractPhiBody codeTl in
--             (Map.unionWith (++) (turnPhis vreg phiBacks) phiMapB, code') 
--         abstractPhiBody code = (Map.empty,code)
-- 
--         turnPhis vreg phiBacks = foldr (turnPhi vreg) Map.empty phiBacks
--         turnPhi vreg (val, label) phiMap =
--           Map.insertWith (++) label [(vreg,val)] phiMap 
--         
-- 
--           
-- -- | Adds all the abstract PHi information as `Imov` instructions
-- addPhiMoves :: AbstractPhiProgram -> Rprog () MWord
-- addPhiMoves (prog, phiMap) = prog {code = map (addPhiFunc phiMap) $ code prog}
--   where addPhiFunc phiMap (Function name ret args code) =
--           Function name ret args (map (addPhiBlock (Map.lookup name phiMap)) code)
-- 
--         addPhiBlock ::
--           (Maybe $ Map.Map Name [(VReg, MRAM.MAOperand VReg MWord)])
--           -> BB Name $ RTLInstr () MWord
--           -> BB Name $ RTLInstr () MWord
--         addPhiBlock Nothing block = block
--         addPhiBlock (Just phiMap) (BB name code term dagd) =
--           (BB name (code ++ (phiMap2Instructions $ Map.lookup name phiMap)) term dagd)
-- 
--         phiMap2Instructions ::
--           Maybe [(VReg, MRAM.MAOperand VReg MWord)]
--           -> [RTLInstr () MWord]
--         phiMap2Instructions Nothing = []
--         phiMap2Instructions (Just ls) = map phiPair2instr ls
--         
--         phiPair2instr (reg, op) = MRI (MRAM.Imov reg op) () 
--         

