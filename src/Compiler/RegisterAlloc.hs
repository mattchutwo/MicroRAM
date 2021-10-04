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
    ( registerAlloc
    , AReg
    , RegisterAllocOptions(..)
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

import           Compiler.Common
import           Compiler.CompilationUnit
import           Compiler.IRs
import           Compiler.Metadata
import           Compiler.RegisterAlloc.Internal
import           Compiler.RegisterAlloc.Liveness
import           Compiler.Registers
import           MicroRAM (MWord)
import qualified MicroRAM as MRAM
import           Util.Util

data RegisterAllocOptions = RegisterAllocOptions {
    registerAllocNumRegisters :: Word
  }

instance Default RegisterAllocOptions where
  def = RegisterAllocOptions 9

type AReg = Int
type Registers = [AReg]

registerAlloc :: Bool
              -> RegisterAllocOptions
              -> CompilationUnit a (Lprog Metadata VReg MWord)
              -> Hopefully $ CompilationUnit a (Lprog Metadata AReg MWord)
registerAlloc True                    _   comp = skipRegisterAlloc comp
registerAlloc _skipRegisterAllocation opt comp = do
  let regData = NumRegisters $ fromEnum numRegisters
  lprog   <- registerAllocProg (pmProg $ programCU comp)
  return $ comp {programCU = (programCU comp) { pmProg = lprog }, regData = regData}
  -- ^ This pass creates *temporary* new names for registers and then colors them over.
  -- no new names persists after the pass. Otherwise, we would have to update
  -- `nameBound` in the compilation unit
  where
    -- Spill registers will be created after this bound
    -- and thats how we recognise them. 
    spillBound = nameBound comp
    
    registerAllocProg :: Lprog Metadata VReg MWord
                  -> Hopefully $ (Lprog Metadata AReg MWord)
    registerAllocProg lprog = do
      let code' = code lprog
      
      -- Run register allocation.
      code <- mapM (registerAllocFunc spillBound registers) code'

      return $ setCode lprog code

      
    -- Available registers.
    numRegisters = registerAllocNumRegisters opt
    registers :: Registers
    registers = [firstAvailableRegister..(fromIntegral numRegisters)-1]

-- First three registers are reserved.
firstAvailableRegister :: Int
firstAvailableRegister = 3


setCode :: Lprog Metadata reg0 MWord -> [LFunction Metadata reg MWord] -> Lprog Metadata reg MWord
setCode (IRprog tenv globals _) code = IRprog tenv globals code

-- Register allocator state.
data RAState = RAState {
    raNextRegister :: Word -- To be used only for registers (not for blocks or functions)
  , raNextStackPosition :: MWord
  , raNextInstructionForBlock :: Map Name Int
  -- , raRegisterStackPosition :: Map VReg Word
  }


registerAllocFunc :: forall wrd . Word -> Registers -> LFunction Metadata VReg wrd -> Hopefully $ LFunction Metadata AReg wrd
registerAllocFunc spillBound registers (LFunction name typ typs argNms stackSize' blocks') = do

  (rtlBlocks, rast) <- flip runStateT (RAState spillBound 0 mempty) $ do
    blocks <- mapM flattenBasicBlock blocks'

    registerAllocFunc' name $ concat blocks
  
  let stackSize = stackSize' + raNextStackPosition rast

  -- Unflatten basic block
  let blocks = unflattenBasicBlock rtlBlocks
      
  return $ LFunction name typ typs argNms stackSize blocks

  where
    -- -- These are for arguments that are passed through registers.
    -- -- If we change our calling convention, this will need to be updated appropriately. 
    -- argRegisters =
    --     let numArgs = length typs in
    --     Set.fromList $ map (Name . BSS.pack . pure . c2w . intToDigit) [0..(numArgs-1)]


    registerAllocFunc' :: Name -> [BB (Name, Int) (LTLInstr Metadata VReg wrdT)] -> StateT RAState Hopefully [BB (Name, Int) (LTLInstr Metadata AReg wrdT)]
    registerAllocFunc' fName blocks = do -- _spilled = do
      let allRegisters = Set.toList $ Set.unions $ map extractRegisters blocks

      liveness <- lift $ livenessAnalysis blocks

      let interferenceGraph = computeInterferenceGraph liveness allRegisters -- argRegisters

      -- TODO: we could coalesce

      -- -- Sort registers by spill cost (lowest cost first).
      -- let sortedTemporaries = sortTemporaries liveness blocks

      let weightFunc n = 
            -- Map from nodes to number of edges they're present in for liveness.
            let weightMap = foldr (\s m -> foldr (\r m -> Map.insertWith (+) r 1 m) m s) mempty liveness in
            Map.findWithDefault 0 n weightMap

      let registerMappingOrSpilled = Graph.color registers weightFunc canSpill interferenceGraph

      case registerMappingOrSpilled of
        Left spillReg -> do
          -- Get next stack position and increment it.
          pos <- raNextStackPosition <$> get
          modify' $ \(RAState r s m) -> RAState r (s+1) m

          -- Spill register.
          let isArg = False -- Set.member spillReg argRegisters
          blocks' <- spillRegister fName spillReg isArg pos blocks

          -- Try again after spilling.
          registerAllocFunc' fName blocks'

        Right coloring ->
          lift $ applyColoring coloring blocks


    -- | Checks if the register was temporarily created for a spill
    -- All old registers are bounded by `spillbound`. Anything above that is
    -- a spill.
    -- TODO: Is there a better way to mark new regs? The old way was marking the
    -- name of the register with: "_reg_alloc" 
    -- ``` "_reg_alloc" `BSC.isPrefixOf` BSS.fromShort n ``` 
    canSpill = not . isSpillReg
    isSpillReg (Name n _) = n >= spillBound 

    -- -- Sort registers by spill cost (highest cost first).
    -- -- sortTemporaries :: LivenessResult instname -> [block] -> [VReg]
    -- sortTemporaries _liveness blocks = 
    --   -- TODO: actually compute a spill cost.
    --   -- Set.toList $ Set.unions liveness
    --   Set.toList $ Set.unions $ map (\(BB _ insts insts' _) -> 
    --       Set.unions $ map (\i -> Set.union (readRegisters i) (writeRegisters i)) $ insts ++ insts'
    --     ) blocks

extractRegisters :: Ord reg => BB name (LTLInstr Metadata reg wrdT) -> Set reg
extractRegisters (BB _ insts insts' _) = Set.unions $ map (\i -> readRegisters i <> writeRegisters i) (insts' ++ insts)



spillRegister :: forall name wrdT . (name ~ (Name, Int)) => Name -> VReg -> Bool -> MWord -> [BB name (LTLInstr Metadata VReg wrdT)] -> StateT RAState Hopefully [BB name (LTLInstr Metadata VReg wrdT)]
spillRegister fName spillReg isArg pos blocks = do
  blocks' <- mapM (spillBlock fName)  blocks
  return $ concat blocks'
  where
    spillBlock :: Name -> BB name (LTLInstr Metadata VReg wrdT) -> StateT RAState Hopefully [BB name (LTLInstr Metadata VReg wrdT)]
    spillBlock fName (BB (name, iid) insts tInsts dag) = do
      let md = trivialMetadata fName name 
      insts' <- concat <$> mapM (spillIRInstruction md) insts
      tInsts' <- concat <$> mapM (spillIRInstruction md) tInsts

      -- If the spilled register is an argument, prepend a push to the stack.
      let insts'' = if isArg then  
              let ty = getTyForRegister spillReg Nothing in
              let push = IRI (Lsetstack spillReg Local pos ty) md in
              push:insts'
            else
              insts'
      flatten name iid dag insts'' tInsts'

    flatten :: Name -> Int -> DAGinfo name -> [LTLInstr Metadata VReg wrdT] -> [LTLInstr Metadata VReg wrdT] -> StateT RAState Hopefully [BB name (LTLInstr Metadata VReg wrdT)]
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

    spillIRInstruction md instr
      | containsReadRegs || containsWriteRegs = do
        -- Create a new temporary register to store the value stored/loaded from
        -- the stack.
        reg <- generateNewRegister
        -- Replace the spilled register with the new temporary.
        --
        -- We could potentially do better by using separate temporaries for the
        -- input and output sides, but that only applies to instructions that
        -- read and write the same virtual register, which should be fairly rare.
        let instr' = substituteRegisters (Map.singleton spillReg reg) instr
        -- Add stack access before/after if needed.
        let ty = getTyForRegister spillReg $ Just instr
        let load = IRI (Lgetstack Local pos ty reg) md
        let store = IRI (Lsetstack reg Local pos ty) md
        return $
          (if containsReadRegs then [load] else []) ++
          [instr'] ++
          (if containsWriteRegs then [store] else [])

      | otherwise =
        return [instr]

        where
          containsReadRegs = Set.member spillReg $ readRegisters instr
          containsWriteRegs = Set.member spillReg $ writeRegisters instr


    generateNewRegister = do
      newName <- raNextRegister <$> get
      modify' $ \(RAState c s m) -> RAState (c+1) s m
      return $ Name newName $ "_reg_alloc" <> BSS.toShort (BSC.pack $ show $ newName)
      
    getTyForRegister _reg _instr = Tint -- TODO: How do we get the Ty?


-- JP: lens/uniplate would make this easier.
applyColoring :: forall reg0 reg name wrdT . (Ord reg0, Show reg0) => Map reg0 reg -> [BB name (LTLInstr Metadata reg0 wrdT)] -> Hopefully [BB name (LTLInstr Metadata reg wrdT)]
applyColoring coloring = mapM applyBasicBlock
  where
    applyBasicBlock :: BB name (LTLInstr Metadata reg0 wrdT) -> Hopefully (BB name (LTLInstr Metadata reg wrdT))
    applyBasicBlock (BB name insts insts' dag) = BB name <$> mapM applyIRInstruction insts <*> mapM applyIRInstruction insts' <*> pure dag

    applyIRInstruction :: LTLInstr Metadata reg0 wrdT -> Hopefully (LTLInstr Metadata reg wrdT)
    applyIRInstruction (MRI inst mdata) = MRI <$> applyMRIInstruction inst <*> pure mdata
    applyIRInstruction (IRI inst mdata) = IRI <$> applyLTLInstruction inst <*> pure mdata

    -- applyLTLInstruction :: LTLInstr' reg0 mdata (MAOperand reg0 wrdT) -> Hopefully (LTLInstr' reg mdata (MAOperand reg wrdT))
    applyLTLInstruction (Lgetstack s w t r1) = Lgetstack s w t <$> applyReg r1
    applyLTLInstruction (Lsetstack r1 s w t) = Lsetstack <$> applyReg r1 <*> pure s <*> pure w <*> pure t
    applyLTLInstruction (LCall t mr op ts ops) = LCall t <$> mr' <*> applyOperand op <*> pure ts <*> mapM applyOperand ops
      where mr' = maybe (pure Nothing) (\r -> Just <$> applyReg r) mr
    applyLTLInstruction (LRet mo) = LRet <$> maybe (pure Nothing) (\o -> Just <$> applyOperand o) mo
    applyLTLInstruction (LAlloc mr t op) = LAlloc <$> mr' <*> pure t <*> applyOperand op
      where mr' = maybe (pure Nothing) (\r -> Just <$> applyReg r) mr
    applyLTLInstruction (LGetBP r) = LGetBP <$> applyReg r

    applyMRIInstruction :: MAInstruction reg0 wrdT -> Hopefully (MAInstruction reg wrdT)
    applyMRIInstruction instr = MRAM.mapInstrM applyReg applyReg applyOperand instr
    {-applyMRIInstruction (MRAM.Iand r1 r2 op) = MRAM.Iand <$> applyReg r1 <*> applyReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Ior r1 r2 op) = MRAM.Ior <$> applyReg r1 <*> applyReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Ixor r1 r2 op) = MRAM.Ixor <$> applyReg r1 <*> applyReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Inot r1 op) = MRAM.Inot <$> applyReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Iadd r1 r2 op) = MRAM.Iadd <$> applyReg r1 <*> applyReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Isub r1 r2 op) = MRAM.Isub <$> applyReg r1 <*> applyReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Imull r1 r2 op) = MRAM.Imull <$> applyReg r1 <*> applyReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Iumulh r1 r2 op) = MRAM.Iumulh <$> applyReg r1 <*> applyReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Ismulh r1 r2 op) = MRAM.Ismulh <$> applyReg r1 <*> applyReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Iudiv r1 r2 op) = MRAM.Iudiv <$> applyReg r1 <*> applyReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Iumod r1 r2 op) = MRAM.Iumod <$> applyReg r1 <*> applyReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Ishl r1 r2 op) = MRAM.Ishl <$> applyReg r1 <*> applyReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Ishr r1 r2 op) = MRAM.Ishr <$> applyReg r1 <*> applyReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Icmpe r1 op) = MRAM.Icmpe <$> applyReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Icmpa r1 op) = MRAM.Icmpa <$> applyReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Icmpae r1 op) = MRAM.Icmpae <$> applyReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Icmpg r1 op) = MRAM.Icmpg <$> applyReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Icmpge r1 op) = MRAM.Icmpge <$> applyReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Imov r1 op) = MRAM.Imov <$> applyReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Icmov r1 op) = MRAM.Icmov <$> applyReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Ijmp op) = MRAM.Ijmp <$> applyOperand op
    applyMRIInstruction (MRAM.Icjmp op) = MRAM.Icjmp <$> applyOperand op
    applyMRIInstruction (MRAM.Icnjmp op) = MRAM.Icnjmp <$> applyOperand op
    applyMRIInstruction (MRAM.Istore w op r1) = MRAM.Istore w <$> applyOperand op <*> applyReg r1
    applyMRIInstruction (MRAM.Iload w r1 op) = MRAM.Iload w <$> applyReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Iread r1 op) = MRAM.Iread <$> applyReg r1 <*> applyOperand op
    applyMRIInstruction (MRAM.Ianswer op) = MRAM.Ianswer <$> applyOperand op
    applyMRIInstruction (MRAM.Ipoison w op r1) = MRAM.Ipoison w <$> applyOperand op <*> applyReg r1
    applyMRIInstruction (MRAM.Iadvise r1) = MRAM.Iadvise <$> applyReg r1
    applyMRIInstruction (MRAM.Itaint r2 op) = MRAM.Itaint <$> applyReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Isink r2 op) = MRAM.Isink <$> applyReg r2 <*> applyOperand op
    applyMRIInstruction (MRAM.Iext name ops) = MRAM.Iext name <$> mapM applyOperand ops
    applyMRIInstruction (MRAM.Iextval name rd ops) = MRAM.Iextval name <$> applyReg rd <*> mapM applyOperand ops
    applyMRIInstruction (MRAM.Iextadvise name rd ops) = MRAM.Iextadvise name <$> applyReg rd <*> mapM applyOperand ops -}

    applyReg :: reg0 -> Hopefully reg
    applyReg r | Just r' <- Map.lookup r coloring = return r'
    applyReg r                                    = otherError $ "Unknown register assignment for: " <> show r

    applyOperand :: MAOperand reg0 wrdT -> Hopefully (MAOperand reg wrdT)
    applyOperand (AReg r)   = AReg <$> applyReg r
    applyOperand (LImm w) = return $ LImm w
    applyOperand (Label s) = return $ Label s
    applyOperand (Glob s) = return $ Glob s
    applyOperand HereLabel = return $ HereLabel


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
computeInterferenceGraph :: Ord reg => LivenessResult instname reg -> [reg] -> Graph reg () -- Set (VReg, VReg)
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




skipRegisterAlloc :: CompilationUnit a (Lprog Metadata VReg MWord)
                  -> Hopefully (CompilationUnit a (Lprog Metadata AReg MWord))
skipRegisterAlloc comp = do
  (lprog, registerC) <- skipRegisterAllocProg $ pmProg $ programCU comp
  return $ comp {programCU = (programCU comp) { pmProg = lprog }, regData = NumRegisters registerC}

  where
    skipRegisterAllocProg :: Lprog Metadata VReg MWord -> Hopefully (Lprog Metadata AReg MWord, Int)
    skipRegisterAllocProg lprog = do
      (code', registerCounts) <- fmap unzip $ mapM skipRegisterAllocFunc $ code lprog
      return (setCode lprog code', foldr1 max registerCounts)

    skipRegisterAllocFunc :: LFunction Metadata VReg wrd -> Hopefully (LFunction Metadata AReg wrd, Int)
    skipRegisterAllocFunc (LFunction name typ typs argNms stackSize' blocks') = do
      let allRegisters = Set.toList $ Set.unions $ map extractRegisters blocks'
      let coloring = Map.fromList $ zip allRegisters [firstAvailableRegister..]
      blocks <- applyColoring coloring blocks'
      return (LFunction name typ typs argNms stackSize' blocks, firstAvailableRegister + Map.size coloring)



