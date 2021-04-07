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
    , trivialRegisterAlloc -- FIXME : remove when reg alloc completed
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

registerAlloc :: RegisterAllocOptions
              -> CompilationUnit a (Rprog Metadata MWord)
              -> Hopefully $ CompilationUnit a (Lprog Metadata AReg MWord)
registerAlloc opt comp = do
  regData <- return $ NumRegisters $ fromEnum $ numRegisters
  lprog   <- registerAllocProg (programCU comp)
  return $ comp {programCU = lprog, regData = regData}
  where
    registerAllocProg :: Rprog Metadata MWord
                  -> Hopefully $ Lprog Metadata AReg MWord
    registerAllocProg rprog = do
      -- Convert to ltl.
      lprog <- rtlToLtl rprog

      -- JP: Load arguments from stack? 
      -- Replace `Name "0"` with `Lgetstack Incoming 0 _ _`, ...
      let code' = map initializeFunctionArgs $ code lprog

      -- Run register allocation.
      code <- mapM (registerAllocFunc registers) code'

      return $ setCode lprog code

      
    -- Available registers.
    -- First three registers are reserved.
    numRegisters = registerAllocNumRegisters opt
    registers :: Registers
    registers = [3..(fromIntegral numRegisters)-1]

    setCode :: Lprog Metadata reg0 MWord -> [LFunction Metadata reg MWord] -> Lprog Metadata reg MWord
    setCode (IRprog tenv globals _) code = IRprog tenv globals code

-- Register allocator state.
data RAState = RAState {
    raNextRegister :: Word
  , raNextStackPosition :: MWord
  , raNextInstructionForBlock :: Map Name Int
  -- , raRegisterStackPosition :: Map VReg Word
  }

-- Initialize function arguments according to the calling convention.
-- Currently, this loads arguments from the stack with `Lgetstack Incoming 0 _ (Name "0")`.
initializeFunctionArgs :: LFunction Metadata VReg MWord -> LFunction Metadata VReg MWord
initializeFunctionArgs (LFunction fname typ typs stackSize blocks) = 
    let b = BB bname insts [] daginfo in
    LFunction fname typ typs stackSize $ b:blocks
  where
    insts = map (\(typ, i) -> 
        let inst = Lgetstack Incoming i typ (Name $ wordToBSS i)
            md = trivialMetadata fname (show bname) in
        IRI inst md 
      ) $ zip typs [0..]

    bname = Name $ BSS.toShort $ BSC.pack (fname <> "_args")

    daginfo = case blocks of
      ((BB name _ _ _):_) -> [name]
      _ -> []

    -- wordToBSS = BSS.pack . pure . (+48) -- c2w . intToDigit
    wordToBSS = BSS.toShort . BSC.pack . show -- TODO: Double check this.


registerAllocFunc :: forall wrd . Registers -> LFunction Metadata VReg wrd -> Hopefully $ LFunction Metadata AReg wrd
registerAllocFunc registers (LFunction name typ typs stackSize' blocks') = do

  (rtlBlocks, rast) <- flip runStateT (RAState 0 0 mempty) $ do
    blocks <- mapM flattenBasicBlock blocks'

    registerAllocFunc' name $ concat blocks
  
  let stackSize = stackSize' + raNextStackPosition rast

  -- Unflatten basic block
  let blocks = unflattenBasicBlock rtlBlocks
      
  return $ LFunction name typ typs stackSize blocks

  where
    -- -- These are for arguments that are passed through registers.
    -- -- If we change our calling convention, this will need to be updated appropriately. 
    -- argRegisters =
    --     let numArgs = length typs in
    --     Set.fromList $ map (Name . BSS.pack . pure . c2w . intToDigit) [0..(numArgs-1)]

    extractRegisters :: Ord reg => BB name (LTLInstr Metadata reg wrdT) -> Set reg
    extractRegisters (BB _ insts insts' _) = Set.unions $ map (\i -> readRegisters i <> writeRegisters i) (insts' ++ insts)


    registerAllocFunc' :: String -> [BB (Name, Int) (LTLInstr Metadata VReg wrdT)] -> StateT RAState Hopefully [BB (Name, Int) (LTLInstr Metadata AReg wrdT)]
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

      let registerMappingOrSpilled = Graph.color registers weightFunc (not . isSpillReg) interferenceGraph

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

    isSpillReg (Name n) = "_reg_alloc" `BSC.isPrefixOf` BSS.fromShort n
    isSpillReg _ = False


    -- -- Sort registers by spill cost (highest cost first).
    -- -- sortTemporaries :: LivenessResult instname -> [block] -> [VReg]
    -- sortTemporaries _liveness blocks = 
    --   -- TODO: actually compute a spill cost.
    --   -- Set.toList $ Set.unions liveness
    --   Set.toList $ Set.unions $ map (\(BB _ insts insts' _) -> 
    --       Set.unions $ map (\i -> Set.union (readRegisters i) (writeRegisters i)) $ insts ++ insts'
    --     ) blocks



spillRegister :: forall name wrdT . (name ~ (Name, Int)) => String -> VReg -> Bool -> MWord -> [BB name (LTLInstr Metadata VReg wrdT)] -> StateT RAState Hopefully [BB name (LTLInstr Metadata VReg wrdT)]
spillRegister fName spillReg isArg pos blocks = do
  blocks' <- mapM (spillBlock fName)  blocks
  return $ concat blocks'
  where
    spillBlock :: String -> BB name (LTLInstr Metadata VReg wrdT) -> StateT RAState Hopefully [BB name (LTLInstr Metadata VReg wrdT)]
    spillBlock fName (BB (name, iid) insts tInsts dag) = do
      let md = trivialMetadata fName (show name) 
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
      reg <- (\i -> Name $ "_reg_alloc" <> BSS.toShort (BSC.pack $ show $ raNextRegister i)) <$> get
      modify' $ \(RAState c s m) -> RAState (c+1) s m
      return reg

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







-- * Triviall allocation: we provide a pass that erases the code. Usefull for early testing.
-- FIXME: remove this once registerAlloc is implemented and can be tested!

trivialRegisterAlloc :: Rprog String MWord -> Hopefully $ Lprog String VReg MWord
trivialRegisterAlloc = rtlToLtl
