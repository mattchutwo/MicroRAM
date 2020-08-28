{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Compiler.CallingConvention where

import qualified Data.Set as Set

import           Compiler.Errors
import           Compiler.IRs
import           Compiler.RegisterAlloc.Internal
import           Util.Util

-- | Update functions to conform to the calling convention.
-- Currently uses callee saved registers.
-- This should be run after register allocation.
callingConvention :: Lprog () VReg Word -> Hopefully $ Lprog () VReg Word
callingConvention lprog = do
    let code' = map callingConventionFunc $ code lprog

    return $ lprog {code = code'}



callingConventionFunc :: LFunction () VReg Word -> LFunction () VReg Word
callingConventionFunc lf@(LFunction fname mdata typ typs stackSize []) = lf
callingConventionFunc (LFunction fname mdata typ typs stackSize (firstBlock:blocks)) = 
    -- Get all registers that the function writes to.
    let registers = Set.toList $ Set.unions $ map (\(BB _ insts insts' _) -> Set.unions $ map writeRegisters (insts' ++ insts)) blocks in

    -- Prepend a push (given stack size).
    let firstBlock' = calleeSave stackSize registers firstBlock in
    let blocks' = firstBlock':blocks in

    -- Postpend a pop (given stack size).
    let restoreInsts = calleeRestore stackSize registers in
    let blocks'' = restoreBlocks restoreInsts blocks' in

    -- let blocks' = calleeSave stackSize registers <> blocks <> calleeRestore stackSize registers in

    -- Update stack size.
    let stackSize' = stackSize + fromIntegral (length registers) in

    LFunction fname mdata typ typs stackSize' blocks''
    
  where
    calleeRestore stackSize registers = map (\(pos, reg) -> Lgetstack Local pos ty reg) $ zip [stackSize..] registers

    calleeSave stackSize registers (BB n insts insts' dag) = 
      let saveInsts = map (\(pos, reg) -> 
              IRI (Lsetstack reg Local pos ty) mempty
            ) $ zip [stackSize..] registers
      in
      BB n (saveInsts <> insts) insts' dag

    ty = Tint -- TODO: How do we get the ty?

    restoreBlocks restoreInsts blocks = map (restoreBlock restoreInsts) blocks

    restoreBlock restoreInsts (BB name insts insts' dag) = 
      BB name (concatMap (restoreInst restoreInsts) insts) (concatMap (restoreInst restoreInsts) insts') dag

    restoreInst restoreInsts mri@(MRI _ _) = pure mri
    restoreInst restoreInsts (IRI inst mdata) = IRI <$> restoreLTLInstruction restoreInsts inst <*> pure mdata

    restoreLTLInstruction restoreInsts inst@(LRet mo) = restoreInsts <> pure inst
    restoreLTLInstruction restoreInsts inst@(Lgetstack s w t r1) = pure inst
    restoreLTLInstruction restoreInsts inst@(Lsetstack r1 s w t) = pure inst
    restoreLTLInstruction restoreInsts inst@(LCall t mr op ts ops) = pure inst
    restoreLTLInstruction restoreInsts inst@(LAlloc mr t op) = pure inst
