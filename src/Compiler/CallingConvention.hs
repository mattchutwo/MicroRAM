{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Compiler.CallingConvention where

import qualified Data.Set as Set

import           Compiler.Common
import           Compiler.Errors
import           Compiler.IRs
import           Compiler.Metadata
import           Compiler.RegisterAlloc.Internal
import           Compiler.Registers
import           MicroRAM (MWord)
import qualified MicroRAM as MRAM
import           Util.Util

-- | Update functions to conform to the calling convention.
-- Currently uses callee saved registers.
-- This should be run after register allocation.
callingConvention :: (Regs reg, Ord reg) => Lprog Metadata reg MWord -> Hopefully $ Lprog Metadata reg MWord
callingConvention lprog = do
    let code' = map callingConventionFunc $ code lprog

    return $ lprog {code = code'}


callingConventionFunc :: (Regs reg, Ord reg) => LFunction Metadata reg MWord -> LFunction Metadata reg MWord
callingConventionFunc lf@(LFunction _fname _typ _typs _nms _stackSize []) = lf
callingConventionFunc (LFunction fname typ typs argNames stackSize (firstBlock:blocks)) = 
    -- Get all registers that the function writes to.
    let isMain = dbName fname == "main" in    -- ATTENTION: relies on debugging name!
    let registers = if isMain then
            []
          else
            Set.toList $ Set.unions $ map (\(BB _ insts insts' _) -> Set.unions $ map writeRegisters (insts' ++ insts)) (firstBlock:blocks)
    in

    -- Prepend a push (given stack size).
    let firstBlock' = calleeSave fname stackSize registers firstBlock in
    let blocks' = firstBlock':blocks in

    -- Postpend a pop (given stack size).
    let restoreInsts = calleeRestore stackSize registers in
    let blocks'' = restoreBlocks restoreInsts blocks' in

    -- Update stack size.
    let stackSize' = stackSize + fromIntegral (length registers) in

    LFunction fname typ typs argNames stackSize' blocks''
    
  where
    calleeRestore stkSize registers = zipWith (\pos reg -> Lgetstack Local pos ty reg) [stkSize..] registers

    calleeSave fname stackSize registers (BB n insts insts' dag) = 
      let saveInsts = map (\(pos, reg) -> 
              IRI (Lsetstack reg Local pos ty) (trivialMetadata fname n) 
            ) $ zip [stackSize..] registers
      in
      BB n (saveInsts <> insts) insts' dag

    ty = Tint -- TODO: How do we get the ty?

    restoreBlocks restoreInsts = map (restoreBlock restoreInsts)

    restoreBlock restoreInsts (BB name insts insts' dag) = 
      BB name (concatMap (restoreInst restoreInsts) insts) (concatMap (restoreInst restoreInsts) insts') dag

    restoreInst _restoreInsts mri@(MRI _ _) = pure mri
    restoreInst restoreInsts (IRI inst mdata) = restoreLTLInstruction restoreInsts inst mdata

    restoreLTLInstruction restoreInsts inst@(LRet Nothing) mdata = fmap (\i -> IRI i mdata) restoreInsts <> pure (IRI inst mdata)
    restoreLTLInstruction restoreInsts inst@(LRet (Just retVal)) mdata = [MRI (MRAM.Imov ax retVal) mdata] <> fmap (\i -> IRI i mdata) restoreInsts <> pure (IRI inst mdata)
    restoreLTLInstruction _restoreInsts inst@(Lgetstack _s _w _t _r1) mdata = pure (IRI inst mdata)
    restoreLTLInstruction _restoreInsts inst@(Lsetstack _r1 _s _w _t) mdata = pure (IRI inst mdata)
    restoreLTLInstruction _restoreInsts inst@(LCall _t _mr _op _ts _ops) mdata = pure (IRI inst mdata)
    restoreLTLInstruction _restoreInsts inst@(LAlloc _mr _t _op) mdata = pure (IRI inst mdata)
    restoreLTLInstruction _restoreInsts inst@(LGetBP _r) mdata = pure (IRI inst mdata)
