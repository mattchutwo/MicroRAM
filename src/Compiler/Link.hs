{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Compiler.Link
Description : MicroASM linker
Maintainer  : spernsteiner@galois.com
Stability   : prototype

Linking of MicroASM compilation units.

-}

module Compiler.Link
( CompUnit
, linkCompUnits, concatCompUnits
) where

import           Control.Monad
import           Control.Monad.Writer
import           Data.Default
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           Compiler.Analysis (appendAnalysisData, renameAnalysisData)
import           Compiler.Common
import           Compiler.CompilationUnit
import           Compiler.Errors
import           Compiler.IRs
import           Compiler.LazyConstants
import           Compiler.Metadata
import           Compiler.Registers (RegisterData(..))
import           Compiler.RegisterAlloc (AReg)
import           MicroRAM


type CompUnit = CompilationUnit [GlobalVariable MWord] (MAProgram Metadata AReg MWord)

-- | Link together several comp_units.  This adjusts certain `Name`s so that
-- comp_units can refer to external symbols (blocks and globals) defined in other
-- comp_units.
linkCompUnits :: [CompUnit] -> Hopefully CompUnit
linkCompUnits objs = do
  let undefNames = [cuUsedNames obj Set.\\ cuDefinedNames obj | obj <- objs]
  let externNames = [cuExternNames obj | obj <- objs]

  -- Map string names to Names for all extern symbols
  -- `builtinNames` are defined by built-in compiler magic.
  let builtinNames = [pcName]
  externNameMap <- foldM (\m name -> do
    let nameStr = dbName name
    when (Map.member nameStr m) $
      otherError $ "multiple definitions of extern symbol " ++ show nameStr
    return $ Map.insert nameStr name m) mempty
      ([name | names <- externNames, name <- Set.toList names] ++ builtinNames)

  -- Check for undefined names that won't be resolved by linking to other
  -- compilation units.
  let globalUndefNames = [name | names <- undefNames, name <- Set.toList names,
        not $ Map.member (dbName name) externNameMap]
  when (not $ null globalUndefNames) $
    otherError $ "undefined symbols: " ++ show globalUndefNames

  -- Map undefined Names to the Name of the matching definition
  let undefNameMap :: Map Name Name
      undefNameMap = Map.fromList [(name, externNameMap Map.! dbName name)
        | names <- undefNames, name <- Set.toList names]
  let nameFunc :: Name -> Name
      nameFunc name = maybe name id $ Map.lookup name undefNameMap

  let objs' = map (renameCompUnit nameFunc) objs
  concatCompUnits objs'

-- | Concatenate several comp_units.  This makes no changes to `Name`s, so symbols
-- defined in one comp_unit won't be visible in the others.
concatCompUnits :: [CompUnit] -> Hopefully CompUnit
concatCompUnits objs = return $ foldl appendCompUnits initObj objs
  where initObj = CompUnit
          { programCU = ProgAndMem
              { pmProg = []
              , pmMem = []
              , pmLabels = mempty
              }
          , traceLen = 0
          , regData = NumRegisters 0
          , aData = def
          , nameBound = firstUnusedName
          , intermediateInfo = []
          }


appendCompUnits :: CompUnit -> CompUnit -> CompUnit
appendCompUnits obj1 obj2 = CompUnit
  { programCU = appendProgAndMem (programCU obj1) (programCU obj2)
  , traceLen = max (traceLen obj1) (traceLen obj2)
  , regData = case (regData obj1, regData obj2) of
    (InfinityRegs, _) -> InfinityRegs
    (_, InfinityRegs) -> InfinityRegs
    (NumRegisters n1, NumRegisters n2) -> NumRegisters (max n1 n2)
  , aData = appendAnalysisData (aData obj1) (aData obj2)
  , nameBound = max (nameBound obj1) (nameBound obj2)
  , intermediateInfo = intermediateInfo obj1 ++ intermediateInfo obj2
  }

appendProgAndMem :: ProgAndMem (MAProgram Metadata AReg MWord)
                 -> ProgAndMem (MAProgram Metadata AReg MWord)
                 -> ProgAndMem (MAProgram Metadata AReg MWord)
appendProgAndMem pm1 pm2 = ProgAndMem
  { pmProg = pmProg pm1 ++ pmProg pm2
  , pmMem = pmMem pm1 ++ pmMem pm2
  , pmLabels = pmLabels pm1 <> pmLabels pm2
  }


cuDefinedNames :: CompUnit -> Set Name
cuDefinedNames obj = execWriter (goCompUnit obj)
  where emit x = tell (Set.singleton x)
        emitSet xs = tell xs

        goCompUnit obj = do
          mapM_ goBlock (pmProg $ programCU obj)
          -- pmMem should be empty at this point - global variables are still
          -- in `GlobalVariable` form, rather than `InitialMem`.
          mapM_ goGlobalVariable (intermediateInfo obj)

        goBlock blk = mapM_ emit (blockName blk)

        goGlobalVariable gv = emitSet $ Set.fromList [name | (name, _, _) <- entryPoints gv]

cuExternNames :: CompUnit -> Set Name
cuExternNames obj = execWriter (goCompUnit obj)
  where emit x = tell (Set.singleton x)
        emitSet xs = tell xs

        goCompUnit obj = do
          mapM_ goBlock (pmProg $ programCU obj)
          -- pmMem should be empty at this point - global variables are still
          -- in `GlobalVariable` form, rather than `InitialMem`.
          mapM_ goGlobalVariable (intermediateInfo obj)

        goBlock blk
          | blockExtern blk = mapM_ emit (blockName blk)
          | otherwise = return ()

        goGlobalVariable gv = emitSet $ Set.fromList [name | (name, _, True) <- entryPoints gv]

cuUsedNames :: CompUnit -> Set Name
cuUsedNames obj = execWriter (goCompUnit obj)
  where emit x = tell (Set.singleton x)
        emitSet xs = tell xs

        goCompUnit obj = do
          mapM_ goBlock (pmProg $ programCU obj)
          mapM_ goGlobalVariable (intermediateInfo obj)

        goBlock blk = do
          sequence_ [goInstr instr | (instr, _) <- blockInstrs blk]

        goInstr instr = do
          traverseInstr (\_ -> return ()) (\_ -> return ()) goOperand instr

        goOperand (AReg _) = return ()
        goOperand (LImm lc) = goLazyConst lc
        goOperand (Label name) = emit name

        goLazyConst (LConst _ names) = emitSet names
        goLazyConst (SConst _) = return ()

        goGlobalVariable gv = do
          mapM_ (mapM_ goLazyConst) (initializer gv)


renameCompUnit :: (Name -> Name) -> CompUnit -> CompUnit
renameCompUnit f (CompUnit prog len regs analysis nextName gvs) =
  CompUnit
    (renameProgAndMem f prog)
    len
    regs
    (renameAnalysisData f analysis)
    nextName
    (map (renameGlobalVariable f) gvs)

renameProgAndMem :: (Name -> Name)
                 -> ProgAndMem (MAProgram Metadata AReg MWord)
                 -> ProgAndMem (MAProgram Metadata AReg MWord)
renameProgAndMem f (ProgAndMem prog mem labels) =
  ProgAndMem
    (map (renameBlock f) prog)
    mem
    (renameLabelsMap f labels)

renameLabelsMap :: (Name -> Name)
                -> Map Name MWord
                -> Map Name MWord
renameLabelsMap f ls = Map.fromList [(f name, val) | (name, val) <- Map.toList ls]

renameBlock :: (Name -> Name)
            -> NamedBlock Metadata AReg MWord
            -> NamedBlock Metadata AReg MWord
renameBlock f (NamedBlock name instrs secret priv extern) =
  NamedBlock
    (fmap f name)
    [(renameInstr f instr, renameMetadata f md) | (instr, md) <- instrs]
    secret
    priv
    extern
  
renameInstr :: (Name -> Name)
            -> MAInstruction AReg MWord
            -> MAInstruction AReg MWord
renameInstr f instr = mapInstr id id (renameOperand f) instr

renameGlobalVariable :: (Name -> Name)
                     -> GlobalVariable MWord
                     -> GlobalVariable MWord
renameGlobalVariable f (GlobalVariable name entryPoints const init size align secret heapInit) =
  GlobalVariable
    (f name)
    [(f name, offset, extern) | (name, offset, extern) <- entryPoints]
    const
    (fmap (map (renameLazyConst f)) init)
    size
    align
    secret
    heapInit

renameOperand :: (Name -> Name)
                -> MAOperand AReg MWord
                -> MAOperand AReg MWord
renameOperand f (AReg r) = AReg r
renameOperand f (LImm lc) = LImm (renameLazyConst f lc)
renameOperand f (Label name) = Label (f name)

renameLazyConst :: (Name -> Name)
                -> LazyConst MWord
                -> LazyConst MWord
renameLazyConst f (LConst calc names) =
  LConst
    (\getGlobal -> calc (\name -> getGlobal (f name)))
    (Set.fromList [f name | name <- Set.toList names])
renameLazyConst f (SConst x) = SConst x
