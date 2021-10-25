{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Removing Labels
Description : Replaces labels with concrete instruction numbers : MARAM -> MRAM
Maintainer  : santiago@galois.com
Stability   : experimental


This module compiles Translates MicroASM to MicroRAM.

MicroASM is different to MicrRAM in that it allows the operands
`Label` and `HereLabel`. The assembler will replace those labels
with the actual instruction numbers to obtain MicroRAM. In particular
a MicroASM program can be "partial" and needs to be linked to another part
that contains some of the labels (this allows some simple separta compilation.

Note: This module is completly parametric over the machine register type.

The assembler translates all `Label` and `HereLabel` to the actual
instruction number to produce well formed MicroRAM. It does so in three passes:
 
1) Create a label map, mapping names -> instruction

2) "Flatten": Removing the names from blocks, leaving a list of instructions

3) Replace all labels with the location given in the label map.

TODO: It can all be done in 2 passes. Optimize?


-}
module Compiler.RemoveLabels
    ( removeLabels,
      stashGlobals
    ) where


import Control.Monad

import MicroRAM
import Compiler.IRs
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V

import Compiler.Common
import Compiler.CompilationUnit
import Compiler.Errors
import Compiler.LazyConstants
import Compiler.Tainted
import Compiler.Layout (alignTo)

import Debug.Trace


type Wrd = MWord

-- * Assembler

blocksStart :: MWord
blocksStart = 0

blockSize :: NamedBlock md regT MWord -> MWord
blockSize (NBlock _ instrs) = fromIntegral $ length instrs

globalsStart :: MWord
globalsStart = 1 * fromIntegral wordBytes

nextGlobalAddr :: MWord -> GlobalVariable MWord -> MWord
nextGlobalAddr addr g =
  alignTo (fromIntegral wordBytes * gAlign g) $
    addr + fromIntegral wordBytes * gSize g

buildLabelMap ::
  [NamedBlock md regT MWord] ->
  [GlobalVariable MWord] ->
  Hopefully (Map Name MWord)
buildLabelMap blocks globs = do
  blockMap <- goBlocks mempty blocksStart blocks
  globMap <- goGlobs mempty globalsStart globs
  let overlap = Set.intersection (Map.keysSet blockMap) (Map.keysSet globMap)
  when (not $ Set.null overlap) $
    assumptError $ "name collision between blocks and globals: " ++ show overlap
  return $ blockMap <> globMap
  where
    goBlocks m _addr [] = return m
    goBlocks m addr (b@(NBlock (Just name) _) : bs) = do
      when (Map.member name m) $
        assumptError $ "name collision between blocks: " ++ show name
      goBlocks (Map.insert name addr m) (addr + blockSize b) bs
    goBlocks m addr (b@(NBlock Nothing _) : bs) = do
      trace "warning: unnamed block in RemoveLabels" $
        goBlocks m (addr + blockSize b) bs

    goGlobs m _addr [] = return m
    goGlobs m addr (g:gs) = do
      let name = globName g
      when (Map.member name m) $
        assumptError $ "name collision between globals: " ++ show name
      goGlobs (Map.insert name addr m) (nextGlobalAddr addr g) gs

getOrZero :: Map Name MWord -> Name -> MWord
getOrZero m n = case Map.lookup n m of
  Nothing -> trace ("warning: label " ++ show n ++ " is missing; defaulting to zero") 0
  Just x -> x

flattenBlocks ::
  Map Name MWord ->
  [NamedBlock md regT MWord] ->
  Hopefully [(Instruction regT MWord, md)]
flattenBlocks lm bs = goBlocks blocksStart bs
  where
    goBlocks ::
      MWord -> [NamedBlock md regT MWord] -> Hopefully [(Instruction regT MWord, md)]
    goBlocks _addr [] = return []
    goBlocks addr (b@(NBlock _ instrs) : bs) = do
      instrs' <- goInstrs addr instrs
      rest <- goBlocks (addr + blockSize b) bs
      return $ instrs' ++ rest

    goInstrs ::
      MWord -> [(MAInstruction regT MWord, md)] -> Hopefully [(Instruction regT MWord, md)]
    goInstrs _addr [] = return []
    goInstrs addr ((i, md) : rest) = do
      i' <- traverse (goOperand addr) i
      rest' <- goInstrs (addr + 1) rest
      return $ (i', md) : rest'

    goOperand :: MWord -> MAOperand regT MWord -> Hopefully (Operand regT MWord)
    goOperand _ (AReg r) = return $ Reg r
    goOperand _ (LImm lc) = return $ Const $ makeConcreteConst lmFunc lc
    goOperand _ (Label name) = return $ Const $ lmFunc name
    goOperand addr HereLabel = return $ Const addr

    lmFunc = getOrZero lm

flattenGlobals ::
  Bool ->
  Map Name MWord ->
  [GlobalVariable MWord] ->
  Hopefully [InitMemSegment]
flattenGlobals tainted lm gs = goGlobals globalsStart gs
  where
    goGlobals :: MWord -> [GlobalVariable MWord] -> Hopefully [InitMemSegment]
    goGlobals _addr [] = return []
    goGlobals addr (g:gs) = do
      init' <- mapM (mapM goLazyConst) (initializer g)
      let seg = InitMemSegment {
            isName   = dbName $ globName g
            isSecret = secret g,
            isReadOnly = isConstant g,
            isHeapInit = gvHeapInit g,
            location = addr `div` fromIntegral wordBytes,
            segmentLen = gSize g,
            content = init',
            labels = if tainted then
                Just $ replicate (fromIntegral $ gSize g) $ V.replicate wordBytes untainted
              else Nothing
            }
      rest <- goGlobals (nextGlobalAddr addr g) gs
      return $ seg : rest

    goLazyConst :: LazyConst MWord -> Hopefully MWord
    goLazyConst lc = return $ makeConcreteConst lmFunc lc

    lmFunc = getOrZero lm

-- ** Remove labels from the entire CompilationUnit  
removeLabels ::
  Bool ->
  CompilationUnit [GlobalVariable MWord] (MAProgram md regT Wrd) ->
  Hopefully (CompilationUnit () (AnnotatedProgram md regT Wrd))
removeLabels tainted cu = do
  let blocks = pmProg $ programCU cu
  let globs = intermediateInfo cu
  lm <- buildLabelMap blocks globs
  prog <- flattenBlocks lm blocks
  mem <- flattenGlobals tainted lm globs
  return $ cu {
    programCU = ProgAndMem prog mem lm,
    intermediateInfo = ()
  }

-- FIXME: This is a temporary hack.  Instead MAProgram should contain both a
-- list of blocks and a list of globals, and Stacking should copy the global
-- env into the right field of the MAProgram.
stashGlobals ::
  CompilationUnit () (Lprog m mreg MWord) ->
  CompilationUnit [GlobalVariable MWord] (Lprog m mreg MWord)
stashGlobals cu = cu { intermediateInfo = gs }
  where gs = globals $ pmProg $ programCU cu
