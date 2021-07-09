{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
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
      removeLabelsProg
    ) where



import MicroRAM
import Compiler.IRs
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vec

import Util.Util

import Compiler.Common
import Compiler.CompilationUnit
import Compiler.Errors
import Compiler.LazyConstants
import Compiler.Tainted

-- * Assembler

type Wrd = MWord

naturals :: [Wrd]
naturals = iterate (1 +) 1


-- ** Create a label map

type LabelMap = Map.Map Name Wrd
getLabel :: LabelMap -> Name -> Hopefully Wrd
getLabel lmap lbl =
  case Map.lookup lbl lmap of
    Just w -> Right w
    Nothing -> assumptError $ "Code label called but not defined. Label" ++ (show lbl)

data State = State { pc :: Wrd, lMap :: LabelMap}

initState :: State
initState = State 0 Map.empty

-- | createMapStep
-- Updates the state given one block 
createMapStep :: State -> NamedBlock md regT wrdT -> State
createMapStep st (NBlock name code) =
  State ( pc st + (fromIntegral $ length code)) (update oldMap name)
  where location = pc st
        oldMap = lMap st
        update lm (Just nm) | Map.member nm lm =
          error $ "duplicate definition of label " ++ show nm
        update lm (Just nm) = Map.insert nm location lm
        update lm _ = lm

createMap :: MAProgram md regT wrdT -> LabelMap
createMap prog = lMap $ foldl createMapStep initState prog

-- ** Flaten the program
-- Now that the labels have been computed we can remove the labes
-- to get a "flat" list of (MicroAssembly) instructions 

flattenStep ::
  [(MAInstruction regT wrdT, md)]
  -> NamedBlock md regT wrdT
  -> [(MAInstruction regT wrdT, md)]
flattenStep prog (NBlock _ code) = prog ++ code
  
flatten :: MAProgram md regT wrdT -> [(MAInstruction regT wrdT,md)]
flatten maProg = foldl flattenStep [] maProg

-- ** Translate label
-- Given the label map, we can replace all labels
-- with the correct code location

-- | Translate the operands
-- This takes the current instruction to translate 'HereLabel'
translateOperand :: LabelMap -> Wrd -> MAOperand regT Wrd -> Hopefully (Operand regT Wrd)
translateOperand _ _ (AReg r) = return $  Reg r
translateOperand _ _ (LImm (SConst sc)) =  return $  Const sc
translateOperand labelMap _ (LImm lc) =
  return $ Const $ makeConcreteConst fullMap lc
  where fullMap = addDefault labelMap
  --assumptError $ "There should be no lazy constants at this point. Found a Lazy Constant LConst. \n"
translateOperand _ _ (Glob g) =  assumptError $ "There should be no globals at this point. Found \n" ++
                                 "Glob " ++ show g
translateOperand lmap _ (Label lbl) = do
  location <- getLabel lmap lbl 
  return $  Const location
translateOperand _ loc (HereLabel) = return $  Const loc

translatePair:: Monad m =>
  (w -> a -> m b) ->
  ((Instruction' regT regT a, md), w) ->
  m (Instruction' regT regT b, md)
translatePair f (inst_md,w) = mapFstM (mapM $ f w) inst_md

translateProgram:: Monad m =>
  (w -> a -> m b) ->
  [((Instruction' regT regT a, md), w)] ->
  m [(Instruction' regT regT b, md)]
translateProgram f = mapM (translatePair f)


-- Reoplaces labels with the real value.
-- In the future it will ALSO replace lazy constants with immediates.
-- (Remember we stillneed "HERE LABEL", which cannot (should not) be replaced by lazy constants)
replaceLabels:: LabelMap -> [(MAInstruction regT Wrd, md)] -> Hopefully $ AnnotatedProgram md regT Wrd
replaceLabels lm amProg = translateProgram (translateOperand lm) numberedProg
  where numberedProg = zip amProg naturals -- naturals == [0..]?


-- ** Assemble : put all steps together

-- Old way of doing it.
removeLabelsProg :: MAProgram md regT Wrd -> Hopefully $ AnnotatedProgram md regT Wrd
removeLabelsProg massProg = replaceLabels lMap flatProg
  where lMap     = createMap massProg
        flatProg = flatten massProg

removeLabelsInitMem :: LabelMap -> LazyInitialMem -> Hopefully $ InitialMem
removeLabelsInitMem lmap lInitMem =
  let fullMap = addDefault lmap in
    mapM (removeLabelsSegment fullMap) lInitMem
  where removeLabelsSegment :: (Name -> Wrd) -> LazyInitSegment -> Hopefully $ InitMemSegment
        removeLabelsSegment labelMap (lMem, initSegment) =
          let vals = removeLabelInitialValues labelMap lMem in
          return $ initSegment {content = vals, labels = fmap (const $ replicate (fromIntegral $ segmentLen initSegment) $ Vec.replicate wordBytes untainted) vals}
        removeLabelInitialValues :: (Name -> Wrd) -> Maybe [LazyConst Name Wrd] -> Maybe [Wrd]
        removeLabelInitialValues labelMap lMem =  map (makeConcreteConst labelMap) <$> lMem
addDefault :: LabelMap -> Name -> Wrd
addDefault labelMap name =
  case Map.lookup name labelMap of
    Just x -> x
    Nothing -> 0  -- Adds a default. Checking undefined names and labels should be done at Instruction Selection.
                  -- So, here we asssume all funcitons are in the map and the default will never be returned.
      

-- ** Remove labels from the entire CompilationUnit  
removeLabels :: (CompilationUnit LazyInitialMem (MAProgram md regT Wrd))
             -> Hopefully $ CompilationUnit () (AnnotatedProgram md regT Wrd)
removeLabels compUnit = do
  let lMap = createMap $ pmProg $ programCU compUnit
  prog' <- replaceLabels lMap $ flatten $ pmProg $ programCU compUnit
  initMem <- removeLabelsInitMem lMap $ intermediateInfo compUnit
  return $ compUnit {programCU = ProgAndMem prog' initMem, intermediateInfo = ()}
  
