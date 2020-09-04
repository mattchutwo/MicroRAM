{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : Globals
Description : MARAM -> MARAM
Maintainer  : 
Stability   : 

This pass converts globals into initial memory and replaces all
uses of global variables with the constant pointer to their
location in memory.


-}
module Compiler.Globals
    ( replaceGlobals, 
    ) where

import qualified Data.Map as Map

import Compiler.Common
import Compiler.CompilationUnit
import Compiler.Errors
import Compiler.LazyConstants
import Compiler.IRs
import Compiler.Registers

import MicroRAM

import Util.Util


{- | Lay global variables in memory. Done in two steps:

1. We create an initial memory that contains all th eglobal varibles
   (maybe uninitialised) and a globalMap mapping global names to their
   location in the initla mem.
2. Then we replace all the uses of global variables in the program, with
   the real location of the variable in memory.

-}
replaceGlobals :: Regs mreg =>
        CompilationUnit (Lprog () mreg MWord)
        -> Hopefully $ CompilationUnit (Lprog () mreg MWord)
replaceGlobals (CompUnit prog tr regs aData _ ) = do
  (prog', initMem) <- globals' prog
  return $ CompUnit prog' tr regs aData initMem

globals' :: Regs mreg => Lprog () mreg MWord
         -> Hopefully $ (Lprog () mreg MWord, InitialMem)
globals' (IRprog tenv genv prog) = do
  (initMem, globalMap) <- return $ memoryFromGlobals genv
  prog' <- raplaceGlobals globalMap prog
  return (IRprog tenv genv prog', initMem)

-- | Lazy initial segments are a temporary version of the InitMemSegment
-- that has a lazy initialization value `[LazyConst Name MWord]`, that completly overrides
-- the `content` `:: [MWord]` field (This field should be `Nothing` anyways, but is completly meaningless).
-- After constructing the globals map, these lazy segments are converted to real ones.
type LazyInitSegment = (Maybe [LazyConst Name MWord], InitMemSegment)
type LazyInitialMem = [LazyInitSegment] 


-- * Building initial memory and the `globalMap`
memoryFromGlobals :: GEnv MWord -> (InitialMem, Map.Map Name MWord)
memoryFromGlobals ggg  = 
  let (lazyInitMem, globs) = lazyMemoryFromGlobals ggg in
    (makeConcreteMem globs lazyInitMem, globs)
  where makeConcreteMem :: Map.Map Name MWord -> LazyInitialMem -> InitialMem
        makeConcreteMem globMap lInitMem =
          map (blah globMap) lInitMem
        blah :: Map.Map Name MWord -> LazyInitSegment -> InitMemSegment
        blah g (lazyConst, InitMemSegment secr rOnly loc len _) =
          let concreteInit = map (makeConcreteConst (addDefault g)) <$> lazyConst in
          InitMemSegment secr rOnly loc len concreteInit
        addDefault :: Ord a => Map.Map a MWord -> a -> MWord
        addDefault m a = case Map.lookup a m of
                           Just w -> w
                           Nothing -> 0
        
lazyMemoryFromGlobals :: GEnv MWord -> (LazyInitialMem, Map.Map Name MWord)
lazyMemoryFromGlobals ggg  = foldr memoryFromGlobal ([],Map.empty) ggg  
  where memoryFromGlobal ::
          GlobalVariable MWord
          -> (LazyInitialMem, Map.Map Name MWord)
          -> (LazyInitialMem, Map.Map Name MWord)
        memoryFromGlobal (GlobalVariable name isConst gTy init secret) (initMem, gMap) =
          let newLoc = newLocation initMem in
          let newLazySegment =
                (init, InitMemSegment secret isConst newLoc (fromIntegral $ tySize gTy) Nothing) in -- __FIXME__
          (newLazySegment:initMem, Map.insert name newLoc gMap)
          
        newLocation :: LazyInitialMem -> MWord
        newLocation [] = 0
        newLocation ((_,InitMemSegment _ _ loc len _):_) = loc + len
          

-- * Replace global variables with pointers to the initial memeory
raplaceGlobals ::
  Regs mreg =>
  Map.Map Name MWord
  -> [LFunction mdata mreg MWord]
  -> Hopefully $ [LFunction mdata mreg MWord] 
raplaceGlobals gmap = mapM $ traverseOpLFun $ raplaceGlobalsOperands gmap
  where raplaceGlobalsOperands :: 
          Regs mreg =>
          Map.Map Name MWord
          -> MAOperand mreg MWord
          -> Hopefully $ MAOperand mreg MWord
        raplaceGlobalsOperands gmap (Glob name) =
          case Map.lookup name gmap of
            Just gptr -> return $ LImm gptr
            _ -> assumptError $ "Global not found in the environment: " ++ show name
        raplaceGlobalsOperands _ op = return op
