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

1. We create an initial memory that contains all the global varibles
   (maybe uninitialised) and a globalMap mapping global names to their
   location in the initla mem. This is memory still has lazy constants
   since code labels haven't been resolved.
2. Then we replace all the uses of global variables in the program, with
   the real location of the variable in memory. -- This part will soon be
   made obsolete by lazy constants.
   We also partially resolve the lazy variables using the locations of
   global variables.
   

-}
replaceGlobals :: Regs mreg =>
        CompilationUnit () (Lprog () mreg MWord)
        -> Hopefully $ CompilationUnit LazyInitialMem (Lprog () mreg MWord)
replaceGlobals (CompUnit prog tr regs aData _ _) = do
  (prog', initMem) <- globals' prog
  return $ CompUnit prog' tr regs aData [] initMem 

globals' :: Regs mreg => Lprog () mreg MWord
         -> Hopefully $ (Lprog () mreg MWord, LazyInitialMem)
globals' (IRprog tenv genv prog) = do
  (initMem, globalMap) <- return $ memoryFromGlobals genv
  prog' <- raplaceGlobals globalMap prog
  return (IRprog tenv genv prog', initMem)

-- | Lazy initial segments are a temporary version of the InitMemSegment
-- that has a lazy initialization value `[LazyConst String MWord]`, that completly overrides
-- the `content` `:: [MWord]` field (This field should be `Nothing` anyways, but is completly meaningless).
-- After constructing the globals map, these lazy segments are converted to real ones.

-- * Building initial memory and the `globalMap`
memoryFromGlobals :: GEnv MWord -> (LazyInitialMem, Map.Map String MWord)
memoryFromGlobals ggg  = 
  let (lazyInitMem, globs) = lazyMemoryFromGlobals ggg in
    (resolveGlobalsMem globs lazyInitMem, globs)
  where resolveGlobalsMem :: Map.Map String MWord -> LazyInitialMem -> LazyInitialMem
        resolveGlobalsMem globMap lInitMem = 
          map (resolveGlobalsSegment globMap) lInitMem
        resolveGlobalsSegment :: Map.Map String MWord -> LazyInitSegment -> LazyInitSegment
        resolveGlobalsSegment g (lazyConst, InitMemSegment secr rOnly loc len _) =
          let concreteInit = map (applyPartialMap g) <$> lazyConst in
          (concreteInit, InitMemSegment secr rOnly loc len Nothing)
        
lazyMemoryFromGlobals :: GEnv MWord -> (LazyInitialMem, Map.Map String MWord)
lazyMemoryFromGlobals ggg  = foldr memoryFromGlobal ([],Map.empty) ggg  
  where memoryFromGlobal ::
          GlobalVariable MWord
          -> (LazyInitialMem, Map.Map String MWord)
          -> (LazyInitialMem, Map.Map String MWord)
        memoryFromGlobal (GlobalVariable name isConst gTy init secret) (initMem, gMap) =
          let newLoc = newLocation initMem in
          let newLazySegment =
                (init, InitMemSegment secret isConst newLoc (fromIntegral $ tySize gTy) Nothing) in -- __FIXME__
          (newLazySegment:initMem, Map.insert (show name) newLoc gMap)
          
        newLocation :: LazyInitialMem -> MWord
        newLocation [] = 0
        newLocation ((_,InitMemSegment _ _ loc len _):_) = loc + len
          

-- * Replace global variables with pointers to the initial memeory
raplaceGlobals ::
  Regs mreg =>
  Map.Map String MWord
  -> [LFunction mdata mreg MWord]
  -> Hopefully $ [LFunction mdata mreg MWord] 
raplaceGlobals gmap = mapM $ traverseOpLFun $ raplaceGlobalsOperands gmap
  where raplaceGlobalsOperands :: 
          Regs mreg =>
          Map.Map String MWord
          -> MAOperand mreg MWord
          -> Hopefully $ MAOperand mreg MWord
        raplaceGlobalsOperands gmap (Glob name) =  -- FIXME : SC I think we can remove Glob as it is covered by lazy constants 
          case Map.lookup (show name) gmap of
            Just gptr -> return $ LImm $ SConst gptr
            _ -> assumptError $ "Global not found in the environment: " ++ show name
        raplaceGlobalsOperands gmap (LImm lc) =
          return $ LImm $ applyPartialMap gmap lc
        raplaceGlobalsOperands _ op = return op
