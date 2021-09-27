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

import Data.Bits
import qualified Data.Map as Map
import qualified Data.Vector as V

import Compiler.Common
--import Compiler.Layout
import Compiler.CompilationUnit
import Compiler.Errors
import Compiler.LazyConstants
import Compiler.IRs
import Compiler.Registers
import Compiler.Tainted (untainted)

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
        Bool ->
        CompilationUnit () (Lprog m mreg MWord) ->
        Hopefully $ CompilationUnit LazyInitialMem (Lprog m mreg MWord)
replaceGlobals tainted (CompUnit pm tr regs anData nmBound _) = do
  (prog', initMem) <- globals' tainted $ pmProg pm
  return $ CompUnit (pm { pmProg = prog' }) tr regs anData nmBound initMem 

globals' :: Regs mreg => Bool -> Lprog m mreg MWord
         -> Hopefully $ (Lprog m mreg MWord, LazyInitialMem)
globals' tainted (IRprog tenv genv prog) = do
  (initMem, globalMap) <- return $ memoryFromGlobals tainted genv
  prog' <- raplaceGlobals globalMap prog
  return (IRprog tenv genv prog', initMem)

-- | Lazy initial segments are a temporary version of the InitMemSegment
-- that has a lazy initialization value `[LazyConst String MWord]`, that completly overrides
-- the `content` `:: [MWord]` field (This field should be `Nothing` anyways, but is completly meaningless).
-- After constructing the globals map, these lazy segments are converted to real ones.

-- * Building initial memory and the `globalMap`
memoryFromGlobals :: Bool -> GEnv MWord -> (LazyInitialMem, Map.Map Name MWord)
memoryFromGlobals tainted ggg = 
  let (lazyInitMem, _, globs) = lazyMemoryFromGlobals tainted ggg in
    (resolveGlobalsMem globs lazyInitMem, globs)
  where resolveGlobalsMem :: Map.Map Name MWord -> LazyInitialMem -> LazyInitialMem
        resolveGlobalsMem globMap lInitMem = map (resolveGlobalsSegment globMap) lInitMem
        resolveGlobalsSegment :: Map.Map Name MWord -> LazyInitSegment -> LazyInitSegment
        resolveGlobalsSegment g (lazyConst, InitMemSegment secr rOnly heapInit loc len _ taint) =
          let concreteInit = map (applyPartialMap g) <$> lazyConst in
          (concreteInit, InitMemSegment secr rOnly heapInit loc len Nothing taint)

lazyMemoryFromGlobals :: Bool -> GEnv MWord -> (LazyInitialMem, MWord, Map.Map Name MWord)
lazyMemoryFromGlobals tainted = foldr memoryFromGlobal ([], 1, Map.empty)
  where memoryFromGlobal ::
          GlobalVariable MWord
          -> (LazyInitialMem, MWord, Map.Map Name MWord)
          -> (LazyInitialMem, MWord, Map.Map Name MWord)
        memoryFromGlobal
            (GlobalVariable name isConst _gTy initzr size align secr heapInit)
            (initMem, nextAddr, gMap) =
          let newLoc = if not heapInit then alignTo align nextAddr
                else heapInitAddress `div` fromIntegral wordBytes in
          let optTaintLabels = if not tainted then Nothing else
                Just $ replicate
                  (fromIntegral (alignTo (fromIntegral wordBytes) size) `div` wordBytes)
                  (V.replicate (fromIntegral wordBytes) untainted) in
          let newLazySegment =
                (initzr, InitMemSegment secr isConst heapInit newLoc (fromIntegral size) Nothing optTaintLabels) in
          -- The addresses assigned to global variable symbols must be given in
          -- bytes, unlike all other global / init-mem related measurements,
          -- which are in words.
          let newByteLoc = newLoc * fromIntegral wordBytes in
          let nextAddr' = if not heapInit then newLoc + size else nextAddr in
          (newLazySegment:initMem, nextAddr', Map.insert name newByteLoc gMap)

        alignTo a x = (a + x - 1) .&. complement (a - 1)
          

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
        raplaceGlobalsOperands gmap' (Glob name) =  -- FIXME : SC I think we can remove Glob as it is covered by lazy constants 
          case Map.lookup name gmap' of
            Just gptr -> return $ LImm $ SConst gptr
            _ -> assumptError $ "Global not found in the environment: " ++ show name
        raplaceGlobalsOperands gmap' (LImm lc) =
          return $ LImm $ applyPartialMap gmap' lc
        raplaceGlobalsOperands _ op = return op
