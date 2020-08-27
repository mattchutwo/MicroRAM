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

import Util.Util


import Compiler.CompilationUnit
import Compiler.Errors
import Compiler.IRs
import Compiler.Registers

import MicroRAM.MicroRAM

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

-- * Building initial memory and the `globalMap`
memoryFromGlobals :: GEnv MWord -> (InitialMem, Map.Map String MWord)
memoryFromGlobals ggg  = foldr memoryFromGlobal ([],Map.empty) ggg  
  where memoryFromGlobal ::
          GlobalVariable MWord
          -> (InitialMem, Map.Map String MWord)
          -> (InitialMem, Map.Map String MWord)
        memoryFromGlobal (GlobalVariable name isConst gTy init secret) (initMem, gMap) =
          let newLoc = newLocation initMem in
          let newSegment = InitMemSegment secret isConst newLoc (fromIntegral $ tySize gTy) init in
          (newSegment:initMem, Map.insert name newLoc gMap)
          
        newLocation :: InitialMem -> MWord
        newLocation [] = 0
        newLocation (InitMemSegment _ _ loc len _:_) = loc + len
          

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
        raplaceGlobalsOperands gmap (Glob name) =
          case Map.lookup name gmap of
            Just gptr -> return $ Const gptr
            _ -> assumptError $ "Global not found in the environment: " ++ name
        raplaceGlobalsOperands _ op = return op
