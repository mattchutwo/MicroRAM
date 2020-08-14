{-# LANGUAGE TypeOperators #-}
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
    ( globals, 
    ) where

import qualified Data.Map as Map

import Util.Util
import Control.Monad

import Compiler.CompilationUnit
import Compiler.Errors
import Compiler.IRs
import Compiler.Registers


{- | Lay global variables in memory. Done in two steps:

1. We create an initial memory that contains all th eglobal varibles
   (maybe uninitialised) and a globalMap mapping global names to their
   location in the initla mem.
2. Then we replace all the uses of global variables in the program, with
   the real location of the variable in memory.

-}
gloabls :: Regs mreg =>
        CompilationUnit (Lprog () mreg Word)
        -> Hopefully $ CompilationUnit (Lprog () mreg Word)
gloabls (CompUnit prog tr regs aData _ ) = do
  (prog', initMem) <- gloabls' prog
  return $ CompUnit prog' tr regs aData initMem

gloabls' :: Regs mreg => Lprog () mreg Word
         -> Hopefully $ (Lprog () mreg Word, InitialMem)
gloabls' (IRprog tenv genv prog) = do
  (initMem, globalMap) <- return $ memoryFromGlobals genv
  prog' <- raplaceGlobals globalMap prog
  return (IRprog tenv genv prog', initMem)

-- * Building initial memory and the `globalMap`N
memoryFromGlobals :: GEnv Word -> (InitialMem, Map.Map Name Word)
memoryFromGlobals ggg  = foldr memoryFromGlobal ([],Map.empty) ggg  
  where memoryFromGlobal ::
          GlobalVariable Word
          -> (InitialMem, Map.Map Name Word)
          -> (InitialMem, Map.Map Name Word)
        memoryFromGlobal (GlobalVariable name isConst gTy init secret) (initMem, gMap) =
          let newLoc = newLocation initMem in
          let newSegment = InitMemSegment secret isConst newLoc (tySize gTy) init in
          (newSegment:initMem, Map.insert name newLoc gMap)
          
        newLocation :: InitialMem -> Word
        newLocation [] = 0
        newLocation (InitMemSegment _ _ loc len _:_) = loc + len
          

-- * Replace global variables with pointers to the initial memeory
raplaceGlobals ::
  Regs mreg =>
  Map.Map Name Word
  -> [LFunction mdata mreg wrdT]
  -> Hopefully $ [LFunction mdata mreg wrdT] 
raplaceGlobals gmap = mapM (raplaceGlobalsFunc gmap)
  where raplaceGlobalsFunc ::
          Regs mreg =>
          Map.Map Name Word
          -> LFunction mdata mreg wrdT
          -> Hopefully $ LFunction mdata mreg wrdT
        raplaceGlobalsFunc gMap func = do
          blocks <- mapM (raplaceGlobalsBlock gMap) (funBody func) 
          return $ func {funBody = blocks }
        
        raplaceGlobalsBlock ::
          Regs mreg =>
          Map.Map Name Word
          -> BB $ LTLInstr mdata mreg wrdT
          -> Hopefully $ BB (LTLInstr mdata mreg wrdT)  
        raplaceGlobalsBlock gMap = undefined
