{-# LANGUAGE TypeOperators #-}
{-|
Module      : Stacking
Description : LTL -> MARAM
Maintainer  : 
Stability   : 

This pass lays out the stack in memory in two steps:

1. Writes global variables to memory (in the 'prestack').
   Then replaces all ocurrences of the global variable with the
   value of its pointer.
2. Allocates stack variables replacing the first definition with
   an alloc and other accesses with store and loads. 


NOTE: we are not setting functions in memory, so
      we do NOT SUPPORT CALLING FUNCTIONS BY POINTER.
      This could be relaxed if need be.


-}
module Compiler.Stacking
    ( stacking, 
    ) where

import qualified Data.Map.Strict as Map
import Data.ByteString.Short


import MicroRAM.MicroRAM
--import qualified MicroRAM.MicroRAM as MRAM

import Compiler.CompileErrors
import Compiler.IRs
import qualified MicroRAM.MicroRAM as MRAM  (MAProgram,Program,NamedBlock(..)) 

type Ptr = Word
type MReg = Int

-- * Reserved pointers: Stack pointer (SP) and Base Pointer (BP) are reserved for
-- managing the stack
sp , bp :: MReg
sp = 0
bp = 1

-- ** Usefull snipets
push r = [Istore (Reg sp) r, Iadd sp sp (Const 1)]
pop r = [Iload r (Reg sp), Isub  sp sp (Const 1)]


-- ** Setting Global Variables

-- | Global environment.
-- Maps names of global variables to
-- their pointer (pointing to their location in memory) 
type GVEnv = Map.Map ShortByteString Ptr

{- | storeGlobVars
   Produces a pair with
   1. A Preamble: set of instructions that lay global variables in memory.
      Sets the initial stack pointer etc. 
   2. A map with the locations of the globals, so we can replace their
      values in the code. 
-}

storeGlobVars ::
  GVEnv
  -> Hopefully $ (GVEnv,[MAInstruction MReg Word])
storeGlobVars _ = return (Map.empty ,[])

-- | replaceGlobals: replaces the invocation of global variables with a constant
-- with their pointer (pointing at the global's location in memory)
-- TODO: can force the type to ensure we remove all globals.

replaceGlobalsInstr :: LTLInstr mdata mrag wrdT -> LTLInstr mdata mrag wrdT
replaceGlobalsInstr = undefined


-- ** Prologues and Epilogues


-- | prologue: allocates the stack at the beggining of the function
prologue :: LFunction mdata mreg wrdT -> [MAInstruction MReg Word]
prologue f =
  let size = stackSize f in
    (push bp) ++ [Imov bp (Reg sp), Isub sp sp (Const size)]


-- | epilogue: allocates the stack at the beggining of the function
epilogue :: LFunction mdata mreg wrdT -> [MAInstruction MReg Word]
epilogue _ = Imov sp (Reg bp) : pop bp


  
stacking :: Lprog () Int Word -> Hopefully $ MAProgram Int Word
stacking = undefined 
