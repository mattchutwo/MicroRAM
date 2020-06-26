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


    Stack layout during function execution

     |                 |
     +=================+ 
     | Local variables | <- SP
     |                 |
     |                 |
     +-----------------+
     | Spilled         |
     | variables       |
     |                 |  <- BP
     +-----------------+
     | Return address  |
     +-----------------+ 
     | Function        | 
     | arguments       |
     +=================+
     | Caller frame    |
     |                 |


NOTE: we are not setting functions in memory, so
      we do NOT SUPPORT CALLING FUNCTIONS BY POINTER.
      This could be relaxed if need be.


-}
module Compiler.Stacking
    ( stacking, 
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Short as Short

import Data.ByteString.Short (ShortByteString)

import MicroRAM.MicroRAM
--import qualified MicroRAM.MicroRAM as MRAM

import Compiler.CompileErrors
import Compiler.IRs
import qualified MicroRAM.MicroRAM as MRAM  (MAProgram,Program,NamedBlock(..)) 

type Ptr = Word
type MReg = Int

type LOperand =  MAOperand MReg Word

-- * Reserved pointers: Stack pointer (SP) and Base Pointer (BP) are reserved for
-- managing the stack
sp , bp :: MReg
sp = 0
bp = 1

-- * Callee saved registers:
-- we need one calee saved register to operate when calling a function
ax  :: MReg
ax = 2

-- ** Usefull snipets
push, pop :: MReg -> [MAInstruction MReg Word]
push r = [Istore (Reg sp) r, Iadd sp sp (Const 1)]
pop r = [Iload r (Reg sp), Isub  sp sp (Const 1)]

-- | pushOperand sometimes we want to push a constant
pushOperand :: LOperand -> [MAInstruction MReg Word]
pushOperand (Reg r) = push r
pushOperand (Const c) = Imov ax (Const c) :  push ax



-- TODO: Can we do this in batch? Unfortunately I dont think we can statically compute
-- sp + 1
-- TODO: include types 
pushN :: [LOperand] -> [MAInstruction MReg Word]
pushN [] = []
pushN (r:rs) = pushOperand r ++ pushN rs 

-- PopN doesn't return, just drops the top n things in the stack
popN :: Word -> [MAInstruction MReg Word]
popN 0 = []
popN n = [Isub sp sp (Const n) ]


-- | smartMov is like Imov, but does nothing if the registers are the same
smartMov :: MReg -> MReg -> [MAInstruction MReg Word]
smartMov r1 r2 = if r1 == r2 then [Imov r1 (Reg r2)] else []

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
  GEnv Word
  -> Hopefully $ (GVEnv, NamedBlock MReg Word)
storeGlobVars _ = return (Map.empty , NBlock Nothing [])

-- | replaceGlobals: replaces the invocation of global variables with a constant
-- with their pointer (pointing at the global's location in memory)
-- TODO: can force the type to ensure we remove all globals.

replaceGlobalsInstr :: LTLInstr mdata mrag wrdT -> LTLInstr mdata mrag wrdT
replaceGlobalsInstr = undefined

-- | replaceGlobals : replace globals with their actual ptr value
-- 
replaceGlobals _ block = return block -- TODO: Replace global







-- ** Prologues and Epilogues


-- | prologue: allocates the stack at the beggining of the function
prologue :: Word -> [MAInstruction MReg Word]
prologue size =
    (push bp) ++ [Imov bp (Reg sp), Isub sp sp (Const size)]


-- | epilogue: allocates the stack at the beggining of the function
epilogue :: [MAInstruction MReg Word]
epilogue = Imov sp (Reg bp) : pop bp


-- ** Function calls:

-- | Instructions produced to call a function.
-- Note that setting the stackframe is the job of the function.
funCallInstructions ::
  Ty   -- ^ Function Type
  -> Maybe MReg      -- ^ Return
  -> LOperand         -- ^ Function name
  -> [Ty]
  -> [LOperand] -- ^ Arguments
  -> [MAInstruction MReg Word]
funCallInstructions _ ret f _ args =
  -- Push all arguments to stack
  -- Mant architectures store arguemnts backwards, we don't
  pushN args ++
  -- Push return addres
    [Imov ax HereLabel, Iadd ax ax (Const 2)] ++ push ax ++
  -- Run function 
    Ijmp f :
  -- The function should return to the this next instruciton
  -- remove arguments and return address from the stack
  (popN (fromIntegral $ (length args) + 1)) ++
  -- move the return value (allways returns to ax)
  setResult ret
  
setResult :: Maybe MReg -> [MAInstruction MReg Word]
setResult Nothing = []
setResult (Just ret) = smartMov ret ax


-- ** Stacking translation

-- | stack only the new instructions
stackLTLInstr :: LTLInstr' MReg Word $ MAOperand MReg Word
              -> [MAInstruction MReg Word]
stackLTLInstr (Lgetstack slot offset typ reg) =
   [Iadd reg bp (Const offset), Iload reg (Reg reg)]
stackLTLInstr (Lsetstack reg slot offset typ) =
   [Iadd reg bp (Const offset), Istore (Reg reg) reg]
stackLTLInstr (LCall typ ret f argsT args) =
  funCallInstructions typ ret f argsT args
stackLTLInstr (LRet Nothing) = epilogue 
stackLTLInstr (LRet (Just retVal)) =
  (Imov ax retVal) : epilogue 
stackLTLInstr (LAlloc reg typ n) =
  -- Return the current sp (that's the base of the new allocation)
  (smartMov reg sp) ++
  -- Compute the size of the allocated memory
  [Imov sp n, -- TODO optimize away multiplying by one (probably common)
   Imull sp sp (Const $ tySize typ)] ++
  -- Set sp to the new value (old sp + size of allocated mem)
  [Iadd sp sp (Reg reg)]
  
 
-- | stack all instructions
stackInstr ::
  LTLInstr () MReg Word
  -> [MAInstruction MReg Word]
stackInstr (MRI instr ()) =  [instr]
stackInstr (IRI instr _) = stackLTLInstr instr 


stackBlock
  :: GVEnv
  -> (BB $ LTLInstr () Int Word)
  -> Hopefully (NamedBlock MReg Word)
stackBlock genv (BB name body _ ) = do
  body' <- return $ map stackInstr body
  body'' <- replaceGlobals genv body'
  return $ NBlock Nothing $ concat body'

-- | Translating funcitons
stackFunction
  :: GVEnv
  -> LFunction () Int Word
  -> Hopefully $ [NamedBlock MReg Word]
stackFunction genv (LFunction name mdata retT argT size code) = do
  prologueBlock <- return $ NBlock (Just name) $ prologue size
  codeBlocks <- mapM (stackBlock genv) code
  return $ prologueBlock : codeBlocks
  
  
stacking :: Lprog () Int Word -> Hopefully $ MAProgram Int Word
stacking (IRprog tenv globals functions) = do
  (genv, preamble) <- storeGlobVars globals
  functions' <- mapM (stackFunction genv) functions
  return $ preamble : concat functions'
