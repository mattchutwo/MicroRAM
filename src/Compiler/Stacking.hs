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
     |                 |  
     +-----------------+
     | Old BP          | <- BP
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

import Control.Monad


import Util.Util

import Compiler.Errors
import Compiler.IRs
import Compiler.Registers
import qualified MicroRAM.MicroRAM as MRAM  (MAProgram,Program,NamedBlock(..)) 

type Ptr = Word
-- moved to typeclass, type MReg = VReg -- FIXME only while we use trivial register allocation

type LOperand mreg =  MAOperand mreg Word

{-
-- * Reserved pointers: Stack pointer (SP) and Base Pointer (BP) are reserved for
-- managing the stack
sp , bp :: MReg
sp = NewName 0
bp = NewName 1


-- * Callee saved registers:
-- we need one calee saved register to operate when calling a function
ax  :: MReg
ax = NewName 2
-}

-- ** Usefull snipets
push, pop :: Regs mreg => mreg -> [MAInstruction mreg Word]
push r = [Iadd sp sp (Const 1),Istore (Reg sp) r]
pop r = [Iload r (Reg sp), Isub  sp sp (Const 1)]

-- | pushOperand sometimes we want to push a constant
-- Notice here we use ax. This can only be done at funciton entry
-- where ax is callee-saved.
pushOperand :: Regs mreg => LOperand mreg -> [MAInstruction mreg Word]
pushOperand (Reg r) = push r
pushOperand (Const c) = Imov ax (Const c) :  push ax



-- TODO: Can we do this in batch? Unfortunately I dont think we can statically compute
-- sp + 1
-- TODO: include types 
pushN :: Regs mreg => [LOperand mreg] -> [MAInstruction mreg Word]
pushN [] = []
pushN (r:rs) = pushOperand r ++ pushN rs 

-- PopN doesn't return, just drops the top n things in the stack
popN :: Regs mreg => Word -> [MAInstruction mreg Word]
popN 0 = []
popN n = [Isub sp sp (Const n) ]


-- | smartMov is like Imov, but does nothing if the registers are the same
smartMov :: Regs mreg => mreg -> mreg -> [MAInstruction mreg Word]
smartMov r1 r2 = if r1 == r2 then [] else [Imov r1 (Reg r2)]

smartMovMaybe :: Regs mreg => Maybe mreg -> mreg -> [MAInstruction mreg Word]
smartMovMaybe Nothing _ = []
smartMovMaybe (Just r) a = smartMov r a


-- * Initial Memory

{-

The initial Memory is passed to the program. Here is how the memory is set up:
     - All arguemnts, files, configurations, are laid in contiguous memory.
     - At the top of the memory (highest address) the arguments to main are laid like:
       + The command line inputs
       + argv[0..] (pointing at each command line input)
       + argc
       + argv (pointing to two mem locations back)
     - Location 1 is resreved for a pointer to argc
@

     |            |
     +------------+                  +-+
     |arg^        +--+                 |
     +------------+  |                 |
+--->+argc        |  |                 |
|    +------------+  |                 |
| +--+argv[argc+1]|  |                 |
| |  |...         |  |                 | Initial
| +--+arg^[0]     +<-+                 | Memory
| |  +------------+                    |
| +->+Comand line |                    |
|    |arguments   |                    |
|    +------------+                    |
|    | files      |                    |
|    |            |                    |
|    +------------+                    |
+----+ ptr argc   |                    |
|    +------------+                    |
     | Null       |                    |
     +============+                  +-+
@

We then "Set global variables" (preamble) and run 'premain'
to set up arguemtns and return address for Main.

-}

-- ** Set global variables

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

  Following up from initial memory it produces the following structure:

@
     |            |    <-+ preamble sp
     +------------+
     | Glogals    |
     |            |
     +------------+                  +-+
     |argv        +--+                 |
     +------------+  |                 |
+--->+argc        |  | <-+ preamble bp |
|    +------------+  |                 |
| +--+argv[argc-1]|  |                 |
| |  |...         |  |                 | Initial
| +--+arg^[0]     +<-+                 | Memory
| |  +------------+                    |
| +->+Comand line |                    |
|    |arguments   |                    |
|    +------------+                    |
|    | files      |                    |
|    |            |                    |
|    +------------+                    |
+----+ ptr argc   |                    |
|    +------------+                    |
     | Null       |                    |
     +============+                  +-+
@


-}

storeGlobVars ::
  Regs mreg =>
  GEnv Word
  -> Hopefully $ (GVEnv, NamedBlock mreg Word)
storeGlobVars [] = return (Map.empty , NBlock Nothing [])
storeGlobVars ls = do 
  -- aggregate all gloabls
  (genvInstr, genvSize) <- foldM storeGlobVar ([],0) ls
  return (Map.empty , NBlock Nothing genvInstr)

  where storeGlobVar :: Regs mreg =>
                        ([MAInstruction mreg Word],Word) 
                        -> GlobalVariable Word
                        -> Hopefully $ ([MAInstruction mreg Word],Word)
        storeGlobVar (instrs, location) (GlobalVariable nm isConst typ init) = do
          newInstrs <- return $ writeGlob init location
          return (instrs++newInstrs, location + tySize typ)
        writeGlob :: Regs mreg =>
                     Maybe [Word]
                     -> Word -> [MAInstruction mreg Word]
        writeGlob Nothing _ = []
        writeGlob (Just []) loc = []
        writeGlob (Just initial) loc =
          Iadd sp bp (Const loc) :
          storeList initial
        storeList :: Regs mreg =>
                     [Word] -> [MAInstruction mreg Word]
        storeList [] = []
        storeList [c] = [Istore (Const c) sp] 
        storeList (c:ls) =
          [Istore (Const c) sp, Iadd sp sp (Const 1)] ++
          storeList ls

        
          
  

-- | replaceGlobals: replaces the invocation of global variables with a constant
-- with their pointer (pointing at the global's location in memory)
-- TODO: can force the type to ensure we remove all globals.

replaceGlobalsInstr :: LTLInstr mdata mrag wrdT -> LTLInstr mdata mrag wrdT
replaceGlobalsInstr = undefined

-- | replaceGlobals : replace globals with their actual ptr value
-- 
replaceGlobals _ block = return block -- TODO: Replace global


-- ** Pre-main:
{-  This is the code that:
    1. Passes the arguments to main
    2. Sets the return address for main.
    3. Returns the final value.

-}

-- -- | Read input: OBSOLETE, We start with an initialized memory
-- -- We read the entire input, store it into the stack (almost like in the paper)
-- readInput :: Regs mreg => [NamedBlock mreg Word]
-- readInput =
--   (MRAM.NBlock Nothing
--   [Istore (Const 0) sp]) :                -- 1.
--   MRAM.NBlock (Just "_read input_")
--   [Iread bp (Const 0),                    -- 2.
--     Icjmp (Label "_End read input_"),     -- 3.
--     Iadd sp sp (Const 1),                 -- 4.
--     Istore (Reg sp) bp,                   -- 5.
--     Ijmp (Label "_read input_")]:         -- 6.
--   MRAM.NBlock (Just "_End read input_")
--   [ Imov argc (Reg sp),
--    Imov argv (Const 1)]  :                 -- FIXME passing arguments in registers for trivial reg alloc.
--   []

{- | Find arguments: in the current setup argc and argv are next to each other,
     and at the end of initial memory, but before the globals. After
     the preamble bp should be pointing at 'bp' and 'sp' should point at the top
     of memory (end of globals).

    This function puts main's arguemnts in the right place (regs 0 and 1 for the
    trivila register allocator) and sets stack pointer to the right place
    (return address).

    After premain the memory should look like this 
    (Look at storeGlobVars to see how memory starts)
       
@
     +Beggining   +
     |Stack memory|
     |============|
     | main return|    <-+ initial sp
     +------------+
     | Glogals    |
     |            |
     +------------+                  +-+
     |arg^        +--+                 |
     +------------+  |                 |
+--->+argc        |  | <-+ initial bp  |
|    +------------+  |                 |
| +--+argv[argc+1]|  |                 |
| |  |...         |  |                 | Initial
| +--+arg^[0]     +<-+                 | Memory
| |  +------------+                    |
| +->+Comand line |                    |
|    |arguments   |                    |
|    +------------+                    |
|    | files      |                    |
|    |            |                    |
|    +------------+                    |
+----+ ptr argc   |                    |
|    +------------+                    |
     | Null       |                    |
     +============+                  +-+
@
-}

findAguments :: Regs mreg => [NamedBlock mreg Word]
findAguments = (MRAM.NBlock (Just "_Find arguments_")
  [ Iload bp (Const 1)    -- Base pointer points to argc
  , Iload sp (Reg bp)     -- Temporarily load argc into sp.
  , Isub ax bp (Reg sp)   -- Compute argv (ax = bp - argc). Is this offset correct?

  , Iadd bp bp (Const 1)  -- Push argv to the stack.
  , Istore (Reg bp) ax

  , Iadd bp bp (Const 1)  -- Push argc to the stack.
  , Istore (Reg bp) sp

  , Iadd bp bp (Const 2)  -- Leave space for return address and old bp.

  , Imov sp (Reg bp)      -- Set stack pointer.
  , Iadd sp sp (Const 1)
  ]) :
  []

  -- [Iload sp (Const 1),  -- Stack pointer points to start of memory
  --  Iload argc (Reg sp), -- Load argc into first argument
  --  Iadd sp sp (Const 1),
  --  Iload argv (Reg sp), -- Load argc into first argument
  --  Iadd sp sp (Const 1)
  -- ]) :
  -- []

-- | Premain:
-- This is a pseudofunction, that sets up return address for main.
-- Stores the input in memory.
-- Sends main to the returnBlock
premain :: Regs mreg => [NamedBlock mreg Word]
premain =
  findAguments ++
  [MRAM.NBlock Nothing $ Imov ax (Label "_ret_") : push ax]

-- | returnBlock: return lets the program output an answer (when main returns)
returnBlock :: Regs mreg => NamedBlock mreg Word
returnBlock = MRAM.NBlock (Just "_ret_") [Ianswer (Reg ax)]



-- ** Function Prologues and Epilogues


-- | prologue: allocates the stack at the beggining of the function
prologue :: Regs mreg => Word -> [MAInstruction mreg Word]
prologue size =
    (push bp) ++ [Imov bp (Reg sp), Isub sp sp (Const size)]


-- | epilogue: deallocate the stack, then jump to return address
epilogue :: Regs mreg => [MAInstruction mreg Word]
epilogue =
  -- restore the old stack pointer (bp is popped by the caller)
  Imov sp (Reg bp) :
  -- load return address and jump (remember return is passed in ax, so we use bp here)
  -- bp = sp point at the old bp and the return address is one bellow. 
  Isub bp bp (Const 1) :
  Iload bp (Reg bp) : 
  [Ijmp (Reg bp)]


-- ** Function calls:

-- | Instructions produced to call a function.
-- Note that setting the stackframe is the job of the callee function.
funCallInstructions ::
  Regs mreg => 
  Ty   -- ^ Function Type
  -> Maybe mreg      -- ^ Return
  -> LOperand mreg         -- ^ Function name
  -> [Ty]
  -> [LOperand mreg] -- ^ Arguments
  -> [MAInstruction mreg Word]
funCallInstructions _ ret f _ args =
  -- Push all arguments to stack
  -- Mant architectures store arguemnts backwards, we don't
  pushN args ++
  -- Push return addres
    [Imov ax HereLabel, Iadd ax ax (Const 2)] ++ push ax ++
  -- Run function 
    Ijmp f :
  -- The function should return to the this next instruciton
  -- restore the base pointer (right before this is used to compute return address)
  pop bp ++
  -- remove arguments and return address from the stack
  (popN (fromIntegral $ (length args) + 1)) ++
  -- move the return value (allways returns to ax)
  setResult ret
  
setResult :: Regs mreg => Maybe mreg -> [MAInstruction mreg Word]
setResult Nothing = []
setResult (Just ret) = smartMov ret ax


-- ** Stacking translation

-- | stack only the new instructions
stackLTLInstr :: Regs mreg => LTLInstr' mreg Word $ MAOperand mreg Word
              -> [MAInstruction mreg Word]
stackLTLInstr (Lgetstack Incoming offset typ reg) =
   [Isub reg bp (Const (-2-offset)), Iload reg (Reg reg)]
stackLTLInstr (Lsetstack reg Incoming offset typ) =
   [Isub reg bp (Const (-2-offset)), Istore (Reg reg) reg]

stackLTLInstr (Lgetstack Local offset typ reg) =
   [Iadd reg bp (Const offset), Iload reg (Reg reg)]  -- JP: offset+1?
stackLTLInstr (Lsetstack reg Local offset typ) =
   [Iadd reg bp (Const offset), Istore (Reg reg) reg] -- JP: offset+1?

stackLTLInstr (LCall typ ret f argsT args) =
  funCallInstructions typ ret f argsT args
stackLTLInstr (LRet Nothing) = epilogue 
stackLTLInstr (LRet (Just retVal)) =
  (Imov ax retVal) : epilogue 
stackLTLInstr (LAlloc reg typ n) =
  -- Return the current sp (that's the base of the new allocation)
  (smartMovMaybe reg sp) ++
  -- sp = sp + n * |typ| 
  incrSP typ n
  where incrSP typ (Reg r) =
          [Imull r r (Const $ tySize typ),
           Iadd sp sp (Reg r)]
        incrSP typ (Const n) = [Iadd sp sp (Const $ n * (tySize typ))] 
  -- Compute the size of the allocated memory
  
 
-- | stack all instructions
stackInstr ::
  Regs mreg => 
  LTLInstr () mreg Word
  -> [MAInstruction mreg Word]
stackInstr (MRI instr ()) =  [instr]
stackInstr (IRI instr _) = stackLTLInstr instr 


stackBlock
  :: Regs mreg
  => GVEnv
  -> (BB Name $ LTLInstr () mreg Word)
  -> Hopefully (NamedBlock mreg Word)
stackBlock genv (BB name body term _ ) = do
  body' <- return $ map stackInstr (body++term)
  body'' <- replaceGlobals genv body'
  return $ NBlock (Just $ show name) $ concat body'

name2string :: Name -> String
name2string (Name st) = "Name:"++(show st)
name2string (NewName st) = "NewName:"++(show st)

-- | Translating funcitons
stackFunction
  :: Regs mreg =>
  GVEnv
  -> LFunction () mreg Word
  -> Hopefully $ [NamedBlock mreg Word]
stackFunction genv (LFunction name mdata retT argT size code) = do
  prologueBlock <- return $ NBlock (Just name) $ prologue size
  codeBlocks <- mapM (stackBlock genv) code
  return $ prologueBlock : codeBlocks
  
  
stacking :: Regs mreg => Lprog () mreg Word -> Hopefully $ MAProgram mreg Word
stacking (IRprog tenv globals functions) = do
  (genv, preamble) <- storeGlobVars globals
  functions' <- mapM (stackFunction genv) functions
  return $ preamble : premain ++ (concat functions') ++ [returnBlock]
