{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module      : Stacking
Description : LTL -> MARAM
Maintainer  : 
Stability   : 

This pass lays out the stack in memory. The register allocator
deals with the stack abstractly issuing `Lgetstack`, `Lsetstack`, `LAlloc`,
this pass transforms those instructions into real stack manipulations.

Moreover this pass adds the necessary instructions from stack frame
creation and destruction on function call and return.

(Note that global variables are passed as initial memory to the cricuit generator.
 see Compiler/Globals.hs for details)


    Stack layout during function execution

     Low address

     ^                 ^
     |                 |
     +=================+ <- SP 
     | Local variables |
     |                 |
     |                 |
     +-----------------+
     | Callee-saved    |
     | registers       |
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
     | Function   arg1 | 
     | arguments  arg2 |
     |            ...  |  
     +=================+
     | Caller frame    |
     |                 |

     High address

NOTE: we are not setting functions in memory, so
      we do NOT SUPPORT CALLING FUNCTIONS BY POINTER.
      This could be relaxed if need be.


-}
module Compiler.Stacking
    ( stacking, 
    ) where


import Compiler.Errors
import Compiler.Common
import Compiler.IRs
import Compiler.LazyConstants
import Compiler.Metadata
import Compiler.Registers

import Data.Bits

import MicroRAM

import Util.Util


type LOperand mreg =  MAOperand mreg MWord

-- ** Usefull snipets
-- sp points at the next free stack location
push, _pop :: Regs mreg => mreg -> [MAInstruction mreg MWord]
push r = [IstoreW (AReg sp) r,Isub sp sp (LImm $ fromIntegral wordBytes)]
_pop r = [Iadd sp sp (LImm $ fromIntegral wordBytes),IloadW r (AReg sp)]

-- | pushOperand sometimes we want to push a constant
-- Notice here we use ax. This can only be done at funciton entry
-- where ax is callee-saved.
pushOperand :: Regs mreg => LOperand mreg -> [MAInstruction mreg MWord]
pushOperand (AReg r) = push r
pushOperand op = Imov ax op :  push ax



pushN :: Regs mreg => [LOperand mreg] -> [MAInstruction mreg MWord]
pushN [] = []
pushN (r:rs) = pushOperand r ++ pushN rs 

-- PopN doesn't return, just drops the top n things in the stack
popN :: Regs mreg => Word -> [MAInstruction mreg MWord]
popN 0 = []
popN n = [Iadd sp sp (LImm $ fromIntegral $ wordBytes * fromIntegral n) ]


-- | smartMov is like Imov, but does nothing if the registers are the same
smartMov :: Regs mreg => mreg -> mreg -> [MAInstruction mreg MWord]
smartMov r1 r2 = if r1 == r2 then [] else [Imov r1 (AReg r2)]

smartMovMaybe :: Regs mreg => Maybe mreg -> mreg -> [MAInstruction mreg MWord]
smartMovMaybe Nothing _ = []
smartMovMaybe (Just r) a = smartMov r a

-- | Premain: no arguments to main!
-- This is a pseudofunction, that sets up return address for main.
-- Stores the input in memory.
-- Sends main to the returnBlock
premain :: Regs mreg => [NamedBlock Metadata mreg MWord]
premain = return $
  NBlock Nothing $
  -- poison address 0
  map (\x -> (x,md)) $
  [ IpoisonW (LImm 0) sp
  -- Set the top of the stack.
  , Imov sp (LImm initAddr)] ++
  -- push return address for main 
  Imov ax (Label "_ret_") : (push ax) ++
  -- set stack frame
  IstoreW (AReg sp) bp :  -- Store "old" base pointer 
  Imov bp (AReg sp) :    -- set base pointer to the stack pointer
  callMain              -- jump to main
  where callMain = return $ Ijmp $ Label $ show $ Name "main"
        md = trivialMetadata "Premain" ""

        -- Start stack at 2^32.
        initAddr = 1 `shiftL` 32

-- | returnBlock: return lets the program output an answer (when main returns)
returnBlock :: Regs mreg => NamedBlock Metadata mreg MWord
returnBlock = NBlock retName [(Ianswer (AReg ax),md)]
  where md = trivialMetadata "_ret_" (show retName) 
        retName = (Just "_ret_") 


-- ** Function Prologues and Epilogues

-- | prologue: allocates the stack at the beggining of the function
prologue :: Regs mreg => MWord -> [MAInstruction mreg MWord]
prologue size =
    [Isub sp sp (LImm $ fromIntegral $ wordBytes * (fromIntegral size + 1))]


-- | epilogue: deallocate the stack, then jump to return address
epilogue :: Regs mreg => [MAInstruction mreg MWord]
epilogue =
  -- Sp is uselles at this point so we use to calculate return adress
  -- remember return value is passed in ax and bp is marking the old stack 
  Iadd sp bp (LImm $ fromIntegral wordBytes) :
  IloadW sp (AReg sp) : 
  [Ijmp (AReg sp)]


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
  -> [MAInstruction mreg MWord]
funCallInstructions _ ret f _ args =
  -- Push all arguments to stack
  -- We store arguments backwards
  pushN (reverse args) ++
  -- Push return addres
    [Imov ax HereLabel,
     Iadd ax ax (LImm 6) -- FIXME: The compiler should do this addition
    ] ++ push ax ++
    [IstoreW (AReg sp) bp, Imov bp (AReg sp)] ++ -- Set new stack frame (sp is increased in the function)
  -- Run function 
    Ijmp f :
  -- The function should return to this next instruciton
  -- restore the base pointer (right before this is used to compute return address)
  Imov sp (AReg bp): -- get old sp 
  IloadW bp (AReg sp) :         -- get old bp
  -- remove arguments and return address from the stack
  (popN (fromIntegral $ (length args) + 1)) ++
  -- move the return value (allways returns to ax)
  setResult ret
  
setResult :: Regs mreg => Maybe mreg -> [MAInstruction mreg MWord]
setResult Nothing = []
setResult (Just ret) = smartMov ret ax


-- ** Stacking translation

-- | stack only the new instructions
stackLTLInstr :: Regs mreg => LTLInstr' mreg MWord $ MAOperand mreg MWord
              -> Hopefully [MAInstruction mreg MWord]
stackLTLInstr (Lgetstack Incoming offset _ reg) = return $
   [ Iadd reg bp (LImm $ fromIntegral $ wordBytes * (2 + fromIntegral offset))
   , IloadW reg (AReg reg)]
stackLTLInstr (Lsetstack reg Incoming offset _) = return $
   [ Iadd bp bp (LImm $ fromIntegral $ wordBytes * (2 + fromIntegral offset))
   , IstoreW (AReg bp) reg
   , Isub bp bp (LImm $ fromIntegral $ wordBytes * (2 + fromIntegral offset))]
stackLTLInstr (Lgetstack Local offset _ reg) = return $
   [ Isub reg bp (LImm $ fromIntegral $ wordBytes * (1 + fromIntegral offset))
   , IloadW reg (AReg reg)]  -- JP: offset+1?
stackLTLInstr (Lsetstack reg Local offset _) = return $
   [ Isub bp bp (LImm $ fromIntegral $ wordBytes * (1 + fromIntegral offset))
   , IstoreW (AReg bp) reg
   , Iadd bp bp (LImm $ fromIntegral $ wordBytes * (1 + fromIntegral offset))] -- JP: offset+1?
stackLTLInstr (LGetBP reg) = return $
   [ Imov reg (AReg bp)
   ]

stackLTLInstr (LCall typ ret f argsT args) = return $
  funCallInstructions typ ret f argsT args
stackLTLInstr (LRet Nothing) = return epilogue 
stackLTLInstr (LRet (Just _retVal)) =
  return epilogue 
  -- (Imov ax retVal) : epilogue -- Calling convention inserts the move to ax for us, so we skip it here. Can we move `restoreLTLInstruction` here?
stackLTLInstr (LAlloc reg sz n) = do
  -- Return the current sp (that's the base of the new allocation)
  copySp <- return $ smartMovMaybe reg sp
  -- sp = sp + n * sz
  increaseSp <- incrSP sz n
  return $ copySp ++ increaseSp
  where incrSP :: (Regs mreg) => MWord -> MAOperand mreg MWord -> Hopefully [MAInstruction mreg MWord]
        incrSP sz (AReg r) = return $
          [Imull r r (LImm $ roundUp $ fromIntegral sz),
           Isub sp sp (AReg r)]
        incrSP sz (LImm n) = return $
          [Isub sp sp (LImm $ roundUp $ n * fromIntegral sz)]
        incrSP _ _ = assumptError $ "Operand not supported for allocation size. Probably a mistake in the Register allocator. \n"

        -- | Round a constant up to the next multiple of `wordBytes`.
        roundUp (SConst n) = SConst $ (n + fromIntegral wordBytes - 1) .&.
          complement (fromIntegral wordBytes - 1)
        roundUp (LConst f) = LConst $ \ge -> (f ge + fromIntegral wordBytes - 1) .&.
          complement (fromIntegral wordBytes - 1)  
 
-- | stack all instructions
stackInstr ::
  Regs mreg => 
  LTLInstr md mreg MWord
  -> Hopefully [(MAInstruction mreg MWord, md)]
stackInstr (MRI instr md) =  return [(instr,md)]
stackInstr (IRI instr md) = addMD md <$> stackLTLInstr instr 

-- | Add metadata to a list of instructions (or anything)
addMD :: b -> [a] -> [(a, b)]
addMD md ls = map (\x -> (x,md)) ls

stackBlock
  :: Regs mreg
  => (BB Name $ LTLInstr md mreg MWord)
  -> Hopefully (NamedBlock md mreg MWord)
stackBlock (BB name body term _) = do
  body' <- mapM stackInstr (body++term)
  return $ NBlock (Just $ show name) $ concat body'

-- | Translating funcitons
stackFunction
  :: Regs mreg =>
  LFunction Metadata mreg MWord
  -> Hopefully $ [NamedBlock Metadata mreg MWord]
stackFunction (LFunction name _retT _argT size code) = do
  prologueBlock <- return $ NBlock (Just name) $ addMD prolMD (prologue size)
  codeBlocks <- mapM stackBlock code
  return $ prologueBlock : codeBlocks
  where prolMD = trivialMetadata name (show $ Just name)
  
  
stacking :: Regs mreg => Lprog Metadata mreg MWord -> Hopefully $ MAProgram Metadata mreg MWord
stacking (IRprog _ _ functions) = do
  functions' <- mapM stackFunction functions
  return $
    premain ++
    (concat functions')
    ++ [returnBlock]
