{-# LANGUAGE ScopedTypeVariables #-}
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
     +=================+
     | Local variables | <- SP
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
-- sp points at the top of the stack
push, _pop :: Regs mreg => mreg -> [MAInstruction mreg MWord]
push r = [Isub sp sp (LImm $ fromIntegral wordBytes), IstoreW (AReg sp) r]
_pop r = [IloadW r (AReg sp), Iadd sp sp (LImm $ fromIntegral wordBytes)]

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

_smartMovMaybe :: Regs mreg => Maybe mreg -> mreg -> [MAInstruction mreg MWord]
_smartMovMaybe Nothing _ = []
_smartMovMaybe (Just r) a = smartMov r a

-- | Premain: no arguments to main!
-- This is a pseudofunction, that sets up return address for main.
-- Stores the input in memory.
-- Sends main to the returnBlock
premain :: Regs mreg => Name -> [NamedBlock Metadata mreg MWord]
premain returnName = return $
  NBlock Nothing $
  -- poison address 0
  (IpoisonW (LImm 0) sp, md{mdFunctionStart = True}) : -- Premain is a 'function' add function start metadata
  (map (\x -> (x,md)) $
  -- Set the top of the stack.
  Imov sp (LImm initAddr) :
  -- push return address for main 
  Imov ax (Label $ returnName) : (push ax) ++
  -- set stack frame
  (push bp) ++           -- Store "old" base pointer
  Imov bp (AReg sp) :    -- set base pointer to the stack pointer
  callMain)              -- jump to main
  where callMain = return $ Ijmp $ Label mainName
        md = trivialMetadata premainName defaultName

        -- Start stack at 2^32.
        initAddr = 1 `shiftL` 32

-- | returnBlock: return lets the program output an answer (when main returns)
returnBlock :: Regs mreg => Name -> NamedBlock Metadata mreg MWord
returnBlock retName = NBlock (Just retName) [(Ianswer (AReg ax),md)]
  where md = (trivialMetadata retName retName) {mdFunctionStart = True}


-- ** Function Prologues and Epilogues

-- | prologue: allocates the stack at the beggining of the function
prologue :: Regs mreg => MWord -> Name -> [MAInstruction mreg MWord]
prologue size entry =
    [ Isub sp sp (LImm $ fromIntegral $ wordBytes * (fromIntegral size))
    , Ijmp $ Label entry
    ]


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
  Metadata
  -> Ty   -- ^ Function Type
  -> Maybe mreg      -- ^ Return
  -> LOperand mreg         -- ^ Function name
  -> [Ty]
  -> [LOperand mreg] -- ^ Arguments
  -> [(MAInstruction mreg MWord, Metadata)]
funCallInstructions md _ ret f _ args =
  -- Push all arguments to stack
  -- We store arguments backwards
  addMD md 
  (
  -- Push return addres
    [Imov ax HereLabel,
     Iadd ax ax (LImm 7) -- FIXME: The compiler should do this addition
    ] ++ push ax ++
    -- push the old base pointer, and move the base pointer to the sp
    push bp ++
    [Imov bp (AReg sp)] ++
  -- Run function 
    [Ijmp f]) ++
  -- The function should return to this next instruciton
  -- restore the base pointer (right before this it is used to compute return address)
  (Imov sp (AReg bp), md{mdReturnCall = True}): -- get old sp 
  addMD md 
  (IloadW bp (AReg sp) :         -- get old bp
  -- remove arguments and return address from the stack
  (popN (fromIntegral $ (length args))) ++ -- TODO: Is this needed? 
  -- move the return value (always returns to ax)
  setResult ret)
  
setResult :: Regs mreg => Maybe mreg -> [MAInstruction mreg MWord]
setResult Nothing = []
setResult (Just ret) = smartMov ret ax


-- ** Stacking translation

-- | stack only the new instructions
stackLTLInstr :: Regs mreg => Metadata -> LTLInstr' mreg MWord $ MAOperand mreg MWord
              -> Hopefully [(MAInstruction mreg MWord, Metadata)]
stackLTLInstr md (Lgetstack Incoming offset _ reg) = return $ addMD md $
   [ Iadd reg bp (LImm $ fromIntegral $ wordBytes * (2 + fromIntegral offset))
   , IloadW reg (AReg reg)]
stackLTLInstr md (Lsetstack reg Incoming offset _) = return $ addMD md $
   [ Iadd bp bp (LImm $ fromIntegral $ wordBytes * (2 + fromIntegral offset))
   , IstoreW (AReg bp) reg
   , Isub bp bp (LImm $ fromIntegral $ wordBytes * (2 + fromIntegral offset))]
stackLTLInstr md (Lgetstack Outgoing offset _ reg) = return $ addMD md $
   [ Isub reg sp (LImm $ fromIntegral $ wordBytes * (1 + fromIntegral offset))
   , IloadW reg (AReg reg)]
stackLTLInstr md (Lsetstack reg Outgoing _offset _) = return $ addMD md $
   [ Isub sp sp (LImm $ fromIntegral $ wordBytes * 1) -- Offset is ignored,calculate it by bumping sp
   , IstoreW (AReg sp) reg]
stackLTLInstr md (Lgetstack Local offset _ reg) = return $ addMD md $
   [ Isub reg bp (LImm $ fromIntegral $ wordBytes * (1 + fromIntegral offset))
   , IloadW reg (AReg reg)]  -- JP: offset+1?
stackLTLInstr md (Lsetstack reg Local offset _) = return $ addMD md $
   [ Isub bp bp (LImm $ fromIntegral $ wordBytes * (1 + fromIntegral offset))
   , IstoreW (AReg bp) reg
   , Iadd bp bp (LImm $ fromIntegral $ wordBytes * (1 + fromIntegral offset))] -- JP: offset+1?
stackLTLInstr md (LGetBP reg) = return $ addMD md $
   [ Imov reg (AReg bp)
   ]

stackLTLInstr md (LCall typ ret f argsT args) = return $ 
  funCallInstructions md typ ret f argsT args
stackLTLInstr md (LRet Nothing) = return $ addMD md $  epilogue 
stackLTLInstr md (LRet (Just _retVal)) = return $ addMD md $ epilogue 
  -- (Imov ax retVal) : epilogue -- Calling convention inserts the move to ax for us, so we skip it here. Can we move `restoreLTLInstruction` here?
stackLTLInstr md (LAlloc reg sz n) = do
  -- sp = sp + n * sz
  increaseSp <- incrSP sz n
  -- Compute the base of the new allocation (at the current sp)
  let addSp = maybe [] (\r -> [Imov r (AReg sp)]) reg
  return $ addMD md $ increaseSp ++ addSp
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
  LTLInstr Metadata mreg MWord
  -> Hopefully [(MAInstruction mreg MWord, Metadata)]
stackInstr (MRI instr md) =  return [(instr,md)]
stackInstr (IRI instr md) = stackLTLInstr md instr 

-- | Add metadata to a list of instructions (or anything)
addMD :: b -> [a] -> [(a, b)]
addMD md ls = map (\x -> (x,md)) ls

stackBlock
  :: Regs mreg
  => (BB Name $ LTLInstr Metadata mreg MWord)
  -> Hopefully (NamedBlock Metadata mreg MWord)
stackBlock (BB name body term _) = do
  body' <- mapM stackInstr (body++term)
  return $ NBlock (Just name) $ concat body'

-- | Translating functions
stackFunction
  :: forall mreg.
  Regs mreg =>
  LFunction Metadata mreg MWord
  -> Hopefully $ [NamedBlock Metadata mreg MWord]
stackFunction (LFunction name _retT _argT _argN size code) = do
  codeBlocks <- mapM stackBlock code
  entryName <- case codeBlocks of
    NBlock (Just name) _ : _ -> return name
    _ -> assumptError $ "function " ++ show name ++ " entry block has no name"
  let prologueBody = addMD prolMD (prologue size entryName)
  let prologueBlock = NBlock (Just name) $ markFunStart prologueBody 
  return $ prologueBlock : codeBlocks
  where prolMD = trivialMetadata name name
        -- | Add metadata for the first instruction in a funciton
        markFunStart :: [(MAInstruction mreg MWord, Metadata)] -> [(MAInstruction mreg MWord, Metadata)]
        markFunStart ls = let firstInst = head ls in -- We know ls is not empyt because the prelude is not empyt.
          (fst firstInst, (snd firstInst){mdFunctionStart = True}) : tail ls 
  
  
stacking :: Regs mreg => (Lprog Metadata mreg MWord, Word) -> Hopefully $ (MAProgram Metadata mreg MWord, Word)
stacking (IRprog _ _ functions, nextName) = do
  functions' <- mapM stackFunction functions
  let returnName = Name nextName "_ret_"
  return $
    (premain returnName ++
    (concat functions')
    ++ [returnBlock returnName], nextName + 1)
