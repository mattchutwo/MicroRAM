{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Instruction Selection
Description : LLVM -> RTL
Maintainer  : santiago@galois.com
Stability   : prototype

This module compiles LLVM to RTL.

Our instruciton selection is linear. There are no complex instructions
so selection is pretty siple.

If we add complex instructions we might want to build the DAG and do
instruction selection there.

As we do the instructino selections WE KEEP THE DAG INFORMATION by
annotating each block with all the blocks it can jump to. This could
be reversed (annotate blocks by those blocks that jump to it) in
a single pass. 

-}

module Compiler.InstructionSelection
    ( instrSelect,
    ) where

import Data.ByteString.Short
import Data.ByteString.UTF8 as BSU
import qualified Data.Map as Map 

import Control.Monad.Except
import Control.Monad.State.Lazy


import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Typed as LLVM
import qualified LLVM.AST.Constant as LLVM.Constant
import qualified LLVM.AST.IntegerPredicate as IntPred


import Compiler.Errors
import Compiler.IRs
import Util.Util

import MicroRAM.MicroRAM (Operand'(..), MAOperand) 
import           MicroRAM.MicroRAM (MWord)
import qualified MicroRAM.MicroRAM as MRAM

{-| Notes on this instruction generation :

   TO DOs:
   1. Check exception handeling, I'm  not sure I'm translating that correctly.
      Particulary, do we include exeption jumpin in DAGS? Right now we don't
   

-}

-- ** Translation between LLVM and RTL "things"
any2short :: Show a => a -> ShortByteString
any2short n = toShort $ BSU.fromString $ show $ n

name2name :: LLVM.Name -> Name
name2name (LLVM.Name s) = Name s
name2name (LLVM.UnName n) = Name $ any2short n

name2nameM :: Monad m => LLVM.Name -> m Name
name2nameM nm = return $ name2name nm

-- | For Global names we just use strings.
-- Should we use Names insted?
gName2String :: LLVM.Name -> String
gName2String = show


--  implError "Unnamed opareands not supported yet. TODO soon... "


wrd2integer:: MWord -> Integer
wrd2integer x = fromIntegral x

integer2wrd:: Integer -> Hopefully $ MWord
integer2wrd x
  | x >= (wrd2integer minBound) && x <= (wrd2integer maxBound) = return $ fromInteger x
  | otherwise = otherError $ "Literal out of bounds: " ++ (show x) ++ ". Bounds " ++ (show (wrd2integer minBound, wrd2integer maxBound)) 
  
getConstant :: LLVM.Constant.Constant -> Hopefully $ MAOperand VReg MWord
getConstant (LLVM.Constant.Int _ val) = Const <$> integer2wrd val
getConstant (LLVM.Constant.Undef _typ) = return $ Const 0 -- Concretising values is allways allowed TODO: Why are there undefined values, can't we remove this?
getConstant (LLVM.Constant.GlobalReference _typ name) = return $ Glob $ gName2String name
getConstant (LLVM.Constant.GetElementPtr _ _ _) = assumptError $
  "Constant structs are not supported yet. This should go away with -O1. If you are seeing this message and used at least -O1 please report."
getConstant (LLVM.Constant.Null _typ) = return $ Const 0 -- Ignores type/size
getConstant consT = otherError $
  "Illegal constant. Maybe you used an unsuported type (e.g. float) or you forgot to run constant propagation (i.e. constant expresions in instructions): \n \t" ++ (show consT) ++ "\n"


operand2operand :: LLVM.Operand -> Hopefully $ MAOperand VReg MWord
operand2operand (LLVM.ConstantOperand c) = getConstant c
operand2operand (LLVM.LocalReference _ name) = do
  name' <- (name2nameM name)
  return $ Reg name'
operand2operand _ = implError "operand, probably metadata"


-- | Transforms `LLVM.Type` into backend types `Ty`
-- Note that LLVM types can be recursive. However, in well-typed LLVMS,
-- all structs have a computable size. This ensures that there are no
-- infinite loops and that `type2type` terminates.
-- Unfortunately this property is not enforced by the type system.
type2type :: LLVMTypeEnv -> LLVM.Type -> Hopefully Ty
type2type _ (LLVM.IntegerType _n) = return Tint -- FIXME check size! 
type2type _tenv (LLVM.PointerType _t _) = do
  --pointee <- type2type tenv t
  return $ Tptr 
type2type _ (LLVM.FunctionType _ _ _) = return Tint -- FIXME enrich typed!
type2type tenv (LLVM.ArrayType size elemT) = do
  elemT' <- type2type tenv elemT
  size' <- return $ size -- wrdFromwrd64 
  return $ Tarray size' elemT'
type2type _ (LLVM.StructureType True _) = assumptError "Can't pack structs yet."
type2type tenv (LLVM.StructureType False tys) = Tstruct <$> mapM (type2type tenv) tys
type2type tenv (LLVM.NamedTypeReference name) = do
  case Map.lookup name tenv of
    Just ty -> type2type tenv ty
    Nothing -> assumptError $ "Type not defined: \n \t" ++ show name ++ ".\n" 
type2type _ t = implError $ "Type: \n \t " ++ (show t)


-- | toRTL lifts simple MicroRAM instruction into RTL.
toRTL :: [MRAM.MA2Instruction VReg MWord] -> [MIRInstr () MWord]
toRTL = map  $ \x -> MirM x ()

returnRTL :: Monad m => [MRAM.MA2Instruction VReg MWord] -> m [MIRInstr () MWord]
returnRTL = return . toRTL

-- ** State
-- We create a state to create new variables
type Statefully = StateT Word Hopefully

initState :: Word
initState = 2 -- Leave space for ESP and EBP

freshName :: Statefully Name
freshName = do
  n <- get
  put (n + 1)
  return $ NewName n

-- ** Instruction selection

-- | Instruction Generation
-- We mostly generate MA2Instructions and then lift them to RTL. The only exception is
-- function call

--isInstrs = undefined

-- |I
type BinopInstruction = VReg -> MRAM.MAOperand VReg MWord -> MRAM.MAOperand VReg MWord ->
  MRAM.MA2Instruction VReg MWord
isBinop ::
  Maybe VReg
  -> LLVM.Operand
  -> LLVM.Operand
  -> BinopInstruction
  -> Hopefully $ [MRAM.MA2Instruction VReg MWord]
isBinop Nothing _ _ _ = Right $ [] --  without return is a noop
isBinop (Just ret) op1 op2 bop = do
  a <- operand2operand op1
  b <- operand2operand op2
  return $ [bop ret a b]

-- ** Comparisons
{- Unfortunately MRAM always puts a comparisons in the "flag"
   So if we want the value we need to do 2 operations to get the value.
   In the future, we can reserve one "register" to work as the flag, then a smart -}

-- There are two ways to move the result from the flag.
-- The straight worfward cmptTail_pos and the negated one cmptTail_neg

cmptTail_pos, cmptTail_neg
  :: (Monad m, Num wrdT) =>
     regT1
     -> m [MRAM.Instruction' regT1 operand1 (Operand' phase regT2 wrdT)]
cmptTail_pos ret = return [MRAM.Imov ret (Const 0), MRAM.Icmov ret (Const 1)] -- moving the flag to the register... super wasteful! write a better register allocater
cmptTail_neg ret = return [MRAM.Imov ret (Const 1), MRAM.Icmov ret (Const 0)] -- Neg. the result && mov the flag to the register... wasteful! 

-- | cmptTail:
-- Describe if the comparison is computed directly or it's negation 
cmptTail :: IntPred.IntegerPredicate -> VReg -> Hopefully [MRAM.MA2Instruction VReg MWord]
cmptTail IntPred.EQ = cmptTail_pos -- This values allow to flip the result
cmptTail IntPred.NE = cmptTail_neg 
cmptTail IntPred.UGT = cmptTail_pos
cmptTail IntPred.UGE = cmptTail_pos
cmptTail IntPred.ULT = cmptTail_neg
cmptTail IntPred.ULE = cmptTail_neg
cmptTail IntPred.SGT = cmptTail_pos
cmptTail IntPred.SGE = cmptTail_pos
cmptTail IntPred.SLT = cmptTail_neg
cmptTail IntPred.SLE = cmptTail_neg

-- | Instruction selection for comparisons
isCompare
  :: IntPred.IntegerPredicate
     -> operand1
     -> operand2
     -> Hopefully (MRAM.Instruction' regT operand1 operand2)
isCompare IntPred.EQ lhs rhs = return $ MRAM.Icmpe lhs rhs
isCompare IntPred.NE lhs rhs = return $ MRAM.Icmpe lhs rhs
-- Unsigned
isCompare IntPred.UGT lhs rhs = return $ MRAM.Icmpa lhs rhs
isCompare IntPred.UGE lhs rhs = return $ MRAM.Icmpae lhs rhs
isCompare IntPred.ULT lhs rhs = return $ MRAM.Icmpae lhs rhs
isCompare IntPred.ULE lhs rhs = return $ MRAM.Icmpa lhs rhs
-- Signed
isCompare IntPred.SGT lhs rhs = return $ MRAM.Icmpg lhs rhs
isCompare IntPred.SGE lhs rhs = return $ MRAM.Icmpge lhs rhs
isCompare IntPred.SLT lhs rhs = return $ MRAM.Icmpge lhs rhs
isCompare IntPred.SLE lhs rhs = return $ MRAM.Icmpg lhs rhs


fError :: MonadError CmplError m => m a
fError = implError "Floatin point arithmetic"

_constzero,constOne :: LLVM.Operand
_constzero = LLVM.ConstantOperand (LLVM.Constant.Int (toEnum 0) 0)
constOne = LLVM.ConstantOperand (LLVM.Constant.Int (toEnum 1) 1)

-- *** Trtanslating Function parameters and types

function2function
  :: LLVMTypeEnv -> Either a LLVM.Operand -> Hopefully (MAOperand VReg MWord, Ty, [Ty])
function2function _ (Left _ ) = implError $ "Inlined assembly not supported"
function2function tenv (Right (LLVM.LocalReference ty nm)) = do
  nm' <- name2nameM nm
  (retT', paramT') <- functionTypes ty
  return (Reg nm',retT',paramT')
  where functionTypes (LLVM.FunctionType retTy argTys False) = do
          retT' <- type2type tenv retTy
          paramT' <- mapM (type2type tenv) argTys
          return (retT',paramT')
        functionTypes (LLVM.FunctionType  _ _ True) =
          implError $ "Variable parameters (isVarArg in function call)."
        functionTypes ty =  assumptError $ "Function type expected found " ++ show ty ++ " instead."
function2function _ (Right (LLVM.ConstantOperand c)) =
  implError $ "Calling a function with a constant or a global. You called: \n \t" ++ show c
function2function _ (Right op) = 
  implError $ "Calling a function with unsuported operand. You called: \n \t" ++ show op

-- | Process parameters into RTL format
-- WE dump the attributes

params2params
  :: Traversable t =>
     t (LLVM.Operand, b)
     -> Either CmplError (t (MAOperand VReg MWord))
params2params params  = do
  params' <- mapM (operand2operand . fst) params -- fst dumps the attributes
  return params' 


-- | Instruction Selection for single LLVM instructions 
isInstruction :: LLVMTypeEnv -> Maybe VReg -> LLVM.Instruction -> Statefully $ [MIRInstr () MWord]
-- *** Arithmetic

-- Add
isInstruction _ ret (LLVM.Add _ _ o1 o2 _) = lift $ toRTL <$> isBinop ret o1 o2 MRAM.Iadd
-- Sub
isInstruction _ ret (LLVM.Sub _ _ o1 o2 _) = lift $ toRTL <$> isBinop ret o1 o2 MRAM.Isub
-- Mul
isInstruction _ ret (LLVM.Mul _ _ o1 o2 _) = lift $ toRTL <$> isBinop ret o1 o2 MRAM.Imull
-- SDiv
isInstruction _ _ret (LLVM.SDiv _ _ _o1 _o2 ) = implError "Signed division ius hard! SDiv"
-- SRem
isInstruction _ _ret (LLVM.SRem _o1 _o2 _) = implError "Signed division ius hard! SRem"



-- *** Floating Point 
-- FAdd
isInstruction _ _ret (LLVM.FAdd _ _o1 _o2 _) = implError "Fast Multiplication FMul"
-- FSub
isInstruction _ _ret (LLVM.FSub _ _o1 _o2 _) =  implError "Fast Multiplication FMul"
-- FMul
isInstruction _ _ret (LLVM.FMul _ _o1 _o2 _) =  implError "Fast Multiplication FMul"
-- FDiv
isInstruction _ _ret (LLVM.FDiv _ _o1 _o2 _) =  implError "Fast Division FDiv"
-- FRem
isInstruction _ _ret (LLVM.FRem _ _o1 _o2 _) = fError

-- *** Unsigned operations
-- UDiv
isInstruction _ ret (LLVM.UDiv _ o1 o2 _) = lift $ toRTL <$> isBinop ret o1 o2 MRAM.Iudiv -- this is easy
-- URem
isInstruction _ ret (LLVM.URem o1 o2 _) = lift $ toRTL <$> isBinop ret o1 o2 MRAM.Iumod -- this is eay


-- *** Shift operations
-- Shl
isInstruction _ ret (LLVM.Shl _ _ o1 o2 _) = lift $ toRTL <$> isBinop ret o1 o2 MRAM.Ishl
-- LShr
isInstruction _ ret (LLVM.LShr _ o1 o2 _) = lift $ toRTL <$> isBinop ret o1 o2 MRAM.Ishr
-- AShr
isInstruction _ _ret (LLVM.AShr _ _o1 _o2 _) =  implError "Arithmetic shift right AShr"

-- *** Logical
--And
isInstruction _ ret (LLVM.And o1 o2 _) =  lift $ toRTL <$> isBinop ret o1 o2 MRAM.Iand
--Or
isInstruction _ ret (LLVM.Or o1 o2 _) =  lift $ toRTL <$> isBinop ret o1 o2 MRAM.Ior
--Xor
isInstruction _ ret (LLVM.Xor o1 o2 _) =  lift $ toRTL <$> isBinop ret o1 o2 MRAM.Ixor

-- *** Memory operations
-- Alloca
{- we ignore type, alignment and metadata and assume we are storing integers,
   we only look at the numElements.
   In fact (currently) we dont check  stack overflow or any similar safety check
   Also the current granularity of our memory is per word so ptr arithmetic and alignment are trivial. -}
isInstruction tenv ret (LLVM.Alloca a Nothing b c) =
  isInstruction tenv ret (LLVM.Alloca a (Just constOne) b c) --NumElements is defaulted to be one. 
isInstruction tenv ret (LLVM.Alloca ty (Just size) _ _) = lift $ do
  ty' <- type2type tenv ty
  size' <- operand2operand size
  return [MirI (RAlloc ret ty' size') ()]



-- Load
isInstruction _ Nothing (LLVM.Load _ _ _ _ _) = return [] -- Optimization reconside if we care about atomics
isInstruction _ (Just ret) (LLVM.Load _ n _ _ _) = lift $ do 
  a <- operand2operand n
  returnRTL $ (MRAM.Iload ret a) : []
    
-- | Store
{- Store yields void so we can will ignore the return location -}

isInstruction _ _ (LLVM.Store _ adr cont _ _ _) = do
  cont' <- lift $ operand2operand cont
  adr' <- lift $ operand2operand adr
  returnRTL [MRAM.Istore adr' cont']


-- *** Compare
{- Unfortunately MRAM always puts a comparisons in the "flag"
   So if we want the value we need to do 2 operations to get the value.
   In the future, we can reserve one "register" to work as the flag, then a smart
   register allocator can actually use the flag as it was intended...
-}

isInstruction _ Nothing (LLVM.ICmp _pred _op1 _op2 _) =  lift $ return [] -- Optimization
isInstruction _ (Just ret) (LLVM.ICmp pred op1 op2 _) = lift $ do
  lhs <- operand2operand op1
  rhs <- operand2operand op2
  comp' <- isCompare pred lhs rhs -- Do the comparison
  compTail <- cmptTail pred ret -- Put the comparison in the ret register
  returnRTL $ comp': compTail

                        
-- *** Function Call 
isInstruction tenv ret (LLVM.Call _ _ _ f args _ _ ) = lift $  do
  (f',retT,paramT) <- function2function tenv f
  args' <- params2params args
  return [MirI (RCall retT ret f' paramT args') ()]

-- *** Phi
isInstruction _ Nothing (LLVM.Phi _ _ _)  = return [] -- Phi without a name is useless
isInstruction _ (Just ret) (LLVM.Phi _typ ins _)  =  lift $ do
  ins' <- mapM convertPhiInput ins
  return $ [MirI (RPhi ret ins') ()]

isInstruction _ Nothing (LLVM.Select _ _ _ _)  = return [] -- Select without a name is useless
isInstruction _ (Just ret) (LLVM.Select cond op1 op2 _)  =  lift $ toRTL <$> do
   cond' <- operand2operand cond
   op1' <- operand2operand op1 
   op2' <- operand2operand op2 
   return [MRAM.Icmpe cond' (Const 1), MRAM.Imov ret op2', MRAM.Icmov ret op1']

-- *** GetElementPtr 
isInstruction _ Nothing (LLVM.GetElementPtr _ _addr _inxs _) = return [] -- GEP without a name is useless
isInstruction tenv (Just ret) (LLVM.GetElementPtr _ addr inxs _) = do
  addr' <- lift $ operand2operand addr
  ty' <- lift $ typeFromOperand addr
  -- inxs' <- lift $ mapM operand2operand inxs
  instructions <- isGEP tenv ret ty' addr' inxs
  return $ map (\instr -> MirM instr ()) instructions

-- ** Conversions
-- We fit everything in size 32 bits, so extensions are trivial
isInstruction _ Nothing (LLVM.SExt _ _  _) = return [] -- without a name is useless
isInstruction _ Nothing (LLVM.ZExt _ _  _) = return [] -- without a name is useless
isInstruction _ (Just ret) (LLVM.SExt op _ _) = lift $ toRTL <$> do
  op' <- operand2operand op
  return $ [MRAM.Imov ret op']
isInstruction _ (Just ret) (LLVM.ZExt op _ _) = lift $ toRTL <$> do
  op' <- operand2operand op
  return $ [MRAM.Imov ret op']
isInstruction _ Nothing (LLVM.BitCast _ _ _) = return $ [] -- without a name is useless
isInstruction _ (Just ret) (LLVM.BitCast op _typ _) = lift $ toRTL <$> do
  op' <- operand2operand op
  return $ [MRAM.Imov ret op']

  
-- *** Not supprted instructions (return meaningfull error)
isInstruction _ _ instr =  implError $ "Instruction: " ++ (show instr)

convertPhiInput :: (LLVM.Operand, LLVM.Name) -> Hopefully $ (MAOperand VReg MWord, Name)
convertPhiInput (op, name) = do
  op' <- operand2operand op
  name' <- name2nameM name
  return (op', name')

-- ** GetElementPtr


{-
llvmSize :: TypeEnv -> LLVM.Type -> Hopefully $ Word

llvmSize LLVM.VoidType = return 0
-- All int types have same size:
-- We don't pay extra for size of memory
llvmSize (LLVM.IntegerType _) = return 1
llvmSize (LLVM.PointerType _ _) = return 1
llvmSize (LLVM.FloatingPointType _) = implError "Floating point type."
llvmSize (LLVM.FunctionType _ _ _) =
  assumptError "Function type have no size. Or do they? If you get this error. please report."
llvmSize (LLVM.VectorType _ _) = implError "Vactor type."
llvmSize (LLVM.StructureType _ _) = implError "Structured type not supported yet. Stay tuned."
llvmSize (LLVM.ArrayType size elemT) = do
  elemSize <- llvmSize elemT
  size' <- return $ wrdFromwrd64 size
  return $ size' * elemSize
--  elemSize <- return (size * llvmSize elemT) 
llvmSize (LLVM.NamedTypeReference name) = implError $ "Named reference: " ++ show name

-- Metadata, labels and token dont have size in memory         
llvmSize LLVM.MetadataType =
  assumptError "Metadata type have no size. Or do they? If you get this error. please report."
llvmSize LLVM.LabelType =
  assumptError "Label type have no size. Or do they? If you get this error. please report."       
llvmSize LLVM.TokenType =
  assumptError "Token type have no size. Or do they? If you get this error. please report."
-}

typeFromOperand :: LLVM.Operand -> Hopefully $ LLVM.Type
typeFromOperand op = return $ LLVM.typeOf op 

--  | Optimized multiplication by a constant
-- If the operand is a constant, statically computes the multiplication
-- If the operand is a register, creates instruction to compute it.
constantMultiplication ::
  MWord
  -> MAOperand VReg MWord
  -> Statefully $ (MAOperand VReg MWord, [MRAM.MA2Instruction VReg MWord])
-- TODO switch to Statefully monad so we can generate a fresh register here
-- TODO support all operand kinds
constantMultiplication c (Const r) =
  return (Const (c*r),[])
constantMultiplication c x = do
  rd <- freshName
  return (Reg rd, [MRAM.Imull rd x (Const c)])


-- Type has to be a pointer
isGEP ::
  LLVMTypeEnv
  -> VReg
  -> LLVM.Type
  -> MAOperand VReg MWord
  -> [LLVM.Operand] -- [MAOperand VReg Word]
  -> Statefully $ [MRAM.MA2Instruction VReg MWord]
isGEP _ _ _ _ [] = assumptError "Getelementptr called with no indices"
isGEP tenv ret (LLVM.PointerType refT _) base (inx:inxs) = do
  typ' <- lift $ type2type tenv refT
  inxOp <- lift $ operand2operand inx
  inxs' <- lift $ mapM operand2operand inxs
  continuation <- isGEP' ret typ' inxs'
  rtemp <- freshName
  return $ [MRAM.Imull rtemp inxOp (Const $ tySize typ'),
            MRAM.Iadd ret (Reg rtemp) base] ++
           continuation
isGEP _ _ llvmTy _ _ =
  assumptError $ "getElementPtr called in a no-pointer type: " ++ show llvmTy
           

-- After first pass every type has to be an agregate type
isGEP' ::
  VReg
  -> Ty
  -> [MAOperand VReg MWord]
  -> Statefully $ [MRAM.MA2Instruction VReg MWord]
isGEP' _ _ [] = return $ []
isGEP' ret (Tarray _ elemsT) (inx:inxs) = do
  (rm, multiplication) <- constantMultiplication (tySize elemsT) inx
  continuation <- isGEP' ret elemsT inxs
  return $ multiplication ++
           -- offset = indes * size type 
           [MRAM.Iadd ret (Reg ret) rm] ++
           continuation
isGEP' ret (Tstruct types) (inx:inxs) = 
  case inx of
    Const i -> do
      offset <- return $ sum $ map tySize $ takeEnum i $ types  
      continuation <- isGEP' ret (types !! (fromEnum i)) inxs -- FIXME add checks for struct bounds
      return $ MRAM.Iadd ret (Reg ret) (Const offset) : continuation
    _ -> assumptError $ "GetElementPtr error. Indices into structs must be constatnts, instead found: " ++
         show inx
isGEP' _ t _ = assumptError $ "getelemptr for non aggregate type: \n" ++ show t ++ "\n"


-- ** Named instructions and instructions lists

isInstrs
  :: LLVMTypeEnv ->  [LLVM.Named LLVM.Instruction]
     -> Statefully $ [MIRInstr () MWord]
isInstrs _ [] = return []
isInstrs tenv instrs = do
  instrs' <- mapM (isNInstruction tenv) instrs
  return $ concat instrs'
  where isNInstruction :: LLVMTypeEnv -> LLVM.Named LLVM.Instruction -> Statefully $ [MIRInstr () MWord]
        isNInstruction tenv (LLVM.Do instr) = isInstruction tenv Nothing instr
        isNInstruction tenv (name LLVM.:= instr) =
          let ret = name2nameM name in
            (\ret -> isInstruction tenv (Just ret) instr) =<< ret






-- -----------------------------------------------






-- ** Selection for Terminator

-- | Instruction Generation for terminators
-- We ignore the name of terminators
isTerminator :: LLVM.Named LLVM.Terminator -> Hopefully $ [MIRInstr () MWord]
isTerminator (_name LLVM.:= term) = do
  termInstr <- isTerminator' term
  return $ termInstr
isTerminator (LLVM.Do term) = do
  termInstr <- isTerminator' term
  return $ termInstr
  
-- Branching

isTerminator' :: LLVM.Terminator -> Hopefully $ [MIRInstr () MWord]
isTerminator' (LLVM.Br name _) = do
  name' <- name2nameM name
  returnRTL $ [MRAM.Ijmp $ Label (show name')] -- FIXME: This works but it's a hack. Think about labels.
isTerminator' (LLVM.CondBr cond name1 name2 _) = do
  cond' <- operand2operand cond
  loc1 <- name2nameM name1
  loc2 <- name2nameM name2 
  returnRTL $ [MRAM.Icmpe cond' (Const 1),
                    MRAM.Icjmp $ Label (show loc1), -- FIXME: This works but it's a hack. Think about labels.
                    MRAM.Ijmp $ Label (show loc2)]
isTerminator' (LLVM.Switch cond deflt dests _ ) = do
  cond' <- operand2operand cond
  deflt' <- name2nameM deflt
  switchInstrs <- mapM (isDest cond') dests
  returnRTL $ (concat switchInstrs) ++ [MRAM.Ijmp (Reg deflt')]
  where isDest cond' (switch,dest) = do
          switch' <- getConstant switch
          dest' <- name2nameM dest
          return [MRAM.Icmpe cond' switch', MRAM.Icjmp (Reg dest')]

  
-- Possible optimisation:
-- Add just one return block, and have all others jump there.
isTerminator' (LLVM.Ret (Just ret) _md) = do
  ret' <- operand2operand ret
  return $ [MirI (RRet $ Just ret') ()]
isTerminator' (LLVM.Ret Nothing _) =
  return $ [MirI (RRet Nothing) ()]

isTerminator' term = implError $ "Terminator not yet supported. \n \t" ++ (show term)


-- | blockJumpsTo : Calculates all the blocks that this block might jump to.
--  By convention the only jumps happen at the end, so they can
--  be easily calculated, but much easier to "keep calculated"
blockJumpsTo' :: LLVM.Terminator -> Hopefully [LLVM.Name]
blockJumpsTo' (LLVM.Ret _ _) = return []
blockJumpsTo' (LLVM.CondBr _ trueDest falseDest _ ) =
  return [trueDest,falseDest]
blockJumpsTo' (LLVM.Br dest _) = return  [dest]
blockJumpsTo' (LLVM.Switch _ defaultDest dests _) = return $ defaultDest : (map snd dests)
blockJumpsTo' (LLVM.IndirectBr _ dests _) = return dests
-- We ignore function calls!!! Only care what block it returns to
-- we also ignore the exeption handling.
blockJumpsTo' (LLVM.Invoke _ _ _ _ _ retDest _exepcDest _ ) = return [retDest]
blockJumpsTo' (LLVM.Resume _ _ ) = return [] -- exception propagation
blockJumpsTo' (LLVM.Unreachable _ ) = return [] -- unreachable
blockJumpsTo' (LLVM.CleanupRet _ _ _) = return [] -- exception propagation
blockJumpsTo' (LLVM.CatchRet _ _ _) = return [] -- exception propagation
blockJumpsTo' (LLVM.CatchSwitch _ _ _ _) = return [] -- exception propagation





dumpName :: (a -> b) -> LLVM.Named a -> b
dumpName f (_ LLVM.:= a) = f a 
dumpName f (LLVM.Do a) = f a 


blockJumpsTo :: LLVM.Named LLVM.Terminator -> Hopefully [Name]
blockJumpsTo term = do
  dests <- (dumpName blockJumpsTo' term)
  mapM name2nameM dests 


-- instruction selection for blocks
isBlock:: LLVMTypeEnv -> LLVM.BasicBlock -> Statefully (BB Name $ MIRInstr () MWord)
isBlock  tenv (LLVM.BasicBlock name instrs term) = do
  body <- isInstrs tenv instrs
  end <- lift $ isTerminator term
  jumpsTo <- lift $ blockJumpsTo term
  name' <- lift $ name2nameM name
  return $ BB name' body end jumpsTo

isBlocks :: LLVMTypeEnv ->  [LLVM.BasicBlock] -> Statefully [BB Name $ MIRInstr () MWord]
isBlocks tenv = mapM (isBlock tenv)

processParams :: ([LLVM.Parameter], Bool) -> [Ty]
processParams (params, _) = map (\_ -> Tint) params

-- | Instruction generation for Functions

isFunction :: LLVMTypeEnv -> LLVM.Definition -> Hopefully $ MIRFunction () MWord
isFunction tenv (LLVM.GlobalDefinition (LLVM.Function _ _ _ _ _ retT name params _ _ _ _ _ _ code _ _)) =
  do
    (body, nextReg) <- runStateT (isBlocks tenv code) initState
    params' <- return $ processParams params
    name' <- name2nameM name
    retT' <- type2type tenv retT
    return $ Function name' retT' params' body nextReg
isFunction _ other = unreachableError $ show other -- Shoudl be filtered out 
  
-- | Instruction Selection for all definitions
-- We create filters to separate the definitions into categories.
-- Then process each category of definition separatedly

-- | Filters
itIsFunc, itIsFuncAttr,itIsGlobVar, itIsTypeDef, itIsMetaData :: LLVM.Definition -> Bool
itIsFunc (LLVM.GlobalDefinition (LLVM.Function  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ )) = True
itIsFunc _ = False

itIsFuncAttr (LLVM.FunctionAttributes _ _) = True
itIsFuncAttr _ = False

itIsGlobVar (LLVM.GlobalDefinition (LLVM.GlobalVariable _name _ _ _ _ _ _ _ _ _ _ _ _ _)) = True
itIsGlobVar _ = False

itIsTypeDef (LLVM.TypeDefinition _ _) = True
itIsTypeDef _ = False

itIsMetaData (LLVM.MetadataNodeDefinition _ _) = True
itIsMetaData (LLVM.NamedMetadataDefinition _ _) = True
itIsMetaData _ = False

unreachableError :: MonadError CmplError m => [Char] -> m b
unreachableError what = otherError $ "This is akward. This error should be unreachable. You called a function that should only be called on a list after filtering, to avoid this error. Here is the info: " ++ what

-- ** Instruction selection for each of those filtered definitions

-- | We check that we are not discarding anything we care about
-- We allow discarding metadata for now...
  
checkDiscardedDefs :: [LLVM.Definition] -> Hopefully ()
checkDiscardedDefs defs = do
  _ <- mapM checkDiscardedDef defs
  return ()
  where checkDiscardedDef :: LLVM.Definition -> Hopefully ()
        checkDiscardedDef def = if acceptedDef def
                                then return ()
                                else implError $
                                     "Definition: " ++
                                     (show def) ++
                                     ".\n While checking discarded defs "
        acceptedDef = fOr
          [itIsFunc, itIsFuncAttr, itIsGlobVar, itIsTypeDef, itIsMetaData]
        fOr ::  [a -> Bool] -> (a -> Bool)
        fOr fs a = or $ map (\f -> f a) fs 


-- | Computes the type environment.
{- We do it lazily in tthree steps, to support recursive types:
   1 - First we just gather the map1: LLVM.Name -> LLVM.Type
   2 - Then we traverse that map converting it to map2: LLVM.Name -> Ty
       This second pass calls the original map on any recursive calls
   3 - Then we just change the keys to get map3 : Name -> Ty
-}

type LLVMTypeEnv = Map.Map LLVM.Name LLVM.Type

isTypeDefs :: [LLVM.Definition] -> Hopefully $ LLVMTypeEnv
isTypeDefs defs = do
  map1 <- Map.fromList <$> mapM def2pair defs
  return map1
  where def2pair :: LLVM.Definition -> Hopefully $ (LLVM.Name, LLVM.Type)
        def2pair (LLVM.TypeDefinition  name (Just ty)) = return (name, ty)
        def2pair (LLVM.TypeDefinition  name Nothing) =
          assumptError $ "Received an empty type definition for " ++
          show name ++
          " what am I supposed to do with this?"
        def2pair other = unreachableError $ show other
        
-- | Turns a Global variable into its descriptor.
isGlobVars :: LLVMTypeEnv -> [LLVM.Definition] -> Hopefully $ GEnv MWord
isGlobVars tenv defs = mapMaybeM (isGlobVar' tenv) defs
  where isGlobVar' tenv  (LLVM.GlobalDefinition g) = do
          flatGVar <- isGlobVar tenv  g
          return $ Just flatGVar 
        isGlobVar' _ _ = return Nothing

isGlobVar :: LLVMTypeEnv -> LLVM.Global -> Hopefully $ GlobalVariable MWord
isGlobVar tenv (LLVM.GlobalVariable name _ _ _ _ _ const typ _ init sectn _ _ _) = do
  typ' <- type2type tenv typ
  init' <- flatInit init
  -- TODO: Want to check init' is the right length?
  return $ GlobalVariable (gName2String name) const typ' init' (sectionIsSecret sectn)
  where flatInit :: Maybe LLVM.Constant.Constant ->
                    Hopefully $ Maybe [MWord]
        flatInit Nothing = return Nothing
        flatInit (Just const) = do
          const' <- flattenConstant const
          return $ Just const'

        sectionIsSecret (Just "__DATA,__secret") = True
        sectionIsSecret (Just ".data.secret") = True
        sectionIsSecret _ = False
isGlobVar _ other = unreachableError $ show other

flattenConstant :: LLVM.Constant.Constant ->
                   Hopefully [MWord]
flattenConstant (LLVM.Constant.Int _ n) = return $ [fromInteger n]
flattenConstant (LLVM.Constant.Null _typ) = return $ [0]
flattenConstant (LLVM.Constant.Array _typ cnts) = do
  cnts' <- mapM flattenConstant cnts
  return $ concat cnts'
flattenConstant (LLVM.Constant.Struct name True _) =
  implError $ "Packed structs not supported yet. Tried packing constant struct \n " ++
  show name
flattenConstant (LLVM.Constant.Struct _name False cnts) = do
  cnts' <- mapM flattenConstant cnts
  return $ concat cnts'  --
flattenConstant cnt = assumptError $ "Constant not supportet for flattening: \n " ++
                      show cnt
                        
isFuncAttributes :: [LLVM.Definition] -> Hopefully $ () -- TODO can we use this attributes?
isFuncAttributes _ = return () 

isDefs :: [LLVM.Definition] -> Hopefully $ MIRprog () MWord
isDefs defs = do
  typeDefs <- isTypeDefs $ filter itIsTypeDef defs
  globVars <- (isGlobVars typeDefs) $ filter itIsGlobVar defs
  --otherError $ "DEBUG HERE: \n" ++ show typeDefs ++ "\n" ++ show globVars ++ "\n"  
  _funcAttr <- isFuncAttributes $ filter itIsFuncAttr defs
  funcs <- mapM (isFunction typeDefs) $ filter itIsFunc defs
  checkDiscardedDefs defs -- Make sure we dont drop something important
  return $ IRprog Map.empty globVars funcs
  
-- | Instruction selection generates an RTL Program
instrSelect :: LLVM.Module -> Hopefully $ MIRprog () MWord
instrSelect (LLVM.Module _ _ _ _ defs) = isDefs defs

