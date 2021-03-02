{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

{-|
Module      : Instruction Selection
Description : LLVM -> RTL
Maintainer  : santiago@galois.com
Stability   : prototype

Instruction selection translates LLVM to MicroIR. It's a linear pass that translates each
LLVM instruction to 0 or MicroIR instructinos. For now, it does not combine
instructinos.

As we do the instructino selections WE KEEP THE DAG INFORMATION by
annotating each block with all the blocks it can jump to. This could
be reversed (annotate blocks by those blocks that jump to it) in
a single pass. 

-}

module Compiler.InstructionSelection
    ( instrSelect,
    ) where

import Data.Bits
import qualified Data.ByteString.Short as Short

import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Map as Map 

import Control.Monad.Except
import Control.Monad.State.Lazy

import qualified Data.Set as Set

import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Constant as LLVM.Constant
import qualified LLVM.AST.IntegerPredicate as IntPred

import Compiler.Errors
import Compiler.Common
import Compiler.IRs
import Compiler.Layout
import Compiler.LazyConstants
import Compiler.TraceInstrs
import Compiler.TypeOf
import Util.Util

import MicroRAM (MWord, MemWidth(..), pattern WWord, widthInt, wordBytes)
import qualified MicroRAM as MRAM

{- Notes on this instruction generation :

   TO DOs:
   1. Check exception handeling, I'm  not sure I'm translating that correctly.
      Particulary, do we include exeption jumpin in DAGS? Right now we don't
   2. 

-}

-- | Environment to keep track of global and type definitions
data Env = Env {llvmtTypeEnv :: LLVMTypeEnv, globs :: Set.Set LLVM.Name}

-- ** Translation between LLVM and RTL "things"
any2short :: Show a => a -> Short.ShortByteString
any2short n = Short.toShort $ BSU.fromString $ show $ n

name2name :: LLVM.Name -> Name
name2name (LLVM.Name s) = Name s
name2name (LLVM.UnName n) = Name $ any2short n

name2nameM :: Monad m => LLVM.Name -> m Name
name2nameM nm = return $ name2name nm

name2label :: Monad m => LLVM.Name -> m $ MAOperand VReg MWord
name2label nm = return $ Label $ show $ name2name nm

getConstant :: Env -> LLVM.Constant.Constant -> Hopefully $ MAOperand VReg MWord
getConstant env c = LImm <$> constant2OnelazyConst env c

operand2operand :: Env -> LLVM.Operand -> Hopefully $ MAOperand VReg MWord
operand2operand env (LLVM.ConstantOperand c) = getConstant env c
operand2operand _env (LLVM.LocalReference _ name') = AReg <$> name2nameM name'
operand2operand _ _= implError "operand, probably metadata"

-- | Get the value of `op`, masking off high bits if necessary to emulate
-- truncation to the appropriate width (determined by the LLVM type of `op`).
-- | Similar to isTruncate
operand2operandTrunc :: Env
                     -> LLVM.Operand
                     -> Statefully (MAOperand VReg MWord, [MA2Instruction VReg MWord])
operand2operandTrunc env op = do
  op' <- lift $ operand2operand env op
  case typeOf (llvmtTypeEnv env) op of
    LLVM.IntegerType w | w < 64 -> do
      tmpReg <- freshName
      let extra = MRAM.Iand tmpReg op' (LImm $ SConst $ (1 `shiftL` fromIntegral w) - 1)
      return (AReg tmpReg, [extra])
    _ -> return (op', [])



-- | Transforms `LLVM.Type` into backend types `Ty`
-- Note that LLVM types can be recursive. However, in well-typed LLVMS,
-- all structs have a computable size. This ensures that there are no
-- infinite loops and that `type2type` terminates.
-- Unfortunately this property is not enforced by the type system.
type2type :: LLVMTypeEnv -> LLVM.Type -> Hopefully Ty
type2type _ LLVM.VoidType = return TVoid -- FIXME check size!
type2type _ (LLVM.IntegerType _n) = return Tint -- FIXME check size! 
type2type _tenv (LLVM.PointerType _t _) = return Tptr 
type2type _ (LLVM.FunctionType {}) = return Tint -- FIXME enrich typed!
type2type tenv' (LLVM.ArrayType size elemT) = do
  elemT' <- type2type tenv' elemT
  size' <- return $ size -- wrdFromwrd64 
  return $ Tarray size' elemT'
--type2type _ (LLVM.StructureType True _) = assumptError "Can't pack structs yet."
type2type tenv' (LLVM.StructureType _ tys) = Tstruct <$> mapM (type2type tenv') tys
type2type tenv' (LLVM.NamedTypeReference name) = do
  ty <- typeDef tenv' name
  type2type tenv' ty
type2type _ t = implError $ "Type conversion of the following llvm type: \n \t " ++ (show t)

typeDef :: LLVMTypeEnv -> LLVM.Name -> Hopefully LLVM.Type  
typeDef tenv' name =
  case Map.lookup name tenv' of
    Just ty -> return $ ty
    Nothing -> assumptError $ "Type not defined: \n \t" ++ show name ++ ".\n" 

-- | toRTL lifts simple MicroRAM instruction into RTL.
toRTL :: [MA2Instruction VReg MWord] -> [MIRInstr () MWord]
toRTL = map  $ \x -> MirM x ()

returnRTL :: Monad m => [MA2Instruction VReg MWord] -> m [MIRInstr () MWord]
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


type BinopInstruction = VReg -> MAOperand VReg MWord -> MAOperand VReg MWord ->
  MA2Instruction VReg MWord
isBinop
  :: Env
     -> Maybe VReg
     -> LLVM.Operand
     -> LLVM.Operand
     -> BinopInstruction
     -> Statefully [MIRInstr () MWord]
isBinop env ret op1 op2 bopisBinop = lift $ toRTL <$> isBinop' ret op1 op2 bopisBinop
  where isBinop' ::
          Maybe VReg
          -> LLVM.Operand
          -> LLVM.Operand
          -> BinopInstruction
          -> Hopefully $ [MA2Instruction VReg MWord]
        isBinop' Nothing _ _ _ = Right [] --  without return is a noop
        isBinop' (Just ret') op1' op2' bop = do
          a <- operand2operand env op1'
          b <- operand2operand env op2'
          return [bop ret' a b]
  
-- ** Comparisons

-- | Instruction selection for comparisons
predicate2instructuion
  :: IntPred.IntegerPredicate
     -> regT
     -> MAOperand regT wrdT
     -> MAOperand regT wrdT
     -> [MA2Instruction regT wrdT]
predicate2instructuion inst r op1 op2 =
  case inst of
  IntPred.EQ  -> [MRAM.Icmpe r op1 op2]
  IntPred.NE  -> [MRAM.Icmpe r op1 op2, MRAM.Inot r (AReg r)]
-- Unsigned                                        r
  IntPred.UGT -> [MRAM.Icmpa  r op1 op2] 
  IntPred.UGE -> [MRAM.Icmpae r op1 op2] 
  IntPred.ULT -> [MRAM.Icmpa  r op2 op1] --FLIPED
  IntPred.ULE -> [MRAM.Icmpae r op2 op1] --FLIPED
-- Signed
  IntPred.SGT -> [MRAM.Icmpg  r op1 op2] 
  IntPred.SGE -> [MRAM.Icmpge r op1 op2] 
  IntPred.SLT -> [MRAM.Icmpg  r op2 op1]  --FLIPED
  IntPred.SLE -> [MRAM.Icmpge r op2 op1]  --FLIPED


floatError :: MonadError CmplError m => m a
floatError = implError "Floating point arithmetic"

_constzero,constOne :: LLVM.Operand
_constzero = LLVM.ConstantOperand (LLVM.Constant.Int (toEnum 0) 0)
constOne = LLVM.ConstantOperand (LLVM.Constant.Int (toEnum 1) 1)

-- *** Trtanslating Function parameters and types

function2function
  :: LLVMTypeEnv -> Either a LLVM.Operand -> Hopefully (MAOperand VReg MWord, Ty, [Ty])
function2function _ (Left _ ) = implError $ "Inlined assembly not supported"
function2function tenv (Right (LLVM.LocalReference ty nm)) = do
  nm' <- name2nameM nm
  (retT', paramT') <- functionTypes tenv ty
  return (AReg nm',retT',paramT')
function2function tenv (Right (LLVM.ConstantOperand (LLVM.Constant.GlobalReference ty nm))) = do
  lbl <- name2label nm
  (retT', paramT') <- functionPtrTypes ty
  return (lbl,retT',paramT')
  where functionPtrTypes :: LLVM.Type -> Hopefully (Ty, [Ty])
        functionPtrTypes (LLVM.PointerType funTy _) = functionTypes tenv funTy
        functionPtrTypes ty = implError $ "Function pointer type expected found "  ++ show ty ++ " instead." 
function2function _tenv (Right (LLVM.ConstantOperand c)) =
  implError $ "Calling a function with a constant. You called: \n \t" ++ show c
function2function _ (Right op) = 
  implError $ "Calling a function with unsuported operand. You called: \n \t" ++ show op

functionTypes :: LLVMTypeEnv ->  LLVM.Type -> Hopefully (Ty, [Ty])
functionTypes tenv' (LLVM.FunctionType retTy argTys False) = do
  retT' <- type2type  tenv' retTy
  paramT' <- mapM (type2type tenv') argTys
  return (retT',paramT')
functionTypes _tenv (LLVM.FunctionType  _ _ True) =
  implError "Variable parameters (isVarArg in function call)."
functionTypes _ ty =  assumptError $ "Function type expected found " ++ show ty ++ " instead."

-- | Process parameters into RTL format
-- WE dump the attributes

params2params
  :: Traversable t =>
     Env
  -> t (LLVM.Operand, b)
  -> Either CmplError (t (MAOperand VReg MWord))
params2params env params  = do
  params' <- mapM ((operand2operand env) . fst) params -- fst dumps the attributes
  return params' 

------------------------------------------------------
-- * Instruction selection for instructions

-- | Instruction Selection for single LLVM instructions 
    

isInstruction :: Env -> Maybe VReg -> LLVM.Instruction -> Statefully $ [MIRInstr () MWord]
-- *** Arithmetic
isInstruction env ret instr =
  case instr of
    -- Arithmetic
    (LLVM.Add _ _ o1 o2 _)   -> isBinop env ret o1 o2 MRAM.Iadd
    (LLVM.Sub _ _ o1 o2 _)   -> isBinop env ret o1 o2 MRAM.Isub
    (LLVM.Mul _ _ o1 o2 _)   -> isBinop env ret o1 o2 MRAM.Imull
    (LLVM.SDiv _ _ _o1 _o2 ) -> implError "Signed division is hard! SDiv"
    (LLVM.SRem _o1 _o2 _)    -> implError "Signed division is hard! SRem"
    (LLVM.FAdd _ _o1 _o2 _)  -> floatError
    (LLVM.FSub _ _o1 _o2 _)  -> floatError
    (LLVM.FMul _ _o1 _o2 _)  -> floatError
    (LLVM.FDiv _ _o1 _o2 _)  -> floatError
    (LLVM.FRem _ _o1 _o2 _)  -> floatError
    (LLVM.UDiv _ o1 o2 _)    -> isBinop env ret o1 o2 MRAM.Iudiv -- this is easy
    (LLVM.URem o1 o2 _)      -> isBinop env ret o1 o2 MRAM.Iumod -- this is eay
    -- Binary
    (LLVM.Shl _ _ o1 o2 _)   -> isBinop env ret o1 o2 MRAM.Ishl
    (LLVM.LShr _ o1 o2 _)    -> isBinop env ret o1 o2 MRAM.Ishr
    (LLVM.AShr _ o1 o2 _)    -> isArithShr env ret o1 o2
    (LLVM.And o1 o2 _)       -> isBinop env ret o1 o2 MRAM.Iand
    (LLVM.Or o1 o2 _)        -> isBinop env ret o1 o2 MRAM.Ior
    (LLVM.Xor o1 o2 _)       -> isBinop env ret o1 o2 MRAM.Ixor
    -- Memory
    (LLVM.Alloca ty Nothing _ _) -> isAlloca env ret ty constOne -- NumElements is defaulted to be one.
    (LLVM.Alloca ty (Just size) _ _) -> isAlloca env ret ty size
    -- TODO: check alignment - MicroRAM memory ops are always aligned, so
    -- unaligned LLVM loads/stores need special handling
    (LLVM.Load _ n _ align _)
      | fromIntegral align >= alignOf (llvmtTypeEnv env) (pointee (typeOf (llvmtTypeEnv env) n)) ->
        withReturn ret $ isLoad env n
      | otherwise -> withReturn ret $ isLoadUnaligned env n
    (LLVM.Store _ adr cont _ align _)
      | fromIntegral align >= alignOf (llvmtTypeEnv env) (pointee (typeOf (llvmtTypeEnv env) adr)) ->
        isStore env adr cont
      | otherwise -> isStoreUnaligned env adr cont
    -- Other
    (LLVM.ICmp pred op1 op2 _) -> withReturn ret $ isCompare env pred op1 op2
    (LLVM.Call _ _ _ f args _ _ ) -> lift $ isCall env ret f args
    (LLVM.Phi _typ ins _)  ->  withReturn ret $ isPhi env ins
    (LLVM.Select cond op1 op2 _)  -> withReturn ret $ isSelect env cond op1 op2 
    (LLVM.GetElementPtr _ addr inxs _) -> withReturn ret $ isGEP env addr inxs
    (LLVM.ExtractValue _ _ _ )   -> lift $ return $ makeTraceInvalid
    -- Transformers
    (LLVM.SExt op _ _)       -> lift $ toRTL <$> withReturn ret (isMove env op) 
    (LLVM.ZExt op _ _)       -> lift $ toRTL <$> withReturn ret (isMove env op)
    (LLVM.PtrToInt op _ty _) -> lift $ toRTL <$> withReturn ret (isMove env op)
    (LLVM.IntToPtr op _ty _) -> lift $ toRTL <$> withReturn ret (isMove env op)
    (LLVM.BitCast op _typ _) -> lift $ toRTL <$> withReturn ret (isMove env op)
    (LLVM.Trunc op ty _ )    -> lift $ toRTL <$> withReturn ret (isTruncate env op ty)
    -- Exceptions
    (LLVM.LandingPad _ _ _ _ ) -> lift $ return $ makeTraceInvalid
    (LLVM.CatchPad _ _ _)      -> lift $ return $ makeTraceInvalid
    (LLVM.CleanupPad _ _ _ )   -> lift $ return $ makeTraceInvalid
    instr ->  implError $ "Instruction: " ++ (show instr)
  where withReturn Nothing _ = return $ []
        withReturn (Just ret) f = f ret

        pointee (LLVM.NamedTypeReference name) = case Map.lookup name (llvmtTypeEnv env) of
          Just x -> pointee x
          Nothing -> error $ "failed to resolve named type " ++ show name
        pointee (LLVM.PointerType ty _) = ty
        pointee ty = error $ "isInstruction: expected pointer, but got " ++ show ty

{- | Implements arithmetic shift right in terms of other binary operations like so:
@
   int s = -((unsigned) x >> (wrdsize - 1));
   int sar = (s^x) >> n ^ s;
@
or
@
 nsign = (shr x wrdsize)
 sign  = nsign * - 1
 ret''  = (xor sign x)
 ret'   = shr ret' n
 ret    = xor ret' sign
-}

isArithShr :: Env
     -> Maybe VReg
     -> LLVM.Operand
     -> LLVM.Operand
     -> Statefully [MIRInstr () MWord]
isArithShr _ Nothing _ _ = return []
isArithShr env (Just ret) o1 o2 = do
  o1' <- lift $ operand2operand env o1
  o2' <- lift $ operand2operand env o2
  nsign <- freshName
  sign  <- freshName
  ret'' <- freshName
  ret'  <- freshName
  returnRTL $
    [ MRAM.Ishr nsign o1' (LImm $ SConst $ fromIntegral width - 1),
      MRAM.Imull sign (AReg nsign)
        (LImm $ SConst $ complement 0 `shiftR` (64 - fromIntegral width)),
      MRAM.Ixor  ret'' (AReg sign) o1',
      MRAM.Ishr  ret'  (AReg ret'') o2',
      MRAM.Ixor  ret (AReg ret') (AReg sign)]
  where
    width = case typeOf (llvmtTypeEnv env) o1 of
      LLVM.IntegerType bits -> bits
      ty -> error $ "don't know how to do ashr on non-integer type " ++ show ty

    
-- *** Memory operations
-- Alloca
{- we ignore type, alignment and metadata and assume we are storing integers,
   we only look at the numElements.
   In fact (currently) we dont check  stack overflow or any similar safety check
   Also the current granularity of our memory is per word so ptr arithmetic and alignment are trivial. -}
isAlloca
  :: Env
     -> Maybe VReg
     -> LLVM.Type
     -> LLVM.Operand
     -> Statefully $ [MIRInstruction () VReg MWord]
isAlloca env ret ty count = lift $ do
  let tySize = sizeOf (llvmtTypeEnv env) ty
  count' <- operand2operand env count
  return [MirI (RAlloc ret tySize count') ()]

pointerOperandWidth :: Env -> LLVM.Operand -> Hopefully MRAM.MemWidth
pointerOperandWidth env op = case resolve (llvmtTypeEnv env) $ typeOf (llvmtTypeEnv env) op of
  LLVM.PointerType ty _ -> case resolve (llvmtTypeEnv env) ty of
    LLVM.IntegerType bits -> case bits of
        8 -> return MRAM.W1
        16 -> return MRAM.W2
        32 -> return MRAM.W4
        64 -> return MRAM.W8
        _ -> progError $ "bad memory access width: " ++ show bits ++ " bits"
    LLVM.PointerType _ _ -> return MRAM.WWord
    ty -> progError $ "bad pointee type in memory op: " ++ show ty
  ty -> progError $ "bad pointer type in memory op: " ++ show ty

-- Load
isLoad
  :: Env -> LLVM.Operand -> VReg -> Statefully $ [MIRInstr () MWord]
isLoad env n ret = lift $ do 
  a <- operand2operand env n
  w <- pointerOperandWidth env n
  returnRTL $ (MRAM.Iload w ret a) : []

isLoadUnaligned
  :: Env -> LLVM.Operand -> VReg -> Statefully $ [MIRInstr () MWord]
isLoadUnaligned env n ret = do
  ptr <- lift $ operand2operand env n
  w <- lift $ pointerOperandWidth env n

  offset <- freshName
  ptr0 <- freshName
  ptr1 <- freshName
  val0 <- freshName
  val1 <- freshName
  shift <- freshName
  flag <- freshName

  -- The unaligned load from `ptr` may span two `w`-sized values.  We load from
  -- `ptr` rounded down (`ptr0`) and `ptr` rounded up (`ptr1`), then stitch the
  -- two halves together

  let lowMask :: MWord
      lowMask = fromIntegral $ widthInt w - 1
  let highMask :: MWord
      highMask = complement $ fromIntegral $ widthInt w - 1
  let valMask :: MWord
      valMask = (1 `shiftL` widthInt w) - 1

  returnRTL $
    -- Compute the offset by which the access is unaligned
    [ MRAM.Iand offset ptr (LImm $ SConst lowMask)
    -- Load the two surrounding words, `val0` and `val1`
    , MRAM.Iand ptr0 ptr (LImm $ SConst highMask)
    , MRAM.Iload w val0 (AReg ptr0)
    , MRAM.Iadd ptr1 (AReg ptr0) (LImm $ fromIntegral $ widthInt w)
    , MRAM.Iload w val1 (AReg ptr1)
    -- Shift `val0` down and `val1` up to their final positions.  Note the
    -- extra bits will be zero-filled, so we can just OR the two together
    -- afterward.
    , MRAM.Imull shift (AReg offset) (LImm 8)
    , MRAM.Ishr val0 (AReg val0) (AReg shift)
    , MRAM.Isub shift (LImm $ fromIntegral $ 8 * widthInt w) (AReg shift)
    , MRAM.Ishl val1 (AReg val1) (AReg shift)
    ] ++
    -- If `w` is word size and `offset` is zero, then the shift of `val1` might
    -- have an out-of-range shift amount (equal to the bit width of a word).
    -- In that case we need to explicitly zero `val1`.
    --
    -- If `w` is not word size, then we need to mask off the high bits of
    -- `val1`, so that the result placed into `ret` is actually `w` bytes wide.
    (if w == WWord then
      [ MRAM.Icmpe flag (AReg offset) (LImm 0)
      , MRAM.Icmov val1 (AReg flag) (LImm 0)
      ]
    else
      [ MRAM.Iand val1 (AReg val1) (LImm $ SConst valMask)
      ]
    ) ++
    [ MRAM.Ior ret (AReg val0) (AReg val1)
    ]


-- | Store
{- Store yields void so we can will ignore the return location -}
isStore
  :: Env
     -> LLVM.Operand
     -> LLVM.Operand
     -> Statefully $ [MIRInstr () MWord]
isStore env adr cont = do
  cont' <- lift $ operand2operand env cont
  adr' <- lift $ operand2operand env adr
  w <- lift $ pointerOperandWidth env adr
  returnRTL [MRAM.Istore w adr' cont']

isStoreUnaligned
  :: Env
     -> LLVM.Operand
     -> LLVM.Operand
     -> Statefully $ [MIRInstr () MWord]
isStoreUnaligned env adr cont = do
  ptr <- lift $ operand2operand env adr
  val <- lift $ operand2operand env cont
  w <- lift $ pointerOperandWidth env adr

  offset <- freshName
  shift <- freshName
  val0 <- freshName
  val1 <- freshName
  mask0 <- freshName
  mask1 <- freshName
  ptr0 <- freshName
  ptr1 <- freshName
  old0 <- freshName
  old1 <- freshName
  flag <- freshName

  let lowMask :: MWord
      lowMask = fromIntegral $ widthInt w - 1
  let highMask :: MWord
      highMask = complement $ fromIntegral $ widthInt w - 1
  let valMask :: MWord
      valMask = (1 `shiftL` (8 * widthInt w)) - 1

  returnRTL $
    [ MRAM.Iand offset ptr (LImm $ SConst lowMask)
    -- Shift `val` and `valMask` into position.
    , MRAM.Imull shift (AReg offset) (LImm 8)
    , MRAM.Ishl val0 val (AReg shift)
    , MRAM.Ishl mask0 (LImm $ SConst valMask) (AReg shift)
    , MRAM.Isub shift (LImm $ fromIntegral $ 8 * widthInt w) (AReg shift)
    , MRAM.Ishr val1 val (AReg shift)
    , MRAM.Ishr mask1 (LImm $ SConst valMask) (AReg shift)
    ] ++
    (if w == WWord then
      [ MRAM.Icmpe flag (AReg offset) (LImm 0)
      , MRAM.Icmov mask1 (AReg flag) (LImm 0)
      ]
    else
      -- It's okay to mangle the high bits of `val0`, since sub-word-sized
      -- writes only write the lower `w` bytes.
      []
    ) ++
    -- Read-modify-write the two surrounding words.  The parts indicated by
    -- `mask0`/`mask1` are updated to contain `val0`/`val1`, while the rest
    -- remains untouched.
    [ MRAM.Iand ptr0 ptr (LImm $ SConst highMask)
    , MRAM.Iload w old0 (AReg ptr0)
    , MRAM.Inot mask0 (AReg mask0)
    , MRAM.Iand old0 (AReg old0) (AReg mask0)
    , MRAM.Ior old0 (AReg old0) (AReg val0)
    , MRAM.Istore w (AReg ptr0) (AReg old0)

    , MRAM.Iadd ptr1 (AReg ptr0) (LImm $ fromIntegral $ widthInt w)
    , MRAM.Iload w old1 (AReg ptr1)
    , MRAM.Inot mask1 (AReg mask1)
    , MRAM.Iand old1 (AReg old1) (AReg mask1)
    , MRAM.Ior old1 (AReg old1) (AReg val1)
    , MRAM.Istore w (AReg ptr1) (AReg old1)
    ]

-- *** Compare

isCompare
  :: Env
     -> IntPred.IntegerPredicate
     -> LLVM.Operand
     -> LLVM.Operand
     -> VReg
     -> Statefully $ [MIRInstr () MWord]
isCompare env pred' op1 op2 ret = do
  (lhs, lhsPre) <- operand2operandTrunc env op1
  (rhs, rhsPre) <- operand2operandTrunc env op2
  comp' <- lift $ return $ predicate2instructuion pred' ret lhs rhs -- Do the comparison
  returnRTL $ lhsPre ++ rhsPre ++ comp'
                        
-- *** Function Call 
isCall
  :: Env
     -> Maybe VReg
     -> Either a LLVM.Operand
     -> [(LLVM.Operand, b)]
     -> Hopefully $ [MIRInstruction () VReg MWord]
isCall env ret f args = do
  (f',retT,paramT) <- function2function (llvmtTypeEnv env) f
  args' <- params2params env args
  return $
    maybeTraceIR ("call " ++ show f') ([optRegName ret, f'] ++ args') ++
    [MirI (RCall retT ret f' paramT args') ()]

        
-- *** Phi
isPhi
  :: Env
  -> [(LLVM.Operand, LLVM.Name)]
  -> VReg
  -> Statefully $ [MIRInstruction () VReg MWord]
isPhi env ins ret = lift $ do
  ins' <- mapM (convertPhiInput env) ins
  return [MirI (RPhi ret ins') ()]

isSelect
  :: Env
  -> LLVM.Operand
  -> LLVM.Operand
  -> LLVM.Operand
  -> VReg
  -> Statefully $ [MIRInstr () MWord]
isSelect env cond op1 op2 ret = lift $ toRTL <$> do
   cond' <- operand2operand env cond
   op1' <- operand2operand env op1 
   op2' <- operand2operand env op2 
   return [MRAM.Imov ret op2', MRAM.Icmov ret cond' op1']

-- *** GetElementPtr 
isGEP
  :: Env
  -> LLVM.Operand
  -> [LLVM.Operand]
  -> VReg
  -> Statefully $ [MIRInstruction () VReg MWord]
isGEP  env addr inxs ret = do
  addr' <- lift $ operand2operand env addr
  ty' <- lift $ typeFromOperand env addr
  -- inxs' <- lift $ mapM operand2operand env inxs
  instructions <- isGEPptr env ret ty' addr' inxs
  return $ map (\instr -> MirM instr ()) instructions
  where isGEPptr
          :: Env
          -> VReg
          -> LLVM.Type
          -> MAOperand VReg MWord
          -> [LLVM.Operand] -- [MAOperand VReg Word]
          -> Statefully $ [MA2Instruction VReg MWord]
        isGEPptr _ _ _ _ [] = assumptError "Getelementptr called with no indices"
        isGEPptr env ret (LLVM.PointerType refT _x) base (inx:inxs) = do
          _typ' <-  lift $ type2type (llvmtTypeEnv env) refT
          inxOp <- lift $ operand2operand env inx
          inxs' <- lift $ mapM (operand2operand env) inxs
          continuation <- isGEPaggregate env ret refT inxs'
          rtemp <- freshName
          return $ [MRAM.Imull rtemp inxOp (LImm $ SConst $ sizeOf (llvmtTypeEnv env) refT),
                  MRAM.Iadd ret (AReg rtemp) base] ++
            continuation
        isGEPptr _ _ llvmTy _ _ =
          assumptError $ "getElementPtr called in a no-pointer type: " ++ show llvmTy
isGEPaggregate
  :: Env
  -> VReg
  -> LLVM.Type
  -> [MAOperand VReg MWord]
  -> Statefully $ [MA2Instruction VReg MWord]
isGEPaggregate _ _ _ [] = return []
isGEPaggregate env ret (LLVM.ArrayType _ elemsT) (inx:inxs) = do
  (rm, multiplication) <- constantMultiplication (sizeOf (llvmtTypeEnv env) elemsT) inx
  continuation <- isGEPaggregate env ret elemsT inxs
  return $ multiplication ++
  -- offset = indes * size type 
    [MRAM.Iadd ret (AReg ret) rm] ++
    continuation
isGEPaggregate env ret (LLVM.StructureType packed types) (inx:inxs) = 
  case inx of
    (LImm (SConst i)) -> do
      new_type <- return $ types !! (fromEnum i)
      offset <- if packed then
                  return $ sum $ map (sizeOf $ llvmtTypeEnv env) $ takeEnum i $ types
                else
                  return $ offsetOfStructElement (llvmtTypeEnv env) new_type $ (takeEnum i) $ types
      continuation <- isGEPaggregate env ret (new_type) inxs -- FIXME add checks for struct bounds
      return $ MRAM.Iadd ret (AReg ret) (LImm $ SConst offset) : continuation
    (LImm lc) -> assumptError $ unexpectedLazyIndexMSG ++ show lc 
    _ -> assumptError $ unexpectedNotConstantIndexMSG ++ show inx
  where unexpectedLazyIndexMSG = "GetElementPtr error. Indices into structs must be constatnts that do not depend on global references. we can probably fix this, but did not expect tit to show up, please report. /n /t Index to gep was: \n \t"
        unexpectedNotConstantIndexMSG = "GetElementPtr error. Indices into structs must be constatnts, instead found: "
isGEPaggregate env ret (LLVM.NamedTypeReference name) inx = do
  typ <- lift $ typeDef (llvmtTypeEnv env) name
  isGEPaggregate env ret typ inx
isGEPaggregate _ _ t _ = assumptError $ "getelemptr for non aggregate type: \n" ++ show t ++ "\n"



-- similar to operand2operandTrunc
isTruncate :: Env
           -> LLVM.Operand
           -> LLVM.Type
           -> VReg
           -> Hopefully [MA2Instruction VReg MWord]
isTruncate env op ty ret = do
  op' <- operand2operand env op
  case ty of
    LLVM.IntegerType w | w < 64 ->
      return [MRAM.Iand ret op' (LImm $ SConst $ (1 `shiftL` fromIntegral w) - 1)]
    _ -> assumptError $ "Can't truncate non integer type " ++ show ty 
  

                       
-- ** Conversions
-- We fit everything in size 32 bits, so extensions are trivial
isMove :: Env -> LLVM.Operand -> VReg -> Hopefully $ [MA2Instruction VReg MWord]
isMove env op ret = -- lift $ toRTL <$>
  do op' <- operand2operand env op
     return $ smartMove ret op'
  
-- | Optimize away the move if it's to the same register
smartMove
  :: Eq regT
  => regT
  -> MAOperand regT wrdT
  -> [MRAM.Instruction' regT operand1 (MAOperand regT wrdT)]
smartMove ret op = if (checkEq op ret) then [] else [MRAM.Imov ret op]
  where checkEq op r = case op of
                         AReg r0 -> r0 == r  
                         _ -> False   

-- ** Exeptions 
  
-- *** Not supprted instructions (return meaningfull error)
{-isInstruction _env _ instr =  implError $ "Instruction: " ++ (show instr)
-}


------------------------------------------------------
-- * Utils for instructions selection of instructions


convertPhiInput :: Env -> (LLVM.Operand, LLVM.Name) -> Hopefully $ (MAOperand VReg MWord, Name)
convertPhiInput env (op, name) = do
  op' <- operand2operand env op
  name' <- name2nameM name
  return (op', name')

typeFromOperand :: Env -> LLVM.Operand -> Hopefully $ LLVM.Type
typeFromOperand env op = return $ typeOf (llvmtTypeEnv env) op 

--  | Optimized multiplication by a constant
-- If the operand is a constant, statically computes the multiplication
-- If the operand is a register, creates instruction to compute it.
constantMultiplication ::
  MWord
  -> MAOperand VReg MWord
  -> Statefully $ (MAOperand VReg MWord, [MA2Instruction VReg MWord])
-- TODO switch to Statefully monad so we can generate a fresh register here
-- TODO support all operand kinds
constantMultiplication c (LImm r) =
  return (LImm $ (SConst c)*r,[])
constantMultiplication c x = do
  rd <- freshName
  return (AReg rd, [MRAM.Imull rd x (LImm $ SConst c)])





-- ** Named instructions and instructions lists

isInstrs
  :: Env ->  [LLVM.Named LLVM.Instruction]
     -> Statefully $ [MIRInstr () MWord]
isInstrs _ [] = return []
isInstrs env instrs = do
  instrs' <- mapM (isNInstruction env) instrs
  return $ concat instrs'
  where isNInstruction :: Env -> LLVM.Named LLVM.Instruction -> Statefully $ [MIRInstr () MWord]
        isNInstruction env (LLVM.Do instr) = isInstruction env Nothing instr
        isNInstruction env (name LLVM.:= instr) =
          let ret = name2nameM name in
            (\ret -> isInstruction env (Just ret) instr) =<< ret






-------------------------------------------------
-- * Terminators



-- ** Selection for Terminator

-- | Instruction Generation for terminators
-- We ignore the name of terminators
isTerminator :: Env
             -> LLVM.Named LLVM.Terminator
             -> Statefully $ [MIRInstr () MWord]
isTerminator env (name LLVM.:= term) = do
  ret <- return $ name2name name
  termInstr <- isTerminator' env (Just ret) term
  return $ termInstr
isTerminator env (LLVM.Do term) = do
  termInstr <- isTerminator' env Nothing term
  return $ termInstr
  
-- Branching

isTerminator' :: Env
              -> Maybe VReg
              -> LLVM.Terminator
              -> Statefully $ [MIRInstr () MWord]
isTerminator' env ret term =
  case term of 
    (LLVM.Br name _) -> lift $ isBr name
    (LLVM.CondBr cond name1 name2 _) -> lift $ isCondBr env cond name1 name2 
    (LLVM.Switch cond deflt dests _ ) -> isSwitch env cond deflt dests
    (LLVM.Ret ret _md) -> lift $ isRet env ret 
    (LLVM.Invoke _ _ f args _ retDest _exceptionDest _ ) -> -- treats this as a call + a jump 
      lift $ do call <- isCall env ret f args
                destJmp <- isBr retDest
                return $ call ++  destJmp
    -- `Resume` and `Unreachable` still need to terminate the block after
    -- flagging the error, so we add an `answer` instruction, which is defined
    -- to stall or halt execution.
    (LLVM.Resume _ _ ) -> return $ makeTraceInvalid ++ halt
    (LLVM.Unreachable _) -> return $ triggerBug ++ halt
    term ->  implError $ "Terminator not yet supported. \n \t" ++ (show term)
  where
    halt = [MirM (MRAM.Ianswer (LImm $ SConst 0)) ()]

makeTraceInvalid :: [MIRInstruction () regT MWord]
makeTraceInvalid = [MirI rtlCallValidIf  ()]
  where rtlCallValidIf = RCall TVoid Nothing (Label $ show $ Name "__cc_flag_invalid") [Tint] []
triggerBug :: [MIRInstruction () regT MWord]
triggerBug = [MirI rtlCallValidIf  ()]
  where rtlCallValidIf = RCall TVoid Nothing (Label $ show $ Name "__cc_flag_bug") [Tint] []

-- | Branch terminator
isBr :: Monad m => LLVM.Name -> m [MIRInstr () MWord]
isBr name =  do
  name' <- name2nameM name
  returnRTL $ [MRAM.Ijmp $ Label (show name')] -- FIXME: This works but it's a hack. Think about labels as strings.

isCondBr
  :: Env
     -> LLVM.Operand
     -> LLVM.Name
     -> LLVM.Name
     -> Hopefully  [MIRInstr () MWord]
isCondBr env cond name1 name2 = do
  cond' <- operand2operand env cond
  loc1 <- name2nameM name1
  loc2 <- name2nameM name2 
  returnRTL $ [MRAM.Icjmp cond' $ Label (show loc1), -- FIXME: This works but it's a hack. Think about labels.
                MRAM.Ijmp $ Label (show loc2)]

isSwitch
  :: Traversable t =>
     Env
     -> LLVM.Operand
     -> LLVM.Name
     -> t (LLVM.Constant.Constant, LLVM.Name)
     -> Statefully [MIRInstr () MWord]
isSwitch env cond deflt dests = do
  cond' <- lift $ operand2operand env cond
  deflt' <- lift $ name2nameM deflt
  switchInstrs <- mapM (isDest cond') dests
  returnRTL $ (concat switchInstrs) ++ [MRAM.Ijmp (Label $ show deflt')]
    where isDest cond' (switch,dest) = do
            switch' <- lift $ getConstant env switch
            isEq <- freshName
            dest' <- lift $ name2nameM dest
            return [MRAM.Icmpe isEq cond' switch', MRAM.Icjmp (AReg isEq) (Label $ show dest')]

-- Possible optimisation:
-- Add just one return block, and have all others jump there.    
isRet
  :: Env -> Maybe LLVM.Operand -> Hopefully [MIRInstruction () VReg MWord]
isRet env (Just ret) = do
  ret' <- operand2operand env ret
  return $ maybeTraceIR "return" [ret'] ++ [MirI (RRet $ Just ret') ()]
isRet _env Nothing =
  return $ maybeTraceIR "return" [] ++ [MirI (RRet Nothing) ()]

------------------------------------------------------
-- * Block calculation





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
isBlock:: Env -> LLVM.BasicBlock -> Statefully (BB Name $ MIRInstr () MWord)
isBlock  env (LLVM.BasicBlock name instrs term) = do
  body <- isInstrs env instrs
  let body' = maybeTraceIR ("enter " ++ show name) [] ++ body
  end <- isTerminator env term
  jumpsTo <- lift $ blockJumpsTo term
  name' <- lift $ name2nameM name
  return $ BB name' body' end jumpsTo

isBlocks :: Env ->  [LLVM.BasicBlock] -> Statefully [BB Name $ MIRInstr () MWord]
isBlocks env = mapM (isBlock env)

processParams :: ([LLVM.Parameter], Bool) -> [Ty]
processParams (params, _) = map (\_ -> Tint) params

-- | Instruction generation for Functions

isFunction :: Env -> LLVM.Definition -> Hopefully $ MIRFunction () MWord
isFunction env (LLVM.GlobalDefinition (LLVM.Function _ _ _ _ _ retT name params _ _ _ _ _ _ code _ _)) =
  do
    (body, nextReg) <- runStateT (isBlocks env code) initState
    params' <- return $ processParams params
    name' <- name2nameM name
    retT' <- type2type  (llvmtTypeEnv env) retT
    return $ Function name' retT' params' body nextReg
isFunction _tenv other = unreachableError $ show other -- Shoudl be filtered out 
  
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

-- Here is how we it works:
-- Create a set with a list of globals that are defined.
isGlobVars :: Env -> [LLVM.Definition] -> Hopefully $ GEnv MWord
isGlobVars env defs =
  mapMaybeM (isGlobVar' env) defs
  where isGlobVar' env (LLVM.GlobalDefinition g) = do
          flatGVar <- isGlobVar env g
          return $ Just flatGVar 
        isGlobVar' _ _ = return Nothing
nameOfGlobals :: [LLVM.Definition] -> Set.Set LLVM.Name
nameOfGlobals defs = Set.fromList $ concat $ map nameOfGlobal defs
  where nameOfGlobal (LLVM.GlobalDefinition (LLVM.GlobalVariable name _ _ _ _ _ _ _ _ _ _ _ _ _)) =
          [name]
        nameOfGlobal (LLVM.GlobalDefinition
                      (LLVM.Function _ _ _ _ _ _ name _ _ _ _ _ _ _ _ _ _)) =
          [name]
        nameOfGlobal _ = []

          
isGlobVar :: Env -> LLVM.Global -> Hopefully $ GlobalVariable MWord
isGlobVar env (LLVM.GlobalVariable name _ _ _ _ _ const typ _ init sectn _ align _) = do
  typ' <- type2type (llvmtTypeEnv env) typ
  byteSize <- return $ sizeOf (llvmtTypeEnv env) typ
  init' <- flatInit env init
  case init' of
    Just initWords ->
      when (byteSize > fromIntegral (length initWords * wordBytes)) $ assumptError $
        "impossible: global size is " ++ show byteSize ++ " but evaluation produced only " ++
          show (length initWords) ++ " words of initializer"
    Nothing -> return ()
  -- GlobalVariable size and align are given in words, not bytes.
  let size' = (byteSize + fromIntegral wordBytes - 1) `div` fromIntegral wordBytes
  -- Force alignment to be at least 1.  LLVM allows globals with no `align`
  -- attribute, which llvm-hs parses as an alignment of 0.  But this confuses
  -- later passes that try to align to a multiple of zero, so we adjust the
  -- alignment here to avoid the problem.
  let align' = max 1 $ (fromIntegral align + fromIntegral wordBytes - 1) `div` fromIntegral wordBytes
  return $ GlobalVariable (name2name name) const typ' init' size' align' (sectionIsSecret sectn)
  where flatInit :: Env ->
                    Maybe LLVM.Constant.Constant ->
                    Hopefully $ Maybe [LazyConst String MWord]
        flatInit _ Nothing = return Nothing
        flatInit env (Just const) = do
          const' <- flattenConstant env const
          return $ Just const'

        sectionIsSecret (Just "__DATA,__secret") = True
        sectionIsSecret (Just ".data.secret") = True
        sectionIsSecret _ = False
isGlobVar _ other = unreachableError $ show other

-- | Evaluate an LLVM constant and flatten its value into a list of (lazy)
-- machine words.
flattenConstant :: Env
                -> LLVM.Constant.Constant
                -> Hopefully [LazyConst String MWord]
flattenConstant env c = do
    chunks <- constant2typedLazyConst env c
    return $ go (SConst 0) 0 $ map unpack chunks
  where
    unpack (TypedLazyConst lc w) = (lc, widthInt w)

    go ::
      LazyConst String MWord -> Int -> [(LazyConst String MWord, Int)] ->
      [LazyConst String MWord]
    go _acc 0 [] = []
    go acc _pos [] = [acc]
    go acc pos ((lc, w) : cs)
      | w > wordBytes = error "flattenConstant: impossible: TLC had width > word size?"
      -- Special case for word-aligned chunks
      | pos == 0 && w == wordBytes = lc : go acc pos cs
      | pos + w < wordBytes = go (combine acc pos lc) (pos + w) cs
      | pos + w == wordBytes = combine acc pos lc : go (SConst 0) 0 cs
      | pos + w > wordBytes = combine acc pos lc :
          go (consume (wordBytes - pos) lc) (pos + w - wordBytes) cs
      | otherwise = error "flattenConstant: unreachable"

    combine acc pos lc = acc .|. (lc `shiftL` (pos * 8))

    consume amt lc = lc `shiftR` (amt * 8)


                        

constant2OnelazyConst ::
  Env
  -> LLVM.Constant.Constant
  -> Hopefully $ LazyConst String MWord
constant2OnelazyConst env c = do
  cs' <- constant2typedLazyConst env c
  case cs' of
    [TypedLazyConst lc _w] -> return lc
    _ -> error $ "expected a single lazy constant, but got " ++ show (length cs') ++
      " (on " ++ show c ++ ")"



-- | A `LazyConst` whose value is guaranteed to fit within `width` bytes.  Do
-- not construct directly; use `mkTypedLazyConst` instead (which enforces the
-- invariant).
data TypedLazyConst = TypedLazyConst (LazyConst String MWord) MemWidth

mkTypedLazyConst :: LazyConst String MWord -> MemWidth -> TypedLazyConst
mkTypedLazyConst lc w = TypedLazyConst (lc .&. SConst mask) w
  where mask = (1 `shiftL` (8 * MRAM.widthInt w)) - 1

typedLazyUop ::
  (LazyConst String MWord -> LazyConst String MWord) ->
  TypedLazyConst -> TypedLazyConst
typedLazyUop op (TypedLazyConst lc1 w1) =
  mkTypedLazyConst (op lc1) w1

typedLazyBop ::
  (LazyConst String MWord -> LazyConst String MWord -> LazyConst String MWord) ->
  TypedLazyConst -> TypedLazyConst -> TypedLazyConst
typedLazyBop op (TypedLazyConst lc1 w1) (TypedLazyConst lc2 w2) =
  mkTypedLazyConst (op lc1 lc2) (max w1 w2)

instance Num TypedLazyConst where
  (+) = typedLazyBop (+)
  (-) = typedLazyBop (-)
  (*) = typedLazyBop (*)
  negate = typedLazyUop negate
  abs = typedLazyUop abs
  signum = typedLazyUop signum
  fromInteger _n = error "fromInteger not supported for TypedLazyConst"

instance Eq TypedLazyConst where
  _ == _ = error "(==) not supported for TypedLazyConst"

instance Bits TypedLazyConst where
  (.&.) = typedLazyBop (.&.)
  (.|.) = typedLazyBop (.|.)
  xor = typedLazyBop xor
  complement = typedLazyUop complement
  shift x b = typedLazyUop (\x -> shift x b) x
  rotate x b = typedLazyUop (\x -> rotate x b) x
  bitSize _ = case bitSizeMaybe (zeroBits :: MWord) of
                Just x -> x
                Nothing -> 0
  bitSizeMaybe _ = bitSizeMaybe (zeroBits :: MWord)
  isSigned _ = isSigned (zeroBits :: MWord)
  testBit _ _ = error "testBit not supported for TypedLazyConst"
  bit _ = error "bit not supported for TypedLazyConst"
  popCount _ = error "popCount not supported for TypedLazyConst"


constant2typedLazyConst ::
  Env
  -> LLVM.Constant.Constant
  -> Hopefully $ [TypedLazyConst]
constant2typedLazyConst env c =
  case c of
    (LLVM.Constant.Int bits val                     ) -> case bits of
      -- Special case for `i1`/bool.  We represent it as 1 byte wide.  `i1`
      -- should never appear in memory accesses, so this shouldn't present any
      -- problem.
      1 -> return [mkTypedLazyConst (fromInteger val) W1]
      8 -> return [mkTypedLazyConst (fromInteger val) W1]
      16 -> return [mkTypedLazyConst (fromInteger val) W2]
      32 -> return [mkTypedLazyConst (fromInteger val) W4]
      64 -> return [mkTypedLazyConst (fromInteger val) W8]
      _ -> implError $ "Constant.Int with width " ++ show bits ++ " is not supported"
    (LLVM.Constant.Null _ty                         ) ->
      return [mkTypedLazyConst (fromInteger 0) WWord]
    (LLVM.Constant.AggregateZero ty                 ) ->
      -- Compute the width, then emit a list of that many bytes.
      return $ replicate (fromIntegral $ sizeOf (llvmtTypeEnv env) ty) zeroByte
    (LLVM.Constant.Struct _name True vals           ) ->
      concat <$> mapM (constant2typedLazyConst env) vals
    (LLVM.Constant.Struct _name False vals          ) -> do
      let pads = structPadding (llvmtTypeEnv env) $ map (typeOf (llvmtTypeEnv env)) vals
      let f :: LLVM.Constant.Constant -> MWord -> Hopefully [TypedLazyConst]
          f val pad = do
            val' <- constant2typedLazyConst env val
            return $ val' ++ replicate (fromIntegral pad) zeroByte
      concat <$> zipWithM f vals pads
    (LLVM.Constant.Array _ty vals                   ) ->
      concat <$> mapM (constant2typedLazyConst env) vals
    (LLVM.Constant.Undef ty                         ) ->
      return $ replicate (fromIntegral $ sizeOf (llvmtTypeEnv env) ty) zeroByte
    (LLVM.Constant.GlobalReference _ty name         ) -> do
      _ <- checkName (globs env) name
      name' <- return $ show $ name2name name
      return [mkTypedLazyConst (LConst $ \ge -> ge name') WWord]
    (LLVM.Constant.Add _ _ op1 op2                  ) -> bop2typedLazyConst env (+) op1 op2
    (LLVM.Constant.Sub  _ _ op1 op2                 ) -> bop2typedLazyConst env (-) op1 op2
    (LLVM.Constant.Mul  _ _ op1 op2                 ) -> bop2typedLazyConst env (*) op1 op2
    (LLVM.Constant.UDiv  _ op1 op2                  ) ->
      bop2typedLazyConst env (typedLazyBop lcQuot) op1 op2
    (LLVM.Constant.SDiv _ _op1 _op2                 ) ->
      implError $ "Signed division is not implemented."
    (LLVM.Constant.URem op1 op2                     ) ->
      bop2typedLazyConst env (typedLazyBop lcRem) op1 op2
    (LLVM.Constant.SRem _op1 _op2                   ) -> 
      implError $ "Signed reminder is not implemented."
    (LLVM.Constant.And op1 op2                      ) -> bop2typedLazyConst env (.&.) op1 op2
    (LLVM.Constant.Or op1 op2                       ) -> bop2typedLazyConst env (.|.) op1 op2
    (LLVM.Constant.Xor op1 op2                      ) -> bop2typedLazyConst env xor op1 op2
    (LLVM.Constant.GetElementPtr _bounds addr inxs  ) -> do
      addr' <- constant2OnelazyConst env addr
      ty' <- return $ typeOf (llvmtTypeEnv env) addr
      inxs' <- mapM (constant2OnelazyConst env) inxs
      gepResult <- constGEP (llvmtTypeEnv env) ty' addr' inxs'
      -- GEP returns a pointer, which is always one word in size
      return [mkTypedLazyConst gepResult WWord]
    (LLVM.Constant.PtrToInt op1 _typ                ) -> constant2typedLazyConst env op1
    (LLVM.Constant.IntToPtr op1 _typ                ) -> constant2typedLazyConst env op1
    -- TODO: special case for bitcasting to ptr or int type: repack list of
    -- TypedLazyConsts into a single value
    (LLVM.Constant.BitCast  op1 _typ                ) -> constant2typedLazyConst env op1
    (LLVM.Constant.Vector _mems                     ) ->  
      implError $ "Vectors not yet supported."
    (LLVM.Constant.ExtractElement _vect _indx       ) -> 
      implError $ "Vectors not yet supported."
    (LLVM.Constant.InsertElement _vect _elem _indx  ) -> 
      implError $ "Vectors not yet supported."
    (LLVM.Constant.ShuffleVector _op1 _op2 _mask    ) -> 
      implError $ "Vectors not yet supported."
    c -> implError $ "Constant not supported yet for global initializers: " ++ show c
  where
    zeroByte = mkTypedLazyConst 0 W1

constGEP :: LLVMTypeEnv
         -> LLVM.Type
         -> LazyConst String MWord
         -> [LazyConst String MWord]
         -> Hopefully $ LazyConst String MWord
constGEP _ (LLVM.PointerType _refT _) _ [] = assumptError "GetElementPtr should have at least one index. "
constGEP tenv (LLVM.PointerType refT _) ptr (inx:inxs) = do
  _typ' <- type2type tenv refT
  ofs <- return $ inx * (SConst $ sizeOf tenv refT)
  final <- constGEP' tenv refT(ptr + ofs) inxs
  return $ final
  where constGEP' :: LLVMTypeEnv
                  -> LLVM.Type
                  -> LazyConst String MWord
                  -> [LazyConst String MWord]
                  -> Hopefully $ LazyConst String MWord 
        constGEP' _ _ ptr [] = return ptr
        constGEP' env (LLVM.ArrayType _ elemsT) ptr (inx:inxs) = 
           flip (constGEP' env elemsT) inxs (ptr + inx * (SConst $ sizeOf env elemsT))
        constGEP' env (LLVM.StructureType packed tys) ptr (inx:inxs) =
          case inx of
            SConst inx' ->
              let new_type = (tys !! (fromEnum inx')) in
                let ofs' = if packed then
                             sum $ map (sizeOf env) $ takeEnum inx' $ tys
                           else
                             offsetOfStructElement tenv new_type $ (takeEnum $ inx') $ tys
              in flip (constGEP' env (tys !! (fromEnum inx'))) inxs (ptr + (SConst $ ofs'))
            _ -> implError $ "GetElementPtr called with a lazy constant. That means that a global reference (or funciton pointer) was used to compute those indices. That is invalid, indices should be constant."
        constGEP' env (LLVM.NamedTypeReference name) ptr inxs = do
          ty' <- typeDef env name
          constGEP' env ty' ptr inxs
        constGEP' _ ty _ _ = assumptError $ "GetElementPtr must be called on an agregate type (the first type must be a pointer) but found a non aggregate one: \n \t " ++ show ty
        
constGEP _ ty _ _ = assumptError $ "GetElementPtr expects a pointer type, but found: \n \t" ++ show ty





bop2typedLazyConst :: Env
              -> (TypedLazyConst -> TypedLazyConst -> TypedLazyConst)
              -> LLVM.Constant.Constant
              -> LLVM.Constant.Constant
              -> Hopefully [TypedLazyConst]
bop2typedLazyConst env bop op1 op2 = do
  op1s <- constant2typedLazyConst env op1
  op1' <- getUniqueWord op1s
  op2s <- constant2typedLazyConst env op2
  op2' <- getUniqueWord op2s
  return [bop op1' op2']
  where getUniqueWord :: [TypedLazyConst] -> Hopefully TypedLazyConst
        getUniqueWord [op1'] = return op1' 
        getUniqueWord _ = assumptError "Tryed to compute a binary operation with an aggregate value." 









isFuncAttributes :: [LLVM.Definition] -> Hopefully $ () -- TODO can we use this attributes?
isFuncAttributes _ = return () 

isDefs :: [LLVM.Definition] -> Hopefully $ MIRprog () MWord
isDefs defs = do
  typeDefs <- isTypeDefs $ filter itIsTypeDef defs
  setGlobNames <- return $ nameOfGlobals defs
  env <- return $ Env typeDefs setGlobNames
  globVars <- (isGlobVars env) $ filter itIsGlobVar defs -- filtered inside the def 
  --otherError $ "DEBUG HERE: \n" ++ show typeDefs ++ "\n" ++ show globVars ++ "\n"  
  _funcAttr <- isFuncAttributes $ filter itIsFuncAttr defs
  funcs <- mapM (isFunction env) $ filter itIsFunc defs
  checkDiscardedDefs defs -- Make sure we dont drop something important
  return $ IRprog Map.empty globVars funcs
  
-- | Instruction selection generates an RTL Program
instrSelect :: LLVM.Module -> Hopefully $ MIRprog () MWord
instrSelect (LLVM.Module _ _ _ _ defs) = isDefs defs
