{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Compiler.TypeOf
Description : llvm-hs-pure typeOf reimplementation
Maintainer  : santiago@galois.com
Stability   : prototype

This module is a copy-paste of LLVM.AST.Typed, patched to add an `LLVMTypeEnv`
argument to the `typeOf` function and to call `resolve` on types before pattern
matching.  This prevents a variety of errors involving `NamedTypeReference`s,
which the original module can't handle.

Specific changes:
- `LLVMTypeEnv` and `resolve` are added, and the signature of `typeOf` is
  changed slightly.
- `typeOf x` is replaced with `typeOf env x` everywhere.
- Calls to helper functions such as `getElementPtrType` have a new `env`
  argument as well.
- All pattern matching on types either calls `resolve` first, or has a
  `NamedTypeReference` case that calls `resolve` and re-applies the function.

-}
module Compiler.TypeOf (
  LLVMTypeEnv,
  resolve,
  Typed(..),
  getElementType,
  getElementPtrType,
  extractValueType,
) where

import qualified Data.Map as Map
import Data.Map (Map)

import LLVM.Prelude

import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Type

import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F

type LLVMTypeEnv = Map Name Type

resolve :: LLVMTypeEnv -> Type -> Type
resolve env (NamedTypeReference name) = case Map.lookup name env of
  Just x -> resolve env x
  Nothing -> error $ "failed to resolve named type " ++ show name
resolve _ ty = ty

class Typed a where
  typeOf :: LLVMTypeEnv -> a -> Type

instance Typed Operand where
  typeOf env (LocalReference t _) = t
  typeOf env (ConstantOperand c)  = typeOf env c
  typeOf env _                    = MetadataType

instance Typed CallableOperand where
  typeOf env (Right op) = typeOf env op
  typeOf env (Left _) = error "typeOf env inline assembler is not defined. (Malformed AST)"

instance Typed C.Constant where
  typeOf env (C.Int bits _)  = IntegerType bits
  typeOf env (C.Float t) = typeOf env t
  typeOf env (C.Null t)      = t
  typeOf env (C.AggregateZero t) = t
  typeOf env (C.Struct {..}) = StructureType isPacked (map (typeOf env) memberValues)
  typeOf env (C.Array {..})  = ArrayType (fromIntegral $ length memberValues) memberType
  typeOf env (C.Vector {..}) = VectorType (fromIntegral $ length memberValues) $
                              case memberValues of
                                  []    -> error "Vectors of size zero are not allowed. (Malformed AST)"
                                  (x:_) -> typeOf env x
  typeOf env (C.Undef t)     = t
  typeOf env (C.BlockAddress {..})   = ptr i8
  typeOf env (C.GlobalReference t _) = t
  typeOf env (C.Add {..})     = typeOf env operand0
  typeOf env (C.FAdd {..})    = typeOf env operand0
  typeOf env (C.FDiv {..})    = typeOf env operand0
  typeOf env (C.FRem {..})    = typeOf env operand0
  typeOf env (C.Sub {..})     = typeOf env operand0
  typeOf env (C.FSub {..})    = typeOf env operand0
  typeOf env (C.Mul {..})     = typeOf env operand0
  typeOf env (C.FMul {..})    = typeOf env operand0
  typeOf env (C.UDiv {..})    = typeOf env operand0
  typeOf env (C.SDiv {..})    = typeOf env operand0
  typeOf env (C.URem {..})    = typeOf env operand0
  typeOf env (C.SRem {..})    = typeOf env operand0
  typeOf env (C.Shl {..})     = typeOf env operand0
  typeOf env (C.LShr {..})    = typeOf env operand0
  typeOf env (C.AShr {..})    = typeOf env operand0
  typeOf env (C.And {..})     = typeOf env operand0
  typeOf env (C.Or  {..})     = typeOf env operand0
  typeOf env (C.Xor {..})     = typeOf env operand0
  typeOf env (C.GetElementPtr {..}) = getElementPtrType env (typeOf env address) indices
  typeOf env (C.Trunc {..})   = type'
  typeOf env (C.ZExt {..})    = type'
  typeOf env (C.SExt {..})    = type'
  typeOf env (C.FPToUI {..})  = type'
  typeOf env (C.FPToSI {..})  = type'
  typeOf env (C.UIToFP {..})  = type'
  typeOf env (C.SIToFP {..})  = type'
  typeOf env (C.FPTrunc {..}) = type'
  typeOf env (C.FPExt {..})   = type'
  typeOf env (C.PtrToInt {..}) = type'
  typeOf env (C.IntToPtr {..}) = type'
  typeOf env (C.BitCast {..})  = type'
  typeOf env (C.ICmp {..})    = case resolve env (typeOf env operand0) of
                              (VectorType n _) -> VectorType n i1
                              _ -> i1
  typeOf env (C.FCmp {..})    = case resolve env (typeOf env operand0) of
                              (VectorType n _) -> VectorType n i1
                              _ -> i1
  typeOf env (C.Select {..})  = typeOf env trueValue
  typeOf env (C.ExtractElement {..})  = case resolve env (typeOf env vector) of
                                      (VectorType _ t) -> t
                                      _ -> error "The first operand of an extractelement instruction is a value of vector type. (Malformed AST)"
  typeOf env (C.InsertElement {..})   = typeOf env vector
  typeOf env (C.ShuffleVector {..})   = case (resolve env (typeOf env operand0), resolve env (typeOf env mask)) of
                                      (VectorType _ t, VectorType m _) -> VectorType m t
                                      _ -> error "The first operand of an shufflevector instruction is a value of vector type. (Malformed AST)"
  typeOf env (C.ExtractValue {..})    = extractValueType env indices' (typeOf env aggregate)
  typeOf env (C.InsertValue {..})     = typeOf env aggregate
  typeOf env (C.TokenNone)          = TokenType
  typeOf env (C.AddrSpaceCast {..}) = type'

getElementPtrType :: LLVMTypeEnv -> Type -> [C.Constant] -> Type
getElementPtrType _ ty [] = ptr ty
getElementPtrType env ty@(NamedTypeReference _) is = getElementPtrType env (resolve env ty) is
getElementPtrType env (PointerType ty _) (_:is) = getElementPtrType env ty is
getElementPtrType env (StructureType _ elTys) (C.Int 32 val:is) =
  getElementPtrType env (elTys !! fromIntegral val) is
getElementPtrType env (VectorType _ elTy) (_:is) = getElementPtrType env elTy is
getElementPtrType env (ArrayType _ elTy) (_:is) = getElementPtrType env elTy is
getElementPtrType _ _ _ = error "Expecting aggregate type. (Malformed AST)"

getElementType :: LLVMTypeEnv -> Type -> Type
getElementType env ty@(NamedTypeReference _) = getElementType env (resolve env ty)
getElementType env (PointerType t _) = t
getElementType _ _ = error $ "Expecting pointer type. (Malformed AST)"

extractValueType :: LLVMTypeEnv -> [Word32] -> Type -> Type
extractValueType _ [] ty = ty
extractValueType env is ty@(NamedTypeReference _) = extractValueType env is (resolve env ty)
extractValueType env (i : is) (ArrayType numEls elTy)
  | fromIntegral i < numEls = extractValueType env is elTy
  | fromIntegral i >= numEls = error "Expecting valid index into array type. (Malformed AST)"
extractValueType env (i : is) (StructureType _ elTys)
  | fromIntegral i < length elTys = extractValueType env is (elTys !! fromIntegral i)
  | otherwise = error "Expecting valid index into structure type. (Malformed AST)"
extractValueType _ _ _ = error "Expecting vector type. (Malformed AST)"

instance Typed F.SomeFloat where
  typeOf env (F.Half _)          = FloatingPointType HalfFP
  typeOf env (F.Single _)        = FloatingPointType FloatFP
  typeOf env (F.Double _)        = FloatingPointType DoubleFP
  typeOf env (F.Quadruple _ _)   = FloatingPointType FP128FP
  typeOf env (F.X86_FP80 _ _)    = FloatingPointType X86_FP80FP
  typeOf env (F.PPC_FP128 _ _)   = FloatingPointType PPC_FP128FP

instance Typed Global where
  typeOf env (GlobalVariable {..}) = type'
  typeOf env (GlobalAlias {..})    = type'
  typeOf env (Function {..})       = let (params, isVarArg) = parameters
                                   in FunctionType returnType (map (typeOf env) params) isVarArg
instance Typed Parameter where
  typeOf env (Parameter t _ _) = t
