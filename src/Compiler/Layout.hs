{-# LANGUAGE RecordWildCards #-}
module Compiler.Layout
(
  LLVMTypeEnv,
  sizeOf,
  alignOf,
  structPadding,
  offsetOfStructElement,
  typeOfConstant,
) where

import Data.Bits
import qualified Data.Map as Map 
import Data.Map (Map)

import MicroRAM

-- Avoid importing `Name` unqualified, to minimize confusion.
import LLVM.AST (Type(..))
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Constant as LLVM.Constant
import LLVM.AST.Typed (typeOf)
import LLVM.AST.AddrSpace

type LLVMTypeEnv = Map LLVM.Name Type

-- | Compute the size of an LLVM type in bytes.
sizeOf :: LLVMTypeEnv -> Type -> MWord
sizeOf tenv ty = case ty of
  VoidType -> 0
  IntegerType 1 -> 1
  IntegerType bits | bits `mod` 8 == 0 -> fromIntegral bits `div` 8
  PointerType _ _ -> 8
  VectorType len ty -> fromIntegral len * sizeOf tenv ty
  StructureType True tys -> sum (map (sizeOf tenv) tys)
  StructureType False tys -> sizeOfStruct tenv tys
  ArrayType len ty -> fromIntegral len * sizeOf tenv ty
  NamedTypeReference name -> sizeOf tenv $ typeDef tenv name
  _ -> error $ "unsupported type " ++ show ty ++ " in sizeOf"

alignOf :: LLVMTypeEnv -> Type -> MWord
alignOf tenv ty = case ty of
  VoidType -> 1
  IntegerType 1 -> 1
  IntegerType bits | bits `elem` [8, 16, 32, 64] -> fromIntegral bits `div` 8
  PointerType _ _ -> 8
  VectorType _ ty -> alignOf tenv ty
  StructureType True _ -> 1
  StructureType False tys -> alignOfStruct tenv tys
  ArrayType _ ty -> alignOf tenv ty
  NamedTypeReference name -> alignOf tenv $ typeDef tenv name
  _ -> error $ "unsupported type " ++ show ty ++ " in alignOf"


alignTo :: (Bits a, Num a) => a -> a -> a
alignTo a x = (x + a - 1) .&. complement (a - 1)

sizeOfStruct :: LLVMTypeEnv -> [Type] -> MWord
sizeOfStruct tenv tys =
  alignTo (alignOfStruct tenv tys) $
  foldl (\pos ty -> alignTo (alignOf tenv ty) pos + sizeOf tenv ty) 0 tys

-- | For each field of a struct, give the amount of padding required after the
-- field.
structPadding :: LLVMTypeEnv -> [Type] -> [MWord]
structPadding env tys = tail $ go 0 1 tys
  where
    go :: MWord -> MWord -> [Type] -> [MWord]
    -- Before each `ty`, pad the `pos` to `alignOf ty`.
    go pos maxAlign (ty : tys) =
      let pad = pos `mod` alignOf env ty
      in pad : go (pos + pad + sizeOf env ty) (max maxAlign (alignOf env ty)) tys
    -- After the last `ty`, pad the `pos` to the overall alignment of the
    -- struct, which is `maxAlign`.
    go pos maxAlign [] = [pos `mod` maxAlign]
    -- `go` returns a list with an extra 0 at the start, indicating that no
    -- padding is required before the first field.

offsetOfStructElement :: LLVMTypeEnv -> Type -> [Type] -> MWord
offsetOfStructElement tenv ty_head tys =
  alignTo (alignOf tenv ty_head) $
  foldl (\pos ty -> alignTo (alignOf tenv ty) pos + sizeOf tenv ty) 0 tys
  

alignOfStruct :: LLVMTypeEnv -> [Type] -> MWord
alignOfStruct tenv tys = maximum (map (alignOf tenv) tys)


typeDef :: LLVMTypeEnv -> LLVM.Name -> Type
typeDef tenv name = case Map.lookup name tenv of
  Just x -> x
  Nothing -> error $ "unknown type " ++ show name



-- ** All the same, but for constants.
-- llvm-hs-pure duplicates instructions in Constants so
-- We need to duplicate all functions over constants.
-- (Note we can't call LLVM.typeOf on the constant to reuse the code above
--  because it fails on references, global or local)




-- | Compute the type of a constant __UP TO pointers__
-- We can't use the llvm-hs-pur `typeOf` because it fails on references.
-- ONLYT TO BE USED FOR SIZE CALCULATIONS!
typeOfConstant :: LLVM.Constant.Constant -> Type
typeOfConstant (LLVM.Constant.Int bits _)  = IntegerType bits
typeOfConstant (LLVM.Constant.Float t) = typeOf t
typeOfConstant (LLVM.Constant.Null t)      = t
typeOfConstant (LLVM.Constant.AggregateZero t) = t
typeOfConstant (LLVM.Constant.Struct {..}) =
  StructureType isPacked (map typeOfConstant memberValues)
typeOfConstant (LLVM.Constant.Array {..})  = ArrayType (fromIntegral $ length memberValues) memberType
typeOfConstant (LLVM.Constant.Vector {..}) = VectorType (fromIntegral $ length memberValues) $
                                 case memberValues of
                                []    -> error "Vectors of size zero are not allowed. (Malformed AST)"
                                (x:_) -> typeOfConstant x
typeOfConstant (LLVM.Constant.Undef t)     = t
typeOfConstant (LLVM.Constant.BlockAddress {..})   = PointerType (IntegerType 8) (AddrSpace 0)
typeOfConstant (LLVM.Constant.GlobalReference t _) = t
typeOfConstant (LLVM.Constant.Add {..})     = typeOfConstant operand0
typeOfConstant (LLVM.Constant.FAdd {..})    = typeOfConstant operand0
typeOfConstant (LLVM.Constant.FDiv {..})    = typeOfConstant operand0
typeOfConstant (LLVM.Constant.FRem {..})    = typeOfConstant operand0
typeOfConstant (LLVM.Constant.Sub {..})     = typeOfConstant operand0
typeOfConstant (LLVM.Constant.FSub {..})    = typeOfConstant operand0
typeOfConstant (LLVM.Constant.Mul {..})     = typeOfConstant operand0
typeOfConstant (LLVM.Constant.FMul {..})    = typeOfConstant operand0
typeOfConstant (LLVM.Constant.UDiv {..})    = typeOfConstant operand0
typeOfConstant (LLVM.Constant.SDiv {..})    = typeOfConstant operand0
typeOfConstant (LLVM.Constant.URem {..})    = typeOfConstant operand0
typeOfConstant (LLVM.Constant.SRem {..})    = typeOfConstant operand0
typeOfConstant (LLVM.Constant.Shl {..})     = typeOfConstant operand0
typeOfConstant (LLVM.Constant.LShr {..})    = typeOfConstant operand0
typeOfConstant (LLVM.Constant.AShr {..})    = typeOfConstant operand0
typeOfConstant (LLVM.Constant.And {..})     = typeOfConstant operand0
typeOfConstant (LLVM.Constant.Or  {..})     = typeOfConstant operand0
typeOfConstant (LLVM.Constant.Xor {..})     = typeOfConstant operand0
typeOfConstant (LLVM.Constant.GetElementPtr {..}) = -- Here we truncate types because all pointers are size.
  PointerType (IntegerType 64) (AddrSpace 0)
typeOfConstant (LLVM.Constant.Trunc {..})   = type'
typeOfConstant (LLVM.Constant.ZExt {..})    = type'
typeOfConstant (LLVM.Constant.SExt {..})    = type'
typeOfConstant (LLVM.Constant.FPToUI {..})  = type'
typeOfConstant (LLVM.Constant.FPToSI {..})  = type'
typeOfConstant (LLVM.Constant.UIToFP {..})  = type'
typeOfConstant (LLVM.Constant.SIToFP {..})  = type'
typeOfConstant (LLVM.Constant.FPTrunc {..}) = type'
typeOfConstant (LLVM.Constant.FPExt {..})   = type'
typeOfConstant (LLVM.Constant.PtrToInt {..}) = type'
typeOfConstant (LLVM.Constant.IntToPtr {..}) = type'
typeOfConstant (LLVM.Constant.BitCast {..})  = type'
typeOfConstant (LLVM.Constant.ICmp {..})    = case (typeOfConstant operand0) of
                            (VectorType n _) -> VectorType n $ IntegerType 1
                            _ -> IntegerType 1
typeOfConstant (LLVM.Constant.FCmp {..})    = case (typeOfConstant operand0) of
                            (VectorType n _) -> VectorType n $ IntegerType 1
                            _ -> IntegerType 1
typeOfConstant (LLVM.Constant.Select {..})  = typeOfConstant trueValue
typeOfConstant (LLVM.Constant.ExtractElement {..})  = case typeOfConstant vector of
                                    (VectorType _ t) -> t
                                    _ -> error "The first operand of an extractelement instruction is a value of vector type. (Malformed AST)"
typeOfConstant (LLVM.Constant.InsertElement {..})   = typeOfConstant vector
typeOfConstant (LLVM.Constant.ShuffleVector {..})   = case (typeOfConstant operand0, typeOfConstant mask) of
                                    (VectorType _ t, VectorType m _) -> VectorType m t
                                    _ -> error "The first operand of an shufflevector instruction is a value of vector type. (Malformed AST)"
typeOfConstant (LLVM.Constant.ExtractValue {..})    =
  error "Constant ExtractValue not supported" -- How do you load from memory in a constant?
typeOfConstant (LLVM.Constant.InsertValue {..})     = typeOfConstant aggregate
typeOfConstant (LLVM.Constant.TokenNone)          = TokenType
typeOfConstant (LLVM.Constant.AddrSpaceCast {..}) = type'
