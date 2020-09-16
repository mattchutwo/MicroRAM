module Compiler.Layout
(
  LLVMTypeEnv,
  sizeOf,
  alignOf,
) where

import Data.Bits
import qualified Data.Map as Map 
import Data.Map (Map) 

-- Avoid importing `Name` unqualified, to minimize confusion.
import LLVM.AST (Type(..))
import qualified LLVM.AST as LLVM

type LLVMTypeEnv = Map LLVM.Name Type

-- | Compute the size of an LLVM type in bytes.
sizeOf :: LLVMTypeEnv -> Type -> Word
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

alignOf :: LLVMTypeEnv -> Type -> Word
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

sizeOfStruct :: LLVMTypeEnv -> [Type] -> Word
sizeOfStruct tenv tys =
  alignTo (alignOfStruct tenv tys) $
  foldl (\pos ty -> alignTo (alignOf tenv ty) pos + sizeOf tenv ty) 0 tys
  where
    alignTo a x = (x + a - 1) .&. complement a

alignOfStruct :: LLVMTypeEnv -> [Type] -> Word
alignOfStruct tenv tys = maximum (map (alignOf tenv) tys)


typeDef :: LLVMTypeEnv -> LLVM.Name -> Type
typeDef tenv name = case Map.lookup name tenv of
  Just x -> x
  Nothing -> error $ "unknown type " ++ show name
