{-# LANGUAGE RecordWildCards #-}
module Compiler.Layout
(
  LLVMTypeEnv,
  sizeOf,
  alignOf,
  alignTo,
  structPadding,
  offsetOfStructElement,
) where

import Data.Bits
import qualified Data.Map as Map 

import MicroRAM
import Compiler.TypeOf (LLVMTypeEnv)

-- Avoid importing `Name` unqualified, to minimize confusion.
import LLVM.AST (Type(..))
import qualified LLVM.AST as LLVM

-- | Compute the size of an LLVM type in bytes.
sizeOf :: LLVMTypeEnv -> Type -> MWord
sizeOf tenv ty = case ty of
  VoidType -> 0
  IntegerType 1 -> 1
  IntegerType bits | bits `mod` 8 == 0 -> fromIntegral bits `div` 8
  FloatingPointType LLVM.FloatFP -> 4
  FloatingPointType LLVM.DoubleFP -> 8
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
  FloatingPointType LLVM.FloatFP -> 4
  FloatingPointType LLVM.DoubleFP -> 8
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
