module Compiler.Layout
(
  LLVMTypeEnv,
  sizeOf,
  alignOf,
  fitHybridMemory,
  fromHybridMemory,
) where

import Data.Bits
import qualified Data.Map as Map 
import Data.Map (Map)

import Compiler.LazyConstants
import Compiler.Errors
import MicroRAM

-- Avoid importing `Name` unqualified, to minimize confusion.
import LLVM.AST (Type(..))
import qualified LLVM.AST as LLVM

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

sizeOfStruct :: LLVMTypeEnv -> [Type] -> MWord
sizeOfStruct tenv tys =
  alignTo (alignOfStruct tenv tys) $
  foldl (\pos ty -> alignTo (alignOf tenv ty) pos + sizeOf tenv ty) 0 tys
  where
    alignTo a x = (x + a - 1) .&. complement a

alignOfStruct :: LLVMTypeEnv -> [Type] -> MWord
alignOfStruct tenv tys = maximum (map (alignOf tenv) tys)


typeDef :: LLVMTypeEnv -> LLVM.Name -> Type
typeDef tenv name = case Map.lookup name tenv of
  Just x -> x
  Nothing -> error $ "unknown type " ++ show name



-- In the hybrid memory, a 8-byte-long word is represented by 8 words,
-- but stored just in the first word.  
fitHybridMemory :: LLVMTypeEnv -> Type -> LazyConst String MWord -> [LazyConst String MWord]
fitHybridMemory env ty lazy =
  let size = sizeOf env ty in
    lazy : emptyPadding size
  where emptyPadding sz = replicate (fromEnum (sz-1)) $ SConst $ fromInteger 0

-- Cant differentiate if 
fromHybridMemory
  :: LLVMTypeEnv
  -> Type
  -> [LazyConst String MWord]
  -> Hopefully (LazyConst String MWord)
fromHybridMemory env ty ls =
  let size = sizeOf env ty in
    if size == toEnum (length ls) && size > 0 then
      return (ls!!0)
    else
      assumptError
      $ "While pulling a constant from a memory representation found a mismatch in sizes. Expected size "
      ++ (show size)
      ++ " but found size "
      ++ (show $ length ls)
      ++ ". Probably an aggregate value was used where a non-aggregate one was expected. \n For type \t"
      ++ show ty
      ++ " \n found \t "
      ++ show ls
      
