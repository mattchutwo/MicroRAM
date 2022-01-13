{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Lazy Constants
Description : 
Maintainer  : santiago@galois.com
Stability   : Prototype

This are constants, up to global constant pointers that have not yet been computed.

-}
module Compiler.LazyConstants where



import Data.Bits
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import qualified LLVM.AST as LLVM(Name(..))


import Util.Util
import Compiler.Errors
import Compiler.Name

-- | Lazy constants are constants that can't be evaluated until a global environment is provided.
-- They should behave like constants for all purposes of the compiler, then a genv can be provided
-- after globals are set in memory, and lazy constants shall be replaced with real constatns

-- | Maps global names to their constant address.
-- gives a default of 0 for undefined globals, those should be checked before. 
type GlobMap wrdT = Name -> wrdT

data LazyConst wrdT =
    LConst
      (GlobMap wrdT -> wrdT)    -- ^ Function for computing the constant value
      (Set Name)                -- ^ Names referenced by this constant
  | SConst wrdT   -- ^ Static constant, allows optimizations/folding upstream and debugging

makeConcreteConst :: GlobMap wrdT -> LazyConst wrdT -> wrdT
makeConcreteConst gmap (LConst lw _) = lw gmap
makeConcreteConst _    (SConst  w) = w

instance (Show wrdT) => Show (LazyConst wrdT) where
  show (LConst _ ns) = "LazyConstant " ++ show (Set.toList ns)
  show (SConst w) = show w 

lcGlobal :: Name -> LazyConst wrdT
lcGlobal name = LConst (\ge -> ge name) (Set.singleton name)

lazyBop :: (wrdT -> wrdT -> wrdT)
     -> LazyConst wrdT -> LazyConst wrdT -> LazyConst wrdT
lazyBop bop (LConst l1 ns1) (LConst l2 ns2) = LConst (\ge -> bop (l1 ge) (l2 ge)) (ns1 <> ns2)
lazyBop bop (LConst l1 ns) (SConst c2) = LConst (\ge -> bop (l1 ge) c2) ns
lazyBop bop (SConst c1) (LConst l2 ns) = LConst (\ge -> bop c1 (l2 ge)) ns
lazyBop bop (SConst c1) (SConst c2) = SConst $ bop c1 c2

lazyUop :: (wrdT -> wrdT) -> LazyConst wrdT -> LazyConst wrdT
lazyUop uop (LConst l1 ns) = LConst (\ge -> uop (l1 ge)) ns
lazyUop uop (SConst c1) = SConst $ uop c1

lazyTop :: (wrdT -> wrdT -> wrdT -> wrdT)
        -> LazyConst wrdT
        -> LazyConst wrdT
        -> LazyConst wrdT
        -> LazyConst wrdT
lazyTop top (SConst c1) (SConst c2) (SConst c3) = SConst $ top c1 c2 c3
lazyTop top a1 a2 a3 = case forceLazy <$> [a1,a2,a3] of
                         [LConst a1',LConst a2',LConst a3'] ->
                           LConst $ \env -> top (a1' env) (a2' env) (a3' env)
                         _ -> undefined -- Impossible case
  where forceLazy (SConst a) = LConst (\_ -> a) -- Allways returns lazy.
        forceLazy lc@(LConst _) = lc


instance Num wrdT => Num (LazyConst wrdT) where
  (+) = lazyBop (+)
  (*) = lazyBop (*)
  (-) = lazyBop (-)
  negate = lazyUop negate 
  abs    = lazyUop abs   
  signum = lazyUop signum
  fromInteger n = SConst $ fromInteger n  

instance Eq (LazyConst wrdT) where
  _ == _ = error "(==) not supported for LazyConst"

instance Bits wrdT => Bits (LazyConst wrdT) where
  (.&.) = lazyBop (.&.)
  (.|.) = lazyBop (.|.)
  xor = lazyBop xor
  complement = lazyUop complement
  shift x b = lazyUop (`shift` b) x
  rotate x b = lazyUop (`rotate` b) x
  bitSize _ = case bitSizeMaybe (zeroBits :: wrdT) of
                Just x -> x
                _ -> 0 
  bitSizeMaybe _ = bitSizeMaybe (zeroBits :: wrdT)
  isSigned _ = isSigned (zeroBits :: wrdT)
  testBit _ _ = error "testBit not supported for LazyConst"
  bit i = SConst (bit i)
  popCount _ = error "popCount not supported for LazyConst"

lcQuot :: Integral wrdT => LazyConst wrdT -> LazyConst wrdT -> LazyConst wrdT
lcQuot = lazyBop quot

lcRem :: Integral wrdT => LazyConst wrdT -> LazyConst wrdT -> LazyConst wrdT
lcRem = lazyBop rem

lcCompare ::
  Num wrdT =>
  (Integer -> Integer -> Bool) ->
  (wrdT -> Integer) ->
  LazyConst wrdT ->
  LazyConst wrdT ->
  LazyConst wrdT
lcCompare cmp convert lc1 lc2 = lazyBop go lc1 lc2
  where go x y = if cmp (convert x) (convert y) then 1 else 0

lcCompareUnsigned ::
  Integral wrdT =>
  (Integer -> Integer -> Bool) ->
  LazyConst wrdT ->
  LazyConst wrdT ->
  LazyConst wrdT
lcCompareUnsigned cmp lc1 lc2 = lcCompare cmp toInteger lc1 lc2

lcCompareSigned ::
  (Integral wrdT, Bits wrdT) =>
  (Integer -> Integer -> Bool) ->
  Int ->
  LazyConst wrdT ->
  LazyConst wrdT ->
  LazyConst wrdT
lcCompareSigned cmp bits lc1 lc2 = lcCompare cmp convert lc1 lc2
  where convert x = toInteger x - 2 * toInteger (x .&. (1 `shiftL` bits - 1))

lazySignedBop ::
  (Integral wrdT, Bits wrdT) =>
  (Integer -> Integer -> Integer) ->
  Int ->
  LazyConst wrdT ->
  LazyConst wrdT ->
  LazyConst wrdT
lazySignedBop bop bits lc1 lc2 =
    lazyBop (\x y -> unconvert $ bop (convert x) (convert y)) lc1 lc2
  where
    convert x = toInteger x - 2 * toInteger (x .&. (1 `shiftL` bits - 1))
    unconvert x = fromInteger x

lcSDiv ::
  (Integral wrdT, Bits wrdT) =>
  Int -> LazyConst wrdT -> LazyConst wrdT -> LazyConst wrdT
lcSDiv = lazySignedBop quot

lcSRem ::
  (Integral wrdT, Bits wrdT) =>
  Int -> LazyConst wrdT -> LazyConst wrdT -> LazyConst wrdT
lcSRem = lazySignedBop rem


-- | Quick shorthand to return lists of one element
returnL :: Monad m => a -> m [a]
returnL x = return $ return $ x

-- | Check the name is a gobal variable.
checkName :: Set.Set LLVM.Name -> LLVM.Name -> Hopefully $ ()
checkName globs name =
  if Set.member name globs
  then return ()
  else assumptError $ "Global variable not defined \n \t" ++ show name ++ "\n"

-- | Duplicated from InstructionSelection
-- Can we unify


applyPartialMap :: Map.Map Name b -> LazyConst b -> LazyConst b
applyPartialMap _  (SConst w) = SConst w
applyPartialMap m1 (LConst lw ns) = LConst (\ge -> lw $ partiallyAppliedMap m1 ge) ns
  where partiallyAppliedMap :: Map.Map Name b -> (Name -> b) -> Name -> b
        partiallyAppliedMap map1 f a = case Map.lookup a map1 of
                                         Just w -> w
                                         Nothing -> f a
