{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Lazy Constants
Description : 
Maintainer  : santiago@galois.com
Stability   : Prototype

This are constants, up to global constant pointers that have not yet been computed.

-}
module Compiler.LazyConstants where


import qualified Data.Set as Set




import qualified LLVM.AST as LLVM(Name(..))


import Util.Util
import Compiler.Errors
--import Compiler.Common

-- | Lazy constants are constants that can't be evaluated until a global environment is provided.
-- They should behave like constants for all purposes of the compiler, then a genv can be provided
-- after globals are set in memory, and lazy constants shall be replaced with real constatns

-- | Maps global names to their constant address.
-- gives a default of 0 for undefined globals, those should be checked before. 
type GlobMap name wrdT = name -> wrdT

data LazyConst name wrdT =
   LConst ( GlobMap name wrdT -> wrdT)  -- ^ Lazy constants
  | SConst wrdT   -- ^ Static constant, allows optimizations/folding upstream and debugging

makeConcreteConst :: GlobMap name wrdT -> LazyConst name wrdT -> wrdT
makeConcreteConst gmap (LConst lw) = lw gmap
makeConcreteConst _    (SConst  w) = w


instance (Show wrdT) => Show (LazyConst name wrdT) where
  show (LConst _) = "LazyConstant"
  show (SConst w) = show w 

lazyBop :: (wrdT -> wrdT -> wrdT)
     -> LazyConst name wrdT -> LazyConst name wrdT -> LazyConst name wrdT
lazyBop bop (LConst l1) (LConst l2) = LConst $ \ge -> bop (l1 ge) (l2 ge) 
lazyBop bop (LConst l1) (SConst c2) = LConst $ \ge -> bop (l1 ge) c2
lazyBop bop (SConst c1) (LConst l2) = LConst $ \ge -> bop c1      (l2 ge)
lazyBop bop (SConst c1) (SConst c2) = SConst $ bop c1 c2

lazyUop :: (wrdT -> wrdT) -> LazyConst name wrdT -> LazyConst name wrdT
lazyUop uop (LConst l1) = LConst $ \ge -> uop (l1 ge) 
lazyUop uop (SConst c1) = SConst $ uop c1


instance Num wrdT => Num (LazyConst name wrdT) where
  (+) = lazyBop (+)
  (*) = lazyBop (*)
  (-) = lazyBop (-)
  negate = lazyUop negate 
  abs    = lazyUop abs   
  signum = lazyUop signum
  fromInteger n = SConst $ fromInteger n  

-- | Quick shorthand to return lists of one element
returnL :: Monad m => a -> m [a]
returnL x = return $ return $ x

checkName :: Set.Set LLVM.Name -> LLVM.Name -> Hopefully $ ()
checkName globs name =
  if Set.member name globs
  then return ()
  else assumptError $ "Global variable not defined \n \t" ++ show name ++ "\n"

-- | Duplicated from InstructionSelection
-- Can we unify


