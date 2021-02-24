{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : Common
Description : Compiler definitions: Types, Global variables, Identifiers.
Maintainer  : santiago@galois.com
Stability   : Prototype

Compiler definitions: Types, Global variables, Identifiers.

-}
module Compiler.Common (
  -- * Types
  -- $types
  Ty(..), tySize, TypeEnv,
  -- * Global variables
  -- $globs
  GlobalVariable(..), GEnv,
  -- * Identifiers
  -- $names
  Name(..),
  )
  where



import Data.ByteString.Short

import qualified Data.Map as Map

import Compiler.Registers
import Compiler.LazyConstants

import MicroRAM(MWord)



-- ** Types
-- $types
-- This type 'system' is used by the compiler backend. Pointer types don't expose
-- the type of the reference since we mostly just care about the size and
-- all pointers have the same size.
--
-- All the types here should be finite even though that is not enforced by the Haskell
-- Datatype.
-- LLVM's mutually recursive types must have a computable sizes, so they allways
-- "pass" through a pointer before a recursive referece. Thus, all those recursive
-- referece disappear in the backend and we allways get finite types.

-- | The type of something in the stack. Used to calculate
-- stack offsets for stack layout.
data Ty =
  TVoid
  | Tint
  | Tptr 
  | Tarray MWord Ty 
  | Tstruct [Ty]
  deriving (Show)

-- | Determines the relative size of types (relative to a 32bit integer/64bit)

tySize ::  Ty -> MWord
tySize typ =
  case typ of
    Tarray length subTyp -> length * (tySize subTyp) * 4
    Tstruct tys -> sum $ map tySize tys   
    TVoid -> 0
    _ -> 1 -- Pointers have the same sizer as Tint
{-  where
    {- TEMPORARY SOLUTION:
       Hybrid Memory model

If you have `struct { int a,b,c,d; }` and you set the four fields to `1,0,0,2`, LLVM may decide to coalesce the writes to `b` and `c` into a single `memset(&x.b, 0, 8)`, which interacts badly with our current memory model. 

If we rearranged the struct layout to put the fields at offsets `0,4,8,12` instead of `0,1,2,3`, then this would mostly work. Specifically, for all aggregate types (structs and arrays) integer fields will take 4 words space.
-} 
    tySizeAggregated typ =
      case typ of
        Tarray length subTyp -> length * (tySizeAggregated subTyp) * 4
        Tstruct tys -> sum $ map tySizeAggregated tys   
        TVoid -> 0
        _ -> 1 -- Pointers have the same sizer as Tint
-}

type TypeEnv = Map.Map Name Ty


-- * Global variables
-- $globs This is the representation of global variables until they are
-- set in memory and translated to constant pointers.

-- | This is the representation of global variables until they are
-- set in memory and translated to constant pointers. 
data GlobalVariable wrdT = GlobalVariable
  { globName :: Name -- Optimize?
  , isConstant :: Bool
  , gType :: Ty
  , initializer :: Maybe [LazyConst String wrdT]
    -- ^ A list of machine words to initialize this global
  , gSize :: MWord    -- ^ Size of this global in words
  , gAlign :: MWord
    -- ^ Alignment of this global in words.  This will be 1 for most globals,
    -- since primitives generally have alignment no greater than the word size,
    -- but the programmer can explicitly set a higher alignment with the right
    -- attributes.
  , secret :: Bool
  } deriving (Show)
type GEnv wrdT = [GlobalVariable wrdT] -- Maybe better as a map:: Name -> "gvar description"


-- * Name identifiers
-- $name These names are an extension to LLVM's register names.
-- It includes a `NewName` to produce temporary registers that
-- don't intefere with existing ones.
  
-- | These names are an extension to LLVM's register names.
-- It includes a `NewName` to produce temporary registers that
-- don't intefere with existing ones. 
data Name =
  Name ShortByteString   -- ^ we keep the LLVM names
  | NewName Word         -- ^ and add some new ones
  deriving (Eq, Ord, Read, Show)

instance Regs Name where
  sp = NewName 0
  bp = NewName 1
  ax = NewName 2
  -- argc = Name "0" -- Where the first arguemtns to main is passed
  -- argv = Name "1" -- Where the second arguemtns to main is passed
  fromWord w      -- FIXME this is terribled: depends on read and show! Ugh!
    | w == 1 = Name "0"
    | even w = NewName $ w `div` 2
    | otherwise = Name $ pack $ read $ show $ digits ((w-1) `div` 2)
  toWord (NewName x) = 2*x
  toWord (Name sh) = 1 + (2 * (read $ read $ show sh))
  
-- Produces the digits, shifted by 48 (ie. the ASCII representation)
digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10 + 48] -- ASCII 0 = 0
