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
  GlobalVariable(..), GEnv, heapInitAddress,
  -- * Identifiers
  -- $names
  Name(..),
  short2string, string2short,
  defaultName, premainName, mainName,
  va_startName,
  firstUnusedName,

  WithNextReg,
  newGlobalName,
  newLocalName
  )
  where



import Data.ByteString.Short (ShortByteString, fromShort, toShort) 
import qualified Data.ByteString.UTF8 as BSU

import qualified Data.Map as Map

import Compiler.Registers
import Compiler.LazyConstants


import MicroRAM(MWord)
import Control.Monad.State (StateT, get, modify)



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
  , initializer :: Maybe [LazyConst Name wrdT]
    -- ^ A list of machine words to initialize this global
  , gSize :: MWord    -- ^ Size of this global in words
  , gAlign :: MWord
    -- ^ Alignment of this global in words.  This will be 1 for most globals,
    -- since primitives generally have alignment no greater than the word size,
    -- but the programmer can explicitly set a higher alignment with the right
    -- attributes.
  , secret :: Bool
  , gvHeapInit :: Bool
    -- | If set, this is a heap-init global variable, which is placed at a
    -- special address and doesn't influence the initial stack pointer.
  } deriving (Show)
type GEnv wrdT = [GlobalVariable wrdT] -- Maybe better as a map:: Name -> "gvar description"

heapInitAddress :: MWord
heapInitAddress = 0x100000000   -- 2^32


-- * Name identifiers
-- $name These names are an extension to LLVM's register names.
-- It includes a `NewName` to produce temporary registers that
-- don't intefere with existing ones.
  
-- | These names are an extension to LLVM's register names.
-- It includes a `NewName` to produce temporary registers that
-- don't intefere with existing ones. 
data Name =
  Name {nameID:: Word,
        dbName :: ShortByteString}   -- ^ Global identifier and a human readable name for debugging.

instance Eq Name where
  (Name n _) == (Name m _) = n == m

instance Ord Name where
  compare (Name n _ ) (Name m _) = compare n m

instance Show Name where
  show (Name n dbgName) = (short2string dbgName) <> "#" <> (show n)

instance Read Name where
  readsPrec _ txt = case splitLast '#' txt of
                      Right (dbgName, nTxt) -> [(Name (read nTxt) (string2short dbgName), "")]
                      Left _ -> error $  "Malformed variable name: " <> txt 
    where splitLast :: Eq a => a -> [a] -> Either [a] ([a],[a])
          splitLast c' = foldr go (Left [])
            where
              go c (Right (f,b)) = Right (c:f,b)
              go c (Left s) | c' == c = Right ([],s)
                            | otherwise = Left (c:s)

short2string :: ShortByteString -> String
short2string s = BSU.toString $ fromShort s

string2short :: String -> ShortByteString
string2short s = toShort $ BSU.fromString s

instance Regs Name where
  sp = spName
  bp = bpName
  ax = axName
  fromWord w = Name w ""
  toWord (Name n _) = n
  
-- | Reserved names.
spName, bpName, axName :: Name 
spName = Name 0 "sp"
bpName = Name 1 "bp"
axName = Name 2 "ax"
defaultName, premainName, mainName, va_startName :: Name
defaultName  = Name 3 "DefaultName-ShouldNeverAppearOnCode"
premainName  = Name 4 "premain"
mainName     = Name 5 "main"
va_startName = Name 6 "__cc_va_start"

-- | Bound for reserved names. 
firstUnusedName :: Word
firstUnusedName = 7
-- ^ TODO: ad the number of reserved names grows,
-- one might easily forget to update `firstUnusedName`.
-- We should create a set or a map with all the reserved names
-- and have `firstUnusedName` computed automatically.


-- | For compiler passes that need fresh registers:
type WithNextReg = StateT Word

-- | Make new names
newGlobalName, newLocalName :: forall m. Monad m => ShortByteString -> WithNextReg m Name
newGlobalName nm = makeNewName $ "@" <> nm
newLocalName  nm = makeNewName $ "%" <> nm

makeNewName :: forall m. Monad m => ShortByteString -> WithNextReg m Name
makeNewName nm = Name <$> useReg <*> pure nm
  where useReg = do
          nextID <- get
          modify $ (1 +)
          return nextID
