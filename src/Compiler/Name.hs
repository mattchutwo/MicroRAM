{-# LANGUAGE OverloadedStrings #-}

module Compiler.Name (
  -- * Identifiers
  -- $names
  Name(..),
  short2string, string2short,
  defaultName, premainName, mainName,
  pcName,
  va_startName,
  firstUnusedName,

  WithNextReg,
  newGlobalName,
  newLocalName
) where

import Data.ByteString.Short (ShortByteString, fromShort, toShort) 
import qualified Data.ByteString.UTF8 as BSU
import           Data.Char (isDigit)

import Compiler.Registers

import Control.Monad.State (StateT, get, modify)

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
  readsPrec _ txt = case span (/= '#') txt of
                      (dbgName, '#':rest1) ->
                        let (nTxt, rest2) = span isDigit rest1
                            name = Name (read nTxt) (string2short dbgName) in
                          [(name, rest2)]
                      _ -> error ("Malformed variable name: " <> txt) 

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
spName, bpName, axName, pcName :: Name 
spName = Name 0 "sp"
bpName = Name 1 "bp"
axName = Name 2 "ax"
pcName = Name 3 "pc"
defaultName, premainName, mainName, va_startName :: Name
defaultName  = Name 4 "DefaultName" -- Can appear in metadata.
premainName  = Name 5 "premain"
mainName     = Name 6 "main"
va_startName = Name 7 "__cc_va_start"

-- | Bound for reserved names. 
firstUnusedName :: Word
firstUnusedName = 8
-- ^ TODO: ad the number of reserved names grows,
-- one might easily forget to update `firstUnusedName`.
-- We should create a set or a map with all the reserved names
-- and have `firstUnusedName` computed automatically.


-- | For compiler passes that need fresh registers:
type WithNextReg = StateT Word

-- | Make new names
newGlobalName, newLocalName :: Monad m => ShortByteString -> WithNextReg m Name
newGlobalName nm = makeNewName $ "@" <> nm
newLocalName  nm = makeNewName $ "%" <> nm

makeNewName :: Monad m => ShortByteString -> WithNextReg m Name
makeNewName nm = Name <$> useReg <*> pure nm
  where useReg = do
          nextID <- get
          modify $ (1 +)
          return nextID
