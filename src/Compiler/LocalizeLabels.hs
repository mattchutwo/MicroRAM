{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : LocalizeLabels
Description : Rename "local" block labels to avoid conflicts between functions
Maintainer  : santiago@galois.com
Stability   : Prototype

Basically adds the name of the function to each block so two functions can't have
overlapping names.

-}

module Compiler.LocalizeLabels
    ( localizeLabels
    ) where

import qualified Data.Map as Map 
import Data.Set (Set)
import qualified Data.Set as Set
-- import Data.String (fromString)

import Compiler.Common (Name(..))
import Compiler.Errors
import Compiler.IRs
import Control.Monad.State.Lazy (State, runState, get, put)

localizeLabels :: (Rprog mdata wrdT, Word) -> Hopefully (Rprog mdata wrdT, Word)
localizeLabels (IRprog te ge funcs, nameBound) =
  let (funcs', RenameState nameBound' _) = runState (mapM (localizeFunc funcNames) funcs) $
        RenameState nameBound Map.empty in
  return $ (IRprog te ge funcs', nameBound')
  where
     funcNames = Set.fromList $ map funcName funcs

data RenameState = RenameState
  { _newName :: Word,
    _renameMap :: Map.Map Name Name
  }

localizeFunc :: Set Name -> RFunction mdata wrdT -> State RenameState (RFunction mdata wrdT)
localizeFunc funcNames func = do
  blocks' <- mapM goBB $ funcBlocks func
  return $ func { funcBlocks = blocks' }
  where
    goBB (BB name body term dag) = do
      BB <$> (rename name)
        <*> (mapM goInstr body)
        <*> (mapM goInstr term)
        <*> (mapM rename dag)

    goInstr (IRI i m) = flip IRI m <$> (goRTLInstr i)
    goInstr (MRI i m) = flip MRI m <$> (mapM goOperand i)

    goOperand (Label s) = Label <$> rename s
    goOperand o = return o

    goRTLInstr (RPhi ret inps) = 
      RPhi ret <$>
      mapM (\(op, label) -> do
               label' <- rename label
               op' <- goOperand op
               return (op', label')) inps
    goRTLInstr i = mapM goOperand  i

    rename :: Name -> State RenameState Name
    rename name
      | Set.member name funcNames = return name
      | otherwise = do
          RenameState nextName renameMap <- get
          case Map.lookup name renameMap of
            Just name' -> return name'
            Nothing -> do
              let name' = Name nextName (dbName (funcName func) <> "_" <> dbName name)
              put (RenameState (nextName + 1) (Map.insert name name' renameMap))
              return name'
