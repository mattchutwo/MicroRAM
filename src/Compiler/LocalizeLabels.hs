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

import Compiler.Common (Name(..))
import Compiler.Errors
import Compiler.IRs
import Control.Monad.State.Lazy (State, runState, get, put, modify)

localizeLabels :: (Rprog mdata wrdT, Word) -> Hopefully (Rprog mdata wrdT, Word)
localizeLabels (IRprog te ge funcs, nameBound) =
  let (funcs', RenameState nameBound' _) = runState (mapM (localizeFunc funcNames) funcs) $
        RenameState nameBound Map.empty in
  return $ (IRprog te ge funcs', nameBound')
  where
    -- Function names are not localized.
    funcNames = Map.fromList $ map (dup . funcName) funcs
    dup a = (a,a)
     
data RenameState = RenameState
  { _newName :: Word,
    renameMap :: Map.Map Name Name
  }

localizeFunc :: Map.Map Name Name -> RFunction mdata wrdT -> State RenameState (RFunction mdata wrdT)
localizeFunc funcNames func = do
  -- Starts by knowing only the function names
  -- but doesn't know about blocks in other funcitons.
  -- Functions all start with `@` and blocks with `%` so
  -- there are no clashes.
  modify $ \st -> st {renameMap = funcNames }  
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
    rename name = do
          RenameState nextName renameMap <- get
          case Map.lookup name renameMap of
            Just name' -> return name'
            Nothing -> do
              let name' = Name nextName (dbName (funcName func) <> "_" <> dbName name)
              put (RenameState (nextName + 1) (Map.insert name name' renameMap))
              return name'
