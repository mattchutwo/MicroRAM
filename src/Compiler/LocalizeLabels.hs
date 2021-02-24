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

import Data.ByteString.Short
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)

import Compiler.IRs
import Compiler.Errors

localizeLabels :: Rprog mdata wrdT -> Hopefully (Rprog mdata wrdT)
localizeLabels (IRprog te ge funcs) = return $ IRprog te ge (map (localizeFunc funcNames) funcs)
  where
     funcNames = Set.fromList $ map funcName funcs

localizeFunc :: Set Name -> RFunction mdata wrdT -> RFunction mdata wrdT
localizeFunc funcNames func = func { funcBlocks = map goBB $ funcBlocks func }
  where
    goBB (BB name body term dag) =
      BB (rename name) (map goInstr body) (map goInstr term) (map rename dag)

    goInstr (IRI i m) = IRI (goRTLInstr i) m
    goInstr (MRI i m) = MRI (fmap goOperand i) m

    goOperand (Label s) = Label $ show $ rename $ read s
    goOperand o = o

    goRTLInstr (RPhi ret inps) =
      RPhi ret (map (\(op, label) -> (goOperand op, rename label)) inps)
    goRTLInstr i = fmap goOperand i

    rename n
      | Set.member n funcNames = n
      | otherwise = Name $ nameStr (funcName func) <> "_" <> nameStr n

nameStr :: Name -> ShortByteString
nameStr (Name s) = s
nameStr (NewName w) = fromString $ show w
