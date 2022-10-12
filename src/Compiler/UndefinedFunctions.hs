{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Intrinsic lowering
Description : Converts calls to intrinsics to MicroASM instructions
Maintainer  : santiago@galois.com
Stability   : prototype
-}

module Compiler.UndefinedFunctions
    ( catchUndefinedFunctions,
    ) where

import Compiler.Errors (Hopefully, assumptError)
import Compiler.IRs

import Control.Monad (when)
import Data.List (sort)

import Util.Util


catchUndefinedFunctions :: Bool -> MIRprog m w -> Hopefully $ MIRprog m w
catchUndefinedFunctions undefAllowed prog = do
    when (not undefAllowed && not (null undefNames)) $ assumptError $
        "Undefinded function(s) found:\n" ++
        concat (map (\x -> "\t" ++ show x ++ "\n") $ sort undefNames) ++
        "Use --allow-undef to remove this warning."
    return prog
  where
    undefNames = externFuncs prog
