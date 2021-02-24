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

import Util.Util


catchUndefinedFunctions :: Bool -> MIRprog m w -> Hopefully $ MIRprog m w
catchUndefinedFunctions undefAllowed prog = do
    when (not undefAllowed) $ mapM_ catchUndefinedFunction $ code prog
    return prog
  where catchUndefinedFunction :: MIRFunction m w -> Hopefully () 
        catchUndefinedFunction f = do
          when (null $ funcBlocks f) $ assumptError $
            "Undefinded function found: " ++ show (funcName f) ++ ". \n \t Use --allow-undef to remove this warning."
          return ()
