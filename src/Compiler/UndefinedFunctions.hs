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


-- import Control.Monad
-- import qualified Data.ByteString.Short as Short
-- import qualified Data.ByteString.UTF8 as BSU
-- import qualified Data.Map as Map
-- import Data.Map (Map)
-- import qualified Data.Set as Set
-- import qualified Data.Text as Text

import Compiler.Errors (Hopefully, assumptError)
import Compiler.IRs
-- import Compiler.LazyConstants

import Control.Monad (when)

-- import MicroRAM
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
