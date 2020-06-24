
{-|
Module      : Compiler Errsrs
Description : Helful monads to compile with errors.
Maintainer  : santiago@galois.com
Stability   : prototype


-}

module Compiler.CompileErrors
    ( Hopefully,
      implError,
      assumptError,
      otherError,
    ) where

import Control.Monad.State.Lazy
import Control.Monad.Except

 -- ** Error handling
data CmplError = NotImpl String      -- Feature not implemented
               | CompilerAssumption String   -- Compiler assumption broken
               | OtherError String   -- Other error, stores the problem description.

-- | Converts Error to a readable message.
instance Show CmplError where
  show (NotImpl msg) =
      "The following feature has not been yet supported by the compiler: " ++ msg
  show (CompilerAssumption msg) = "Compiler assumption broken: " ++ msg
  show (OtherError msg) = msg

type Hopefully = Either CmplError
ok :: a -> Hopefully a
ok result = Right result
implError msg = Left $ NotImpl msg
assumptError msg = Left $ CompilerAssumption msg
otherError msg = Left $ OtherError msg
