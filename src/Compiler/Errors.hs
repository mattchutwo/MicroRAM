{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Compiler Errsrs
Description : Helful monads to compile with errors.
Maintainer  : santiago@galois.com
Stability   : prototype


-}

module Compiler.Errors
    ( Hopefully,
      implError,
      assumptError,
      otherError,

      CmplError,
      describeError,

      
      tag, tagPass,
      handleErrorWith

    ) where


import Control.Monad.Except

import System.Exit
import System.IO

-- ** Error handling

-- | Custom compiler errors
data CmplError =
  NotImpl String                -- ^ Feature not implemented
  | CompilerAssumption String   -- ^ Compiler assumption broken
  | OtherError String           -- ^ Other error, stores the problem description.
  | ErrorAt String CmplError    -- ^ An error at a particular location.

describeError :: CmplError -> String
describeError (ErrorAt loc e) = "while running " ++ loc ++ ": " ++ describeError e
describeError (NotImpl msg) = "feature not yet implemented: " ++ msg
describeError (CompilerAssumption msg) = "compiler assumption violated: " ++ msg
describeError (OtherError msg) = msg

-- | Monad for passing custom compiler errors. 
type Hopefully = Either CmplError

-- | Shorthands for common errors
implError, assumptError, otherError :: MonadError CmplError m => String -> m b
implError msg = throwError $ NotImpl msg
assumptError msg = throwError $ CompilerAssumption msg
otherError msg = throwError $ OtherError msg

-- | Tags a possible error with a location 
tag :: String -> Hopefully a -> Hopefully a
tag pass (Left err) = Left $ ErrorAt pass err
tag _ (Right a) = Right a

-- | Tags an function with a location (in case of error)
tagPass :: String -> (a -> Hopefully b) -> (a -> Hopefully b)
tagPass passName pass prog = tag passName (pass prog)

-- | Handler for compiler errors.
handleErrorWith :: Hopefully a -> IO a
handleErrorWith (Left e) = do
  hPutStr stderr ("Backend compilation error: " ++ describeError e)
  exitWith (ExitFailure 1)
handleErrorWith (Right a) = return a
