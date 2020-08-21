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

      tagProg,
      tagPass,
      handleErrorWith

    ) where

import Control.Monad.State.Lazy
import Control.Monad.Except

import System.Exit
import System.IO

 -- ** Error handling
data CmplError =
  NotImpl String      -- Feature not implemented
  | CompilerAssumption String   -- Compiler assumption broken
  | OtherError String   -- Other error, stores the problem description.
  | ErrorAt String CmplError    -- An error at a particular location.

describeError :: CmplError -> String
describeError (ErrorAt loc e) = "while running " ++ loc ++ ": " ++ describeError e
describeError (NotImpl msg) = "feature not yet implemented: " ++ msg
describeError (CompilerAssumption msg) = "compiler assumption violated: " ++ msg
describeError (OtherError msg) = msg

type Hopefully = Either CmplError
ok :: Monad m => a -> m a
ok = return

implError :: MonadError CmplError m => String -> m b
implError msg = throwError $ NotImpl msg
assumptError :: MonadError CmplError m => String -> m b
assumptError msg = throwError $ CompilerAssumption msg
otherError :: MonadError CmplError m => String -> m b
otherError msg = throwError $ OtherError msg


tagProg :: String -> Hopefully a -> Hopefully a
tagProg pass (Left err) = Left $ ErrorAt pass err
tagProg _ (Right a) = Right a

tagPass :: String -> (a -> Hopefully b) -> (a -> Hopefully b)
tagPass passName pass prog = tagProg passName (pass prog)

handleErrorWith :: Hopefully a -> IO a
handleErrorWith (Left e) = do
  hPutStr stderr ("Backend compilation error: " ++ describeError e)
  exitWith (ExitFailure 1)
handleErrorWith (Right a) = return a
