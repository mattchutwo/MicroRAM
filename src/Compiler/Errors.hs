{-# LANGUAGE DeriveFunctor #-}

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
      
      CompilerPassError,
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

-- | Converts Error to a readable message.
instance Show CmplError where
  show (NotImpl msg) =
      "Feature not supported by the compiler yet: " ++ msg
  show (CompilerAssumption msg) = "Compiler assumption broken: " ++ msg
  show (OtherError msg) = msg

type Hopefully = Either CmplError
ok :: a -> Hopefully a
ok result = Right result
implError, assumptError, otherError :: String -> Hopefully b
implError msg = Left $ NotImpl msg
assumptError msg = Left $ CompilerAssumption msg
otherError msg = Left $ OtherError msg

data CompilerPassError a =
  CompilerOK a
  | CompilerError
    String -- ^ The compiler pass
    CmplError
  deriving Functor
    
instance Applicative CompilerPassError where
  pure a = (CompilerOK a)
  
  CompilerOK f <*> CompilerOK a = CompilerOK $ f a
  CompilerError a b <*> _ = CompilerError a b
  _ <*> CompilerError a b = CompilerError a b
  
instance Monad CompilerPassError where
  (CompilerOK a) >>= f = f a
  CompilerError p e >>= _ = CompilerError p e


  a >> b = b

  return a = CompilerOK a

tagProg :: String -> Hopefully a -> CompilerPassError a
tagProg pass (Left err) = CompilerError pass err
tagProg pass (Right a) = CompilerOK a

tagPass :: String -> (a -> Hopefully b) -> (a -> CompilerPassError b)
tagPass passName pass prog = tagProg passName (pass prog)

handleErrorWith :: CompilerPassError a -> (a -> IO ()) -> IO ()
handleErrorWith (CompilerError pass error) _ = do
  hPutStr stderr ("Backend compilation error while doing " ++ pass ++
                  ":\n \n \t" ++ show error)
  exitWith (ExitFailure 1)
handleErrorWith (CompilerOK a) f = f a
