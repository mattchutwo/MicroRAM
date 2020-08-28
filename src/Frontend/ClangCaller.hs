
{-|
Module      : ClangCaller
Description : Inteface with clang
Maintainer  : santiago@galois.com
Stability   : alpha at best

Functionality to call the frontend of the
clang conmpiler call clang directly from haskell.
It does so through terminal, no FFIs.

-}

module Frontend.ClangCaller
  (
    callClang,
    ClangArgs(..),
    defualtClangArgs,
    
  ) where

import System.Process

-- | Arguments to a clang call
data ClangArgs = ClangArgs
  { input :: FilePath
  , output :: Maybe FilePath
  , optimisation :: Int
  , verboseClang :: Bool }

-- | Default arguments
defualtClangArgs :: String -> ClangArgs
defualtClangArgs input = ClangArgs input Nothing 0 False

toArguments :: ClangArgs -> [String]
toArguments (ClangArgs inp out opt verb) =
   [inp, "-S", "-emit-llvm"]
   ++ (setOutput out)
   ++ (setOptimisation opt)
   ++ verbStr
  where setOutput Nothing = []
        setOutput (Just out)  = ["-o", out]

        setOptimisation 0 = []
        setOptimisation 1 = ["-O"]
        setOptimisation 2 = ["-O2"]
        setOptimisation _ = ["-O3"]
        
        verbStr = if verb then ["-v"] else [] 

-- | Call the front end of the clang conmpiler
callClang :: ClangArgs -> IO String
callClang cargs= readProcess "clang" (toArguments cargs) ""

