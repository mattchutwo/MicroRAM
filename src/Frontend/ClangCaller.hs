module Frontend.ClangCaller where

import System.Process


data ClangArgs = ClangArgs
  { input :: FilePath
  , output :: Maybe FilePath
  , optimisation :: Int }

defualtClangArgs :: String -> ClangArgs
defualtClangArgs input = ClangArgs input Nothing 0


toArguments :: ClangArgs -> [String]
toArguments (ClangArgs inp out opt) =
   [inp, "-S", "-emit-llvm"] ++
   setOutput out ++
   setOptimisation opt
   where setOutput Nothing = []
         setOutput (Just out)  = ["-o", out]

         setOptimisation 0 = []
         setOptimisation 1 = ["-O"]
         setOptimisation 2 = ["-O2"]
         setOptimisation _ = ["-O3"]
         

callClang :: ClangArgs -> IO String
callClang cargs= readProcess "clang" (toArguments cargs) ""

