{-|
Module      : CBOR Format
Description : Format the output of the compiler and interpreter as CBOR
              to communicate down with the Circuit generator.
Maintainer  : santiago@galois.com
Stability   : experimental

Format for compiler units

-}

module Output.VersionHandler
  (Version, getVersion) where
  
--import System.IO  
type Version = (Int, Int)

getVersion :: IO Version
getVersion = do
  content <- readFile "VERSION"
  let versionList = read <$> words [if c == '.' then ' ' else c|c <- content] in
--      versionPair = if length version == 2 then (versions !! 0, versions !! 1) else (0,0)  
    return (versionList !! 0, versionList !! 1)

-- printVersion :: IO ()
-- printVersion = do
--   str <- getVersion
--   print str
