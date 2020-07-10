{-# LANGUAGE TypeOperators #-}
module Util.Util where
  
import Control.Monad.State.Lazy
import Control.Monad.Except

type ($) a b = a b


-- From Python :D
enumerate :: (Num n, Enum n) => [a] -> [(n, a)]
enumerate ls = zip [0..] ls
