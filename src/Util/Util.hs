{-# LANGUAGE TypeOperators #-}

{-|
Module      : Util
Description : Usful tools
Maintainer  : santiago@galois.com
Stability   : experimental

-}

module Util.Util where

-- | Lift this usefull operator to types
type ($) a b = a b

-- | From the Python function of the same name. :D
enumerate :: (Num n, Enum n) => [a] -> [(n, a)]
enumerate ls = zip [0..] ls

-- From Control.Monad.Extra
-- | A version of 'mapMaybe' that works with a monadic predicate.
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
{-# INLINE mapMaybeM #-}
mapMaybeM op = foldr f (pure [])
    where f x xs = do x <- op x; case x of Nothing -> xs; Just x -> do xs <- xs; pure $ x:xs


-- | take with words
takeEnum :: Enum a1 => a1 -> [a2] -> [a2]
takeEnum w = take (fromEnum w)
