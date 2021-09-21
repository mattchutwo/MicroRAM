{-# LANGUAGE TypeOperators #-}

{-|
Module      : Util
Description : Usful tools
Maintainer  : santiago@galois.com
Stability   : experimental

-}

module Util.Util where
import qualified Numeric

-- | Lift this usefull operator to types
type ($) a b = a b

-- | Pretty print Hex code
showHex :: (Integral a, Show a) => a -> [Char]
showHex w = "0x" ++ Numeric.showHex w ""


-- | From the Python function of the same name. :D
enumerate :: (Num n, Enum n) => [a] -> [(n, a)]
enumerate ls = zip [0..] ls

-- From Control.Monad.Extra
-- | A version of 'mapMaybe' that works with a monadic predicate.
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
{-# INLINE mapMaybeM #-}
mapMaybeM op = foldr f (pure [])
    where f x xs = do x <- op x; case x of Nothing -> xs; Just x -> do xs <- xs; pure $ x:xs

-- From Control.Monad.Loops
-- | Yields the result of applying f until p holds.
iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f v 
    | p v       = return v
    | otherwise = f v >>= iterateUntilM p f

-- | take with words
takeEnum :: Enum a1 => a1 -> [a2] -> [a2]
takeEnum w = take (fromEnum w)

-- |
infixr 9 <.>
(<.>) :: Functor f => (a -> b) -> (c -> f a) -> c -> f b
f1 <.> f2 = fmap f1 . f2

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a,c) = (f a, c)
mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a,b) = (a, f b)

mapFstM :: Monad m => (a -> m b) -> (a, c) -> m (b, c)
mapFstM f (a, c) = do {b <- f a; return (b, c)}

mapSndM :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
mapSndM f (a,b) = do {c <- f b ; return (a, c)}
