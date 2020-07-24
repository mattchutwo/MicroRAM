module Data.Queue where

import           Data.Sequence as Seq

newtype Queue a = Queue (Seq a)

fromList :: [a] -> Queue a
fromList = Queue . Seq.fromList

push :: a -> Queue a -> Queue a
push v (Queue q) = Queue (q :|> v)

pop :: Queue a -> Maybe (Queue a, a)
pop (Queue q) | (a :< q') <- viewl q = Just (Queue q', a)
pop _         | otherwise            = Nothing

