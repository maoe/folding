{-# LANGUAGE ExistentialQuantification #-}
module Control.Fold where
import Data.Foldable (toList)
import Prelude hiding
  ( Fold(..)
  , head
  , and
  , or
  , sum
  , product
  , length
  )

data Fold a b = forall x. Fold (x -> a -> (x -> x) -> x) x (x -> b)

data FoldM m a b = forall x. FoldM
  (x -> a -> (x -> x) -> m x)
  (m x)
  (x -> m b)

fold :: Foldable f => Fold a b -> f a -> b
fold (Fold step begin done) = done . cfold step begin . toList
  where
    cfold f z xs = case xs of
      [] -> z
      a:as -> f z a (\z' -> cfold f z' as)

head :: Fold a (Maybe a)
head = Fold step Nothing id
  where
    step x a k = case x of
      Nothing -> k $ Just a
      _ -> x

and :: Fold Bool Bool
and = Fold step True id
  where
    step a b k
      | a && b = k True
      | otherwise = False

or :: Fold Bool Bool
or = Fold step False id
  where
    step a b k
      | a || b = True
      | otherwise = k False

sum :: Num a => Fold a a
sum = Fold step 0 id
  where
    step x a k = k $! x + a

product :: Num a => Fold a a
product = Fold step 1 id
  where
    step x a k = k $! x * a

length :: Fold a Int
length = Fold step 0 id
  where
    step x _ k = k $! x + 1
