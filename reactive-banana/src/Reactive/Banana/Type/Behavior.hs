{-# LANGUAGE
    NoImplicitPrelude
  #-}

module Reactive.Banana.Type.Behavior where

import Prelude ((+), (-), fst, snd)

import Control.Applicative
import Control.Category ((.), id)
import Data.Functor
import Data.Bifunctor (bimap)
import Data.Semigroup
import Data.Foldable
import Numeric.Natural


-- A behavior is modeled as an /infinite/ list of values.
newtype Behavior a = Behavior{ getBehavior ::(a, Behavior a) }


----- Instances -----

instance Functor Behavior where
  fmap f = Behavior . bimap f (fmap f) . getBehavior

instance Applicative Behavior where
  pure x = let bx = Behavior (x, bx) in bx
  Behavior (f, fs) <*> Behavior ~(x, xs) = Behavior (f x, fs <*> xs)

instance Semigroup a => Semigroup (Behavior a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Behavior a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Foldable Behavior where
  foldr f _ = go
     where
     go xs = head xs `f` go (tail xs)


----- Functions -----

diagonalB :: Behavior (Behavior a) -> Behavior a
diagonalB = liftA2 indexing (behaviorFrom (1 +) 0)
  where
  indexing :: Natural -> Behavior a -> a
  indexing 0 = head
  indexing i = indexing (i - 1) . tail

behaviorFrom :: (a -> a) -> a -> Behavior a
behaviorFrom f = go
   where
   go  x = Behavior (x, go (f x))

cons :: a -> Behavior a -> Behavior a
cons x = Behavior . (,) x

head :: Behavior a -> a
head = fst . getBehavior

tail :: Behavior a -> Behavior a
tail = snd . getBehavior

tails :: Behavior a -> Behavior (Behavior a)
tails = behaviorFrom tail

drop :: Natural -> Behavior a -> Behavior a
drop 0 = id
drop n = drop (n - 1) . tail

take :: Natural -> Behavior a -> [a]
take 0 _ = []
take n (Behavior (x, xs)) = x : take (n - 1) xs
