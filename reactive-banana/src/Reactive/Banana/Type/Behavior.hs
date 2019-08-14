{-# LANGUAGE
    NoImplicitPrelude
  #-}

module Reactive.Banana.Type.Behavior where

import Prelude ((+), (-), flip, fst, snd)

import Control.Applicative
import Control.Category ((.), id)
import Data.Functor
import Data.Bifunctor (bimap)
import Data.Semigroup
import Data.Monoid
import Data.Foldable
import Numeric.Natural


-- A behavior is modeled as an /infinite/ list of values.
newtype Behavior a = Behavior{ getBehavior ::(a, Behavior a) }

type Moment = (->) (Sum Natural)

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

-- | Convert a Behavior to a Moment.
-- (!!) :: Behavior a -> (Natural -> a)
(!!) :: Behavior a -> Moment a
xs !! 0 = head xs
xs !! i = tail xs !! (i - 1)

-- | Convert a Moment to a Behavior
memo :: Moment a -> Behavior a
memo f = go 0
   where go i = Behavior (f i, go (i + 1))

diagonalB :: Behavior (Behavior a) -> Behavior a
diagonalB = liftA2 (flip (!!)) (behaviorFrom (1 +) 0)

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

drop :: Sum Natural -> Behavior a -> Behavior a
drop 0 = id
drop n = drop (n - 1) . tail

take :: Sum Natural -> Behavior a -> [a]
take 0 _ = []
take n (Behavior (x, xs)) = x : take (n - 1) xs

prependList :: [a] -> Behavior a -> Behavior a
prependList = flip (foldr cons)
