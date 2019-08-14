{-# LANGUAGE
    RecursiveDo
  #-}

module Reactive.Banana.Model where

import Prelude (($), (+), (-), const, flip, fst, snd, fromIntegral, replicate)

import Control.Applicative (Applicative ((<*>), pure, liftA2))
import Control.Category ((.), id)
import Control.Monad
import Data.Function ((&))
import Data.Bifunctor (bimap)
import Data.Functor
import Data.Semigroup
import Data.Monoid
import Data.Foldable (Foldable (foldr))

import Data.Functor.Compose (Compose (Compose), getCompose)
import Data.Maybe
import qualified Data.List as List
import Numeric.Natural


interpret :: (Event a -> Moment (Event b)) -> [Maybe a] -> [Maybe b]
interpret f xs =
  take (fromIntegral $ List.length xs) . unE . ($ 0) . f . Compose $ prependList xs (unE never)


-- A behavior is modeled as an /infinite/ list of values.
newtype Behavior a = Behavior{ getBehavior ::(a, Behavior a) }

-- | Time is modeled as discrete and moving only forward.
type Time = Max Natural

-- | A moment gives a behavior at one point in time.
-- (The isomorphism between Behavior and Moment is witnessed by (!!) and enumerate.)
type Moment = (->) Time

type Event = Compose Behavior Maybe

----- Behavior Instances -----

instance Functor Behavior where
  fmap f = Behavior . bimap f (fmap f) . getBehavior

instance Applicative Behavior where
  pure x = xs where xs = cons x xs
  liftA2 f = loop
    where
    loop xs ys =
      cons (head xs `f` head ys) (tail xs `loop` tail ys)

instance Semigroup a => Semigroup (Behavior a) where
  -- There would be no point to putting behaviors end-to-end, so zip them.
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Behavior a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Foldable Behavior where
  foldr f _ = foldr1 f


----- Functions -----

--- Behavior ---
-- | Convert a Behavior to a function.
(!!) :: Behavior a -> Time -> a
xs !! n = (head . drop n) xs

valueB :: Behavior a -> Time -> a
valueB = (!!)

time :: Behavior Time
time = iterate (+ 1) 0

-- | Sample each sub-behavior at its occurrence in the behavior.
diagonalB :: Behavior (Behavior a) -> Behavior a
diagonalB = liftA2 (flip (!!)) time

-- | Create a Behavior from a function and a seed.
iterate :: (a -> a) -> a -> Behavior a
iterate f = loop
   where
   loop x = cons x (loop (f x))

foldr1 :: (a -> b -> b) -> Behavior a -> b
foldr1 f = loop
   where
   loop xs = head xs `f` loop (tail xs)

prependList :: [a] -> Behavior a -> Behavior a
prependList = flip (foldr cons)

cons :: a -> Behavior a -> Behavior a
cons x = Behavior . (,) x

head :: Behavior a -> a
head = fst . getBehavior

tail :: Behavior a -> Behavior a
tail = snd . getBehavior

tails :: Behavior a -> Behavior (Behavior a)
-- diagonalB . tails = id
tails = iterate tail

drop :: Time -> Behavior a -> Behavior a
-- ^ Move t = 0 forward in time. (Or, relatively, move the behavior backward in time.)
drop 0 = id
drop n = drop (n - 1) . tail

take :: Time -> Behavior a -> [a]
-- ^ Sample a period of time starting at 0.
take 0 _ = []
take n (Behavior (x, xs)) = x : take (n - 1) xs

cycle :: [a] -> Behavior a
-- ^ Cycle through xs forever.
cycle xs = xs' where xs' = prependList xs xs'


--- Event ---

unE :: Event a -> Behavior (Maybe a)
unE = getCompose

never :: Event a
-- ^ Nothing ever happens.
never = Compose (pure Nothing)

timeE :: Event Time
timeE = behaviorToEvent time

unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
unionWith f (Compose xs) (Compose ys) = Compose (liftA2 combine xs ys)
   where
   combine (Just x) = Just . maybe x (f x)
   combine _ = id

filterJust :: Event (Maybe a) -> Event a
filterJust = Compose . fmap join . getCompose

forgetE :: Time -> Event a -> Event a
-- ^ forget all events before time n. (I.e. move the events back in time.)
forgetE n = Compose . drop n . getCompose

accumE :: a -> Event (a -> a) -> Moment (Event a)
accumE x fs = mdo
  let gs = fmap (&) ys `apply` fs
  ys <- stepper x gs
  pure gs

observeE :: Event (Moment a) -> Event a
observeE = apply (fmap (&) time)

forgetDiagonalE :: Event (Event a) -> Event (Event a)
forgetDiagonalE = liftA2 forgetE timeE

switchE :: Event (Event a) -> Moment (Event a)
switchE xss n = Compose $ take n (unE never) `prependList` switch (unE never) (drop n (unE (forgetDiagonalE xss)))
    where
    switch (Behavior (x, _)) (Behavior (Just (Compose xs), ys)) = x `cons` switch (tail xs) ys
    switch (Behavior (x, xs)) (Behavior (_, ys)) = x `cons` switch xs ys
    -- switch xs ys = case head ys of
       -- Just xs' -> cons (head xs) (switch (tail xs') ys)
       -- _ -> cons (head xs) (switch (tail xs) ys)

--- Functions that combine Behaviors and Events ---

-- | Apply a Behavior function to an Event to get a new Event.
apply :: Behavior (a -> b) -> Event a -> Event b
apply fs = Compose . liftA2 fmap fs . getCompose

stepper :: a -> Event a -> Moment (Behavior a)
stepper x (Compose xs) n = prependList
  (replicate (fromIntegral (getMax n)) x)
  (step x (drop n xs))
  where
  step y ys = cons y (step (fromMaybe y (head ys)) (tail ys))

behaviorToEvent :: Behavior a -> Event a
behaviorToEvent = Compose . fmap Just

switchB :: Behavior a -> Event (Behavior a) -> Moment (Behavior a)
switchB xs xss = diagonalB <$> stepper xs xss
