
module Reactive.Banana.Type.Event where

import qualified Reactive.Banana.Type.Behavior as B
import Control.Applicative
import Control.Monad
import Data.Functor.Compose (Compose (Compose), getCompose)
import Data.Bifunctor (bimap)
import Data.Semigroup
import Data.Maybe (fromMaybe)
import Numeric.Natural

-- newtype Event a = E{ getEvent :: Behavior (Maybe a) }
type Event = Compose B.Behavior Maybe

unE :: Event a -> B.Behavior (Maybe a)
unE = getCompose

never :: Event a
never = Compose (pure Nothing)


fromBehavior :: B.Behavior a -> Event a
fromBehavior = Compose . go
  where go = B.Behavior . bimap Just go . B.getBehavior


filterJust :: Event (Maybe a) -> Event a
filterJust = Compose . fmap join . getCompose

apply :: B.Behavior (a -> b) -> Event a -> Event b
apply fs = Compose . (<*>) (fmap fmap fs) . getCompose

-- | Just the elements of a list, then Nothing.
fromList :: [a] -> Event a
fromList = Compose . go
  where
  go = foldr (B.cons . Just) (getCompose never)

forget :: Sum Natural -> Event a -> Event a
forget n = Compose . B.drop n . getCompose
