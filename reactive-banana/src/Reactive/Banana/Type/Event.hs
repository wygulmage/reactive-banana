
module Reactive.Banana.Type.Event where

import Reactive.Banana.Type.Behavior
import Control.Applicative
import Control.Monad
import Data.Functor.Compose (Compose (Compose), getCompose)
import Data.Bifunctor (bimap)

-- newtype Event a = E{ getEvent :: Behavior (Maybe a) }
type Event = Compose Behavior Maybe


never :: Event a
never = Compose (pure Nothing)

unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
unionWith = liftA2

-- fromBehavior :: Behavior a -> Event a
-- fromBehavior = Compose . Behavior . bimap Just fromBehavior . getBehavior
-- fromBehavior (Behavior (x, xs)) = Compose (Behavior (Just x, fromBehavior xs))


filterJust :: Event (Maybe a) -> Event a
filterJust = Compose . fmap join . getCompose

apply :: Behavior (a -> b) -> Event a -> Event b
apply fs = Compose . (<*>) (fmap fmap fs) . getCompose
