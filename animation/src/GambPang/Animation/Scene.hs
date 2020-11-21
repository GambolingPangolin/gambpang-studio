{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GambPang.Animation.Scene (
    Time (..),
    Animated (..),
    valueAtTime,
    timeControl,
    time,
    shift,
    backwards,
) where

import Data.Monoid (Sum (..))

newtype Time = Time {unTime :: Double}
    deriving (Eq, Ord, Show)
    deriving (Semigroup, Monoid) via Sum Double

newtype Animated a = Animated (Time -> a) deriving (Functor, Applicative, Monad)

time :: Animated Time
time = Animated id

valueAtTime :: Time -> Animated a -> a
valueAtTime t (Animated f) = f t

timeControl :: (Time -> Time) -> Animated a -> Animated a
timeControl f (Animated a) = Animated $ a . f

shift :: Time -> Animated a -> Animated a
shift (Time offset) = timeControl f
  where
    f (Time t) = Time $ t + offset

backwards :: Animated a -> Animated a
backwards = timeControl f
  where
    f (Time t) = Time $ negate t
