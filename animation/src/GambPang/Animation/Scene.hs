{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GambPang.Animation.Scene (
    Time (..),
    Animated (..),
    valueAtTime,
    timeControl,
    time,
    compress,
    expand,
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

-- | Speed up an animation.  So `compress 2 a` runs `a` and double speed (half time).
compress :: Double -> Animated a -> Animated a
compress a = timeControl $ \(Time t) -> Time (a * t)

-- | Slow down an animation.  So `expand 2 a` makes `a` run in twice the time (half speed).
expand :: Double -> Animated a -> Animated a
expand a = timeControl $ \(Time t) -> Time (t / a)

shift :: Time -> Animated a -> Animated a
shift (Time offset) = timeControl f
  where
    f (Time t) = Time $ t + offset

backwards :: Animated a -> Animated a
backwards = timeControl f
  where
    f (Time t) = Time $ negate t
