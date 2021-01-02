{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GambPang.Animation.Scene (
    Time,
    Animated (..),
    valueAtTime,
    timeControl,
    time,
    compress,
    expand,
    shiftEarlier,
    shiftLater,
    backwards,
) where

type Time = Double

newtype Animated a = Animated (Time -> a) deriving (Functor, Applicative, Monad)

time :: Animated Time
time = Animated id

valueAtTime :: Time -> Animated a -> a
valueAtTime t (Animated f) = f t

timeControl :: (Time -> Time) -> Animated a -> Animated a
timeControl f (Animated a) = Animated $ a . f

-- | Speed up an animation.  So `compress 2 a` runs `a` and double speed (half time).
compress :: Double -> Animated a -> Animated a
compress a = timeControl $ (a *)

-- | Slow down an animation.  So `expand 2 a` makes `a` run in twice the time (half speed).
expand :: Double -> Animated a -> Animated a
expand a = timeControl (/ a)

shiftEarlier :: Time -> Animated a -> Animated a
shiftEarlier offset = timeControl (+ offset)

shiftLater :: Time -> Animated a -> Animated a
shiftLater offset = shiftEarlier $ negate offset

backwards :: Animated a -> Animated a
backwards = timeControl negate
