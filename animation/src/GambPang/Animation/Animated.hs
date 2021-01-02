{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GambPang.Animation.Animated (
    Animated,
    getStill,

    -- * Frame
    frame,
    rescale,
    unitRescale,

    -- * Time
    Time,
    timeControl,
    time,
    compress,
    expand,
    shiftEarlier,
    shiftLater,
    backwards,
) where

import Control.Monad.Trans.Reader (Reader, asks, local, runReader)
import Data.Bifunctor (Bifunctor (second))

import GambPang.Animation.LinearAlgebra (Point (..))
import GambPang.Animation.Rectangle (
    Rectangle (..),
    height,
    unitRectangle,
    width,
 )

type Time = Double

newtype Animated a = Animated {getAnimated :: Reader (Rectangle, Time) a}
    deriving (Functor, Applicative, Monad)

getStill :: Rectangle -> Time -> Animated a -> a
getStill r t a = runReader (getAnimated a) (r, t)

time :: Animated Time
time = Animated $ asks snd

timeControl :: (Time -> Time) -> Animated a -> Animated a
timeControl f (Animated a) = Animated $ local (second f) a

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

frame :: Animated Rectangle
frame = Animated $ asks fst

-- | Given a point and a rectangle, produce a new point with the same relation to the animation frame
rescale :: Rectangle -> Point -> Animated Point
rescale r0@Rectangle{lowerLeft = Point llx0 lly0} (Point x y) = getRescaledPoint <$> frame
  where
    getRescaledPoint r1@Rectangle{lowerLeft = Point llx lly} =
        Point
            { pointX = llx + (x - llx0) * width r1 / width r0
            , pointY = lly + (y - lly0) * height r1 / width r1
            }

-- | Rescale in the context of the unit rectangle
unitRescale :: Point -> Animated Point
unitRescale = rescale unitRectangle
