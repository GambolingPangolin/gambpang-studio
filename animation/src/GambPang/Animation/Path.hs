{-# LANGUAGE TypeApplications #-}

module GambPang.Animation.Path (
    Path,
    PiecewiseLinearPath (..),
    piecewiseLinear,
    pathProgram,
    makeCircular,
    circularPath,
) where

import Control.Monad.Trans.State (evalState, get, put)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import GambPang.Animation.LinearAlgebra (
    Point (Point),
    displacement,
    norm,
 )
import GambPang.Animation.Animated (Animated, Time, time, timeControl)

type Path = Animated Point

circularPath :: Time -> Point -> Double -> Animated Point
circularPath t (Point x y) r = thePath <$> time
  where
    thePath s = Point (x + r * cos (toRad s)) (y + r * sin (toRad s))
    toRad s = 2 * pi * s / t

-- | Repeat a given path segment forever
makeCircular :: Time -> Animated a -> Animated a
makeCircular t = timeControl toInterval
  where
    toInterval s = let q = floor @_ @Int $ s / t in s - fromIntegral q

-- | The resulting program takes unit time
pathProgram :: NonEmpty Point -> PiecewiseLinearPath
pathProgram (p :| []) = PiecewiseLinearPath{plpStartingPoint = p, plpSegments = mempty}
pathProgram ps@(_ :| (p : ps')) = mkPLP $ normalizeTime <$> pointPairs
  where
    pointPairs = NE.zipWith mkSegment ps (p :| ps')
    mkSegment p1 p2 = (dist p1 p2, p1, p2)
    totalTime = sum $ getTime <$> pointPairs
    getTime (t', _, _) = t'
    normalizeTime (t', p1, p2) = (t' / totalTime, p1, p2)
    dist p1 = norm . displacement p1
    mkPLP ((t', p1, p2) :| pts) =
        PiecewiseLinearPath
            { plpStartingPoint = p1
            , plpSegments = (t', p2) : (simpleSegment <$> pts)
            }
    simpleSegment (t', _, p2) = (t', p2)

piecewiseLinear :: PiecewiseLinearPath -> Path
piecewiseLinear plp = thePath plan <$> time
  where
    thePath (lps : lpss) t
        | 0 >= t = startPoint lps
        | t <= endTime lps =
            interpolate (startPoint lps) (endPoint lps) $ (t - startTime lps) / duration lps
        | otherwise = thePath lpss t
    thePath [] _ = finalPoint plp

    plan = toPathPlan $ toSegments plp

interpolate :: Point -> Point -> Double -> Point
interpolate (Point x1 y1) (Point x2 y2) t = Point (convex x1 x2) (convex y1 y2)
  where
    convex u v = (1 - t) * u + t * v

data PiecewiseLinearPath = PiecewiseLinearPath
    { plpStartingPoint :: Point
    , plpSegments :: [(Time, Point)]
    }
    deriving (Eq, Show)

toSegments :: PiecewiseLinearPath -> [(Time, Point, Point)]
toSegments plp = zipWith mkSegment ps (plpSegments plp)
  where
    mkSegment p (t, p') = (t, p, p')
    ps = plpStartingPoint plp : (snd <$> plpSegments plp)

finalPoint :: PiecewiseLinearPath -> Point
finalPoint plp
    | null (plpSegments plp) = plpStartingPoint plp
    | otherwise = snd . last $ plpSegments plp

data LinearSegment = LinearSegment
    { startTime :: Double
    , endTime :: Double
    , startPoint :: Point
    , endPoint :: Point
    }
    deriving (Eq, Show)

duration :: LinearSegment -> Double
duration lps = endTime lps - startTime lps

toPathPlan :: [(Time, Point, Point)] -> [LinearSegment]
toPathPlan ss = evalState (traverse accumTime ss) 0
  where
    accumTime (dt, p1, p2) = do
        t0 <- get
        put $ dt + t0
        pure
            LinearSegment
                { startTime = t0
                , endTime = t0 + dt
                , startPoint = p1
                , endPoint = p2
                }
