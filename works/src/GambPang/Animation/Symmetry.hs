{-# LANGUAGE OverloadedStrings #-}

module GambPang.Animation.Symmetry (
    animations,
    pentagon1,
) where

import Control.Monad (zipWithM)
import Data.List (inits, tails, uncons)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import GambPang.Animation (
    Animated,
    Drawing,
    Point (Point),
    Rigged (..),
    compress,
    displacement,
    followPath,
    origin,
    pathProgram,
    piecewiseLinear,
    pointToVector,
    rotate,
    scale,
    shiftLater,
    time,
    translate,
    valueAtTime,
 )
import GambPang.Animation.ColorStyle (
    ColorStyle (..),
    PaletteChoice,
    nightlights,
 )
import GambPang.Animation.Drawing (Shape)
import qualified GambPang.Animation.Drawing as D
import GambPang.Animation.Piece (
    AnimatedPiece (..),
    AnimationSource (AnimatedDrawing),
    applyPaletteChoice,
 )
import GambPang.Animation.Utils (originViewFrame)

animations :: PaletteChoice -> Map Text AnimatedPiece
animations paletteChoice =
    applyPaletteChoice paletteChoice
        <$> Map.fromList [("pentagon-1", pentagon1)]

regularPolygon :: Int -> Shape
regularPolygon n = D.polygon . reverse $ unitCircle . toAngle n <$> [1 .. n]

toAngle :: Int -> Int -> Double
toAngle n i = 2 * pi * (fromIntegral i / fromIntegral n)

data TriangularSegment = TriangularSegment
    { triangle :: Shape
    , handlePoint :: Point
    , position :: Int
    }

instance Rigged TriangularSegment where
    transform t ts =
        ts
            { triangle = transform t $ triangle ts
            , handlePoint = transform t $ handlePoint ts
            }

rotateAboutHandle :: Double -> TriangularSegment -> TriangularSegment
rotateAboutHandle a ts = rotate (handlePoint ts) a ts

regularTriangleSet :: Int -> [TriangularSegment]
regularTriangleSet n = zipWith toTriangle [1 ..] cwPairs
  where
    points = unitCircle . toAngle n <$> [1 .. n]
    cwPairs = zip (drop 1 $ cycle points) points
    toTriangle ix (p0, p1) =
        TriangularSegment
            { triangle = D.polygon [p0, p1, origin]
            , handlePoint = getHandlePoint n ix
            , position = ix
            }

getHandlePoint :: Int -> Int -> Point
getHandlePoint n ix = Point x y
  where
    a0 = toAngle n $ ix + 1
    a1 = toAngle n ix
    x = (cos a0 + cos a1) / 2
    y = (sin a0 + sin a1) / 2

unitCircle :: Double -> Point
unitCircle a = Point (cos a) (sin a)

permutations :: [a] -> [[a]]
permutations [x] = [[x]]
permutations xs = uncurry mkPermutations =<< zip fronts backs
  where
    fronts = inits xs
    backs = mapMaybe uncons $ tails xs
    mkPermutations fs (x, bs) = (x :) <$> permutations (fs <> bs)

-- | In which the triangles of a pentagon run through all 120 permutations, 2 per second
pentagon1 :: AnimatedPiece
pentagon1 =
    AnimatedPiece
        { source = AnimatedDrawing . resize $ D.union <$> sequenceA [segments, pure underlayer]
        , viewFrame = originViewFrame
        , framesPerSec = 50
        , frameCount = 100
        , palette = nightlights
        }
  where
    segments = animConcat . take 5 $ animatePermutation 5 ts <$> permutations [1 .. 5]
    ts = regularTriangleSet 5
    underlayer = D.draw HighlightA $ regularPolygon 5
    resize = scale 120

animatePermutation :: Int -> [TriangularSegment] -> [Int] -> Animated (Drawing ColorStyle)
animatePermutation n ts ps = D.union <$> zipWithM (animateTriangle n) ts ps

animateTriangle :: Int -> TriangularSegment -> Int -> Animated (Drawing ColorStyle)
animateTriangle n t nextPosition =
    D.union <$> sequenceA [dot, drawTriangle <$> animConcat [slide, reposition]]
  where
    slide = slideTriangleOut t 0.25
    outPosition = valueAtTime 1 slide
    reposition = rotateAndSlide n nextPosition outPosition
    drawTriangle = D.draw Foreground . triangle

    dot = followPath dotPath origin <*> pure (D.draw HighlightB $ D.disc origin 10)
    dotPath =
        piecewiseLinear . pathProgram 1 $
            handlePoint t :| [handlePoint outPosition, handlePoint t]

-- | Move a triangle of the polygon
slideTriangleOut :: TriangularSegment -> Double -> Animated TriangularSegment
slideTriangleOut ts scalingFactor = getTranslation <$> time <*> pure ts
  where
    p = handlePoint ts
    getTranslation t = translate . scale (t * scalingFactor) $ pointToVector p

-- | Move a triangle to its destination, while rotating it as appropriate
rotateAndSlide :: Int -> Int -> TriangularSegment -> Animated TriangularSegment
rotateAndSlide n ix ts = applyTranslation <*> (applyRotation <*> pure ts)
  where
    applyTranslation = translate . getDisplacement <$> time
    getDisplacement t = scale t $ displacement p0 p1

    applyRotation = rotateAboutHandle . getAngle <$> time
    getAngle t = 2 * pi * t * fromIntegral (ix - pos0) / fromIntegral n

    p1 = getHandlePoint n ix

    p0 = handlePoint ts
    pos0 = position ts

-- | Schedule a bunch of unit animations back to back
animConcat :: [Animated a] -> Animated a
animConcat as = time >>= play . getSlot
  where
    n = length as
    nd = fromIntegral n
    getSlot t = floor (nd * t) `mod` n
    play ix = shiftLater (fromIntegral ix / nd) . compress nd $ as !! ix
