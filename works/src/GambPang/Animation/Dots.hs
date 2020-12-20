{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module GambPang.Animation.Dots (
    animations,
    dots1,
    dots2,
    dots3,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import GambPang.Animation (
    Animated (..),
    Path,
    Point (..),
    Time (..),
    Vector (..),
    followPath,
    origin,
    pathProgram,
    piecewiseLinear,
    rotateO,
    shift,
    time,
    translate,
 )
import GambPang.Animation.Drawing (Drawing)
import qualified GambPang.Animation.Drawing as D

import GambPang.Animation.ColorStyle (
    ColorStyle (..),
    PaletteChoice,
    snowy,
 )
import GambPang.Animation.Piece (
    AnimatedPiece (..),
    AnimationSource (AnimatedDrawing),
    applyPaletteChoice,
 )
import GambPang.Animation.Utils (defaultAnimatedPiece, defaultViewFrame, originViewFrame, rotating)

animations :: PaletteChoice -> Map Text AnimatedPiece
animations paletteChoice =
    applyPaletteChoice paletteChoice
        <$> Map.fromList
            [ ("dots-1", dots1)
            , ("dots-2", dots2)
            , ("dots-3", dots3)
            , ("dots-4", dots4)
            , ("dots-5", dots5)
            ]

dots1 :: AnimatedPiece
dots1 = defaultAnimatedPiece . translate v' $ rotating 1 <*> pure s0
  where
    d = D.draw Foreground $ D.disc origin 10
    s0 = translate v d
    v = Vector 0 175
    v' = Vector 250 250

dots2 :: AnimatedPiece
dots2 = defaultAnimatedPiece $ translate v $ union2 <$> movingDot <*> pure field
  where
    n = 5
    v = Vector offset offset
    offset = 50
    gapSize = 400 / fromIntegral (n - 1)
    field = dotField gapSize $ uniformDotFieldSpec n 5 Foreground

    union2 x y = D.union [x, y]

    movingDot = followPath path origin <*> pure d

    d = D.draw HighlightA $ D.disc origin 20
    path =
        piecewiseLinear $
            pathProgram
                (Time 1)
                ( Point 0 0
                    :| [ Point 400 0
                       , Point 400 400
                       , Point 0 400
                       , Point 0 0
                       ]
                )

dotField ::
    -- | Gap
    Double ->
    Map (Int, Int) (Double, c) ->
    Drawing c
dotField s colors = D.union $ Map.foldlWithKey addDot mempty colors
  where
    addDot dots (x, y) (r, c) = D.draw c (D.disc (center x y) r) : dots
    center x y = Point (s * fromIntegral x) (s * fromIntegral y)

uniformDotFieldSpec :: Int -> Double -> c -> Map (Int, Int) (Double, c)
uniformDotFieldSpec n r c =
    Map.fromList $ [((x, y), (r, c)) | x <- [0 .. n - 1], y <- [0 .. n - 1]]

dots3 :: AnimatedPiece
dots3 = defaultAnimatedPiece . translate v . fmap D.union $ traverse mkDot [0 .. 10]
  where
    mkDot i = shift (Time $ i / 10) dot
    dot = followPath sinPath p0 <*> pure d
    sinPath = Animated $ \(Time t) -> let s = toCircular t in Point (500 * s) (200 * sin (2 * pi * s))
    v = Vector 0 250
    d = D.draw Foreground $ D.disc origin 10
    p0 = origin
    toCircular = snd . properFraction @_ @Int

dots4 :: AnimatedPiece
dots4 =
    AnimatedPiece
        { source =
            AnimatedDrawing $
                translate v . D.union
                    <$> sequenceA
                        [ dotElement
                        , rotateO (2 * pi / 3) dotElement
                        , rotateO (4 * pi / 3) dotElement
                        ]
        , viewFrame = defaultViewFrame
        , frameCount = 200
        , framesPerSec = 33
        , palette = snowy
        }
  where
    dotElement = rotateO <$> (mkAngle <$> time) <*> pure d
    d = D.draw Foreground $ D.disc pCenter 20
    mkAngle (Time t)
        | t <= 0.5 = 2 * m * pi * progress (2 * t)
        | otherwise = negate $ 2 * m * progress (2 * t - 1)
    progress t = sin (pi * t)
    pCenter = Point 100 0
    v = Vector 250 250
    m = 2

-- | In which dots coil around an annulus
dots5 :: AnimatedPiece
dots5 = piece{viewFrame = originViewFrame, frameCount = 500}
  where
    piece = defaultAnimatedPiece $ withSwapping isAboveAnnulus (pure annulus) coilingDot
    annulus = D.exclude innerCircle $ D.draw Foreground outerCircle
    outerCircle = D.disc origin rOuter
    innerCircle = D.disc origin rInner

    rInner = 150
    rOuter = 200
    rCoiler = 20

    coiler = D.draw HighlightA $ D.disc origin rCoiler

    rInnerPath = rInner - rCoiler - 5
    rOuterPath = rOuter + rCoiler + 5

    coilingDot = followPath (toroidalPath rInnerPath rOuterPath verticalWindingNumber 1) origin <*> pure coiler
    isAboveAnnulus (Time t) = sin (2 * pi * verticalWindingNumber * t) > 0

    verticalWindingNumber = 5

    withSwapping criterion x y = implementLayering criterion <$> time <*> x <*> y

    implementLayering criterion t x y
        | criterion t = D.union [y, x]
        | otherwise = D.union [x, y]

toroidalPath ::
    -- | Inner radius
    Double ->
    -- | Outer radius
    Double ->
    -- | Winding number (vertical)
    Double ->
    -- | Winding number (longitudinal)
    Double ->
    Path
toroidalPath r1 r2 n m = Animated $ \(Time t) -> toroidalProjection r1 r2 (a1 t, a2 t)
  where
    a1 = (2 * pi * m *)
    a2 = (2 * pi * n *)

toroidalProjection :: Double -> Double -> (Double, Double) -> Point
toroidalProjection r1 r2 (a1, a2) = Point x y
  where
    x = r * cos a1
    y = r * sin a1
    r = r1 + (r2 - r1) * u
    u = (1 + cos a2) / 2