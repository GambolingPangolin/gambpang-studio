module GambPang.Animation.Bars (
    animations,
    bars1,
    bars2,
) where

import Codec.Picture (Image, PixelRGBA8)
import Data.Map (Map)
import qualified Data.Map as Map
import GambPang.Animation (
    Animated (..),
    Drawing,
    Point (..),
    Time (..),
    Vector (Vector),
    circularPath,
    followPath,
    negateV,
    origin,
    rotateO,
    scale,
    shift,
    time,
    translate,
    valueAtTime,
 )
import qualified GambPang.Animation.Drawing as D

import GambPang.Animation.ColorStyle (ColorStyle (Foreground), PaletteChoice, mellow)
import GambPang.Animation.Utils (defaultRender)

animations :: PaletteChoice -> Map String [Image PixelRGBA8]
animations paletteChoice =
    defaultRender mellow paletteChoice
        <$> Map.fromList
            [ ("bars-1", bars1)
            , ("bars-2", bars2)
            , ("bars-3", bars3)
            ]

bars1 :: Animated (Drawing ColorStyle)
bars1 = translate v . D.mask sqMask . scale 400 400 . D.union <$> layers
  where
    sqMask = D.rectangle 400 400
    v = Vector 50 50
    layers = fmap <$> (mkCutLayer <$> time) <*> pure [0 .. 19]

mkCutLayer :: Time -> Int -> Drawing ColorStyle
mkCutLayer (Time t) n =
    translate v . D.union $
        [ section
        , translate v1 section
        , translate v2 section
        ]
  where
    ph = fromIntegral n / fromIntegral stripeCount
    v = Vector (t + ph - 2) ph
    v1 = Vector 1 0
    v2 = Vector 2 0

    section = D.draw Foreground $ D.rectangle w stripeHeight
    w = 1 - cutSize

cutSize :: Double
cutSize = 0.1

stripeHeight :: Double
stripeHeight = 0.75 * (1 / fromIntegral stripeCount)

stripeCount :: Int
stripeCount = 20

bars2 :: Animated (Drawing ColorStyle)
bars2 =
    D.union
        <$> sequenceA
            [ rotatingMask
            , shift (Time 0.33) rotatingMask
            , shift (Time 0.66) rotatingMask
            ]
  where
    rotatingMask = D.mask <$> (circleMask <$> time) <*> pure (stripes 20 500)
    circleMask t = D.disc (valueAtTime t path) 50
    path = circularPath (Time 1) (Point 250 250) 100

bars3 :: Animated (Drawing ColorStyle)
bars3 =
    D.union
        <$> sequenceA
            [ element
            , shift (Time 0.33) element
            , shift (Time 0.66) element
            , pure bigBackground
            ]
  where
    element = followPath path origin <*> rotatingMaskedStripes
    rotatingMaskedStripes = motion <*> pure (D.mask circleMask background)

    motion = Animated $ rotateO . mkAngle
    mkAngle (Time t) = 4 * pi * t
    circleMask = D.disc origin 50
    background = translate v $ stripes 10 h

    path = circularPath (Time 1) (Point 250 250) 100
    v = negateV $ Vector (h / 2) (h / 2)
    h = 100

    bigBackground = D.exclude (D.disc windowCenter windowR) $ stripes 60 500
    windowCenter = Point 250 250
    windowR = 200

stripes :: Int -> Double -> Drawing ColorStyle
stripes numStripes totalSize = D.union $ mkStripe <$> [0 .. n - 1]
  where
    mkStripe i = positionStripe i . D.draw Foreground $ D.rectangle totalSize h
    positionStripe i = translate $ Vector 0 (2 * h * i)
    h = totalSize / (2 * n)
    n = fromIntegral numStripes
