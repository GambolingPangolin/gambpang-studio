{-# LANGUAGE OverloadedStrings #-}

module GambPang.Animation.Bars (
    animations,
    bars1,
    bars2,
    bars3,
    bars4,
    bars5,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import GambPang.Animation (
    Animated,
    Drawing,
    Point (..),
    Time,
    Vector (Vector),
    backwards,
    circularPath,
    followPath,
    fromWidthHeight,
    makeCircular,
    negateV,
    origin,
    reflect,
    rescale,
    rotatingO,
    scaleXY,
    shiftEarlier,
    time,
    translate,
 )
import qualified GambPang.Animation.Drawing as D

import GambPang.Animation.ColorStyle (ColorStyle (..), PaletteChoice, snowy, vegetablegarden)
import GambPang.Animation.Piece (AnimatedPiece (palette, viewFrame), applyPaletteChoice)
import GambPang.Animation.Utils (defaultAnimatedPiece, originViewFrame)

animations :: PaletteChoice -> Map Text AnimatedPiece
animations paletteChoice =
    applyPaletteChoice paletteChoice
        <$> Map.fromList
            [ ("bars-1", bars1)
            , ("bars-2", bars2)
            , ("bars-3", bars3)
            , ("bars-4", bars4)
            , ("bars-5", bars5)
            ]

bars1 :: AnimatedPiece
bars1 = defaultAnimatedPiece . rescale r $ D.mask sqMask . D.union <$> layers
  where
    sqMask = D.rectangle 1 1
    layers = fmap <$> (mkCutLayer <$> time) <*> pure [0 .. 19]
    r = fromWidthHeight (Point (-0.1) (-0.1)) 1.2 1.2

mkCutLayer :: Time -> Int -> Drawing ColorStyle
mkCutLayer t n =
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

bars2 :: AnimatedPiece
bars2 =
    defaultAnimatedPiece $ D.composite components
  where
    components =
        [ rotatingMask
        , shiftEarlier 0.33 rotatingMask
        , shiftEarlier 0.66 rotatingMask
        ]
    rotatingMask = D.mask <$> (circleMask <$> path) <*> pure (stripes 20 500)
    circleMask p = D.disc p 50
    path = circularPath 1 (Point 250 250) 100

bars3 :: AnimatedPiece
bars3 = defaultAnimatedPiece $ D.composite components
  where
    components =
        [ element
        , shiftEarlier 0.33 element
        , shiftEarlier 0.66 element
        , pure bigBackground
        ]
    element = followPath path origin <*> rotatingMaskedStripes
    rotatingMaskedStripes = rotatingO 2 <*> pure (D.mask circleMask background)

    circleMask = D.disc origin 50
    background = translate v $ stripes 10 h

    path = circularPath 1 (Point 250 250) 100
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

-- | In which we exit a canyon
bars4 :: AnimatedPiece
bars4 = piece{palette = vegetablegarden, viewFrame = originViewFrame}
  where
    piece = defaultAnimatedPiece canyonExit

canyonExit :: Animated (Drawing ColorStyle)
canyonExit = makeCircular 1 $ D.composite [leftSide, rightSide]
  where
    leftSide = animateScale <*> (pure . scaleXY (16 * 500) 500 . D.union) boxes
    rightSide = reflect (Vector 1 0) origin leftSide

    section l w c = translate (Vector (l - 2) (-0.5)) . D.draw c $ D.rectangle w 1
    boxes = take 12 $ zipWith3 section offsets widths colors

    widths = (2 ^^) . negate <$> [0 :: Int ..]
    offsets = 0 : zipWith (+) widths offsets
    colors = cycle [Background, Foreground, HighlightA, HighlightB]

    animateScale = mkScale <$> time
    mkScale t = scaleXY (0.5 ** 4 * t) 1

-- | In which we go into a canyon
bars5 :: AnimatedPiece
bars5 = piece{palette = snowy, viewFrame = originViewFrame}
  where
    piece = defaultAnimatedPiece $ backwards canyonExit
