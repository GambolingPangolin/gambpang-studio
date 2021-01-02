{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module GambPang.Animation.Scrollers (
    scroller90,
    scroller30,
    animations,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import GambPang.Animation (
    Animated,
    Field2D,
    Point (Point),
    Vector (..),
    point,
    rotateO,
    scale,
    time,
    translate,
 )

import GambPang.Animation.ColorStyle (
    ColorStyle (..),
    PaletteChoice,
    mellow,
 )
import GambPang.Animation.Piece (
    AnimatedPiece (..),
    AnimationSource (AnimatedField2D),
    applyPaletteChoice,
 )
import GambPang.Animation.Utils (defaultViewFrame)

animations :: PaletteChoice -> Map Text AnimatedPiece
animations pc =
    applyPaletteChoice pc
        <$> Map.fromList
            [ ("scroller-90", scroller90)
            , ("scroller-30", scroller30)
            ]

scroller90 :: AnimatedPiece
scroller90 = scroller (pi / 2) 20

scroller30 :: AnimatedPiece
scroller30 = scroller (pi / 6) 10

scroller :: Double -> Double -> AnimatedPiece
scroller a w =
    AnimatedPiece
        { source = AnimatedField2D $ scrollerAnim w a field
        , viewFrame = defaultViewFrame
        , frameCount = 50
        , framesPerSec = 33
        , palette = mellow
        }
  where
    field = infiniteField Background Foreground

scrollerAnim :: Double -> Double -> Field2D a -> Animated (Field2D a)
scrollerAnim zoom a s = mkAnim <$> time
  where
    mkAnim t = translate (vec t) $ scale zoom rotatedScene
    vec t = let z = 2 * zoom * t in scale z v
    v = rotateO a $ Vector 1 0
    rotatedScene = rotateO a s

infiniteField :: a -> a -> Field2D a
infiniteField lightPixel darkPixel = field <$> point
  where
    field (Point x _)
        | even @Int (floor x) = darkPixel
        | otherwise = lightPixel
