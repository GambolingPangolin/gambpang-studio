{-# LANGUAGE TypeApplications #-}

module GambPang.Animation.Scrollers (
    scroller90,
    scroller30,
    animations,
) where

import Codec.Picture (Image, PixelRGBA8)
import Data.Colour.Names (beige, chocolate)
import Data.Map (Map)
import qualified Data.Map as Map

import GambPang.Animation (
    Animated (..),
    Field2D (Field2D),
    Point (Point),
    Time (..),
    Vector (..),
    colorPixel,
    renderAnimField2D,
    rotateO,
    scale,
    translate,
 )
import GambPang.Animation.ColorStyle (PaletteChoice)
import GambPang.Animation.Utils (defaultViewFrame)

animations :: PaletteChoice -> Map String [Image PixelRGBA8]
animations _ =
    Map.fromList
        [ ("scroller-90", scroller90)
        , ("scroller-30", scroller30)
        ]

scroller90 :: [Image PixelRGBA8]
scroller90 = scroller (pi / 2) 20

scroller30 :: [Image PixelRGBA8]
scroller30 = scroller (pi / 6) 10

scroller :: Double -> Double -> [Image PixelRGBA8]
scroller a w = renderAnimField2D 50 defaultViewFrame $ scrollerAnim w a field
  where
    field = infiniteField (colorPixel beige 0xff) (colorPixel chocolate 0xff)

scrollerAnim :: Double -> Double -> Field2D PixelRGBA8 -> Animated (Field2D PixelRGBA8)
scrollerAnim zoom a s = Animated $ \(Time t) ->
    translate (vec t) $ scale zoom zoom rotatedScene
  where
    vec t = let z = 2 * zoom * t in scale z z v
    v = rotateO a $ Vector 1 0
    rotatedScene = rotateO a s

infiniteField :: a -> a -> Field2D a
infiniteField lightPixel darkPixel = Field2D field
  where
    field (Point x _)
        | even @Int (floor x) = darkPixel
        | otherwise = lightPixel
