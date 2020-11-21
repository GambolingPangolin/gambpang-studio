module GambPang.Animation.Utils (
    rotating,
    translations,
    defaultViewFrame,
    union2,
    defaultRender,
    renderWithFrames,
) where

import Codec.Picture (Image, PixelRGBA8)
import Data.Colour (Colour)
import GambPang.Animation (
    Animated,
    Drawing,
    Rigged,
    Time (Time),
    Vector,
    ViewFrame (..),
    renderAnimDrawing,
    rotateO,
    time,
    translate,
 )
import qualified GambPang.Animation.Drawing as D

import GambPang.Animation.ColorStyle (
    ColorStyle (Background),
    PaletteChoice (..),
 )

defaultRender ::
    (ColorStyle -> Colour Double) ->
    PaletteChoice ->
    Animated (Drawing ColorStyle) ->
    [Image PixelRGBA8]
defaultRender defaultPalette paletteChoice = renderWithFrames defaultPalette paletteChoice 100

renderWithFrames ::
    (ColorStyle -> Colour Double) ->
    PaletteChoice ->
    Int ->
    Animated (Drawing ColorStyle) ->
    [Image PixelRGBA8]
renderWithFrames defaultPalette paletteChoice n = renderAnimDrawing n defaultViewFrame bg style
  where
    bg = style Background
    style = case paletteChoice of
        DefaultPalette -> defaultPalette
        PaletteChoice somePalette -> somePalette

defaultViewFrame :: ViewFrame
defaultViewFrame =
    ViewFrame
        { lowerLeft = (0, 0)
        , viewFrameWidth = 500
        , viewFrameHeight = 500
        , endTime = Time 1
        }

union2 :: Drawing color -> Drawing color -> Drawing color
union2 x y = D.union [x, y]

rotating ::
    Rigged a =>
    -- | Rate of rotation
    Double ->
    Animated (a -> a)
rotating r = rot <$> time
  where
    rot (Time t) = rotateO (2 * r * t * pi)

translations :: (Functor f, Rigged b) => b -> f Vector -> f b
translations s = fmap (`translate` s)
