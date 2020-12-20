module GambPang.Animation.Piece (
    AnimationSource (..),
    AnimatedPiece (..),
    applyPaletteChoice,
    renderImageSequence,
    renderGif,
) where

import Codec.Picture (GifLooping (LoopingForever), Image, encodeGifAnimation, pixelMap)
import Codec.Picture.Types (PixelRGBA8, TransparentPixel (dropTransparency))
import Data.ByteString.Lazy (ByteString)
import Data.Colour (Colour)
import GambPang.Animation (
    Animated,
    Drawing,
    Field2D,
    ViewFrame,
    colorPixel,
    renderAnimDrawing,
    renderAnimField2D,
 )

import GambPang.Animation.ColorStyle (ColorStyle (Background), PaletteChoice (..))

data AnimationSource
    = AnimatedField2D (Animated (Field2D ColorStyle))
    | AnimatedDrawing (Animated (Drawing ColorStyle))

data AnimatedPiece = AnimatedPiece
    { source :: AnimationSource
    , viewFrame :: ViewFrame
    , frameCount :: Int
    , framesPerSec :: Double
    , palette :: ColorStyle -> Colour Double
    }

applyPaletteChoice :: PaletteChoice -> AnimatedPiece -> AnimatedPiece
applyPaletteChoice pc piece = case pc of
    DefaultPalette -> piece
    PaletteChoice p -> piece{palette = p}

renderImageSequence :: AnimatedPiece -> [Image PixelRGBA8]
renderImageSequence piece = case source piece of
    AnimatedField2D x ->
        (renderAnimField2D <$> frameCount <*> viewFrame) piece $ fmap toColor <$> x
    AnimatedDrawing x ->
        (renderAnimDrawing <$> frameCount <*> viewFrame <*> pure bg <*> palette) piece x
  where
    bg = palette piece Background
    toColor = (`colorPixel` 0xff) . palette piece

renderGif :: AnimatedPiece -> Either String ByteString
renderGif piece =
    encodeGifAnimation msDelay LoopingForever
        . fmap (pixelMap dropTransparency)
        $ renderImageSequence piece
  where
    msDelay = floor $ 1000 / framesPerSec piece
