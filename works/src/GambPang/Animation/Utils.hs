{-# LANGUAGE OverloadedStrings #-}

module GambPang.Animation.Utils (
    rotating,
    translations,
    union2,
    dataUrl,
    defaultViewFrame,
    defaultAnimatedPiece,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as Text
import GambPang.Animation (
    Animated,
    Drawing,
    Rigged,
    Time (Time),
    Vector,
    ViewFrame (..),
    rotateO,
    time,
    translate,
 )
import qualified GambPang.Animation.Drawing as D

import GambPang.Animation.ColorStyle (
    ColorStyle,
    mellow,
 )
import GambPang.Animation.Piece (AnimatedPiece (..), AnimationSource (AnimatedDrawing))

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

dataUrl :: Text -> ByteString -> Text
dataUrl mime content = "data:" <> mime <> ";base64," <> b64 content
  where
    b64 = Text.pack . BS8.unpack . B64.encode

defaultAnimatedPiece :: Animated (Drawing ColorStyle) -> AnimatedPiece
defaultAnimatedPiece anim =
    AnimatedPiece
        { source = AnimatedDrawing anim
        , viewFrame = defaultViewFrame
        , frameCount = 100
        , framesPerSec = 50
        , palette = mellow
        }
