{-# LANGUAGE OverloadedStrings #-}

module GambPang.Animation.Utils (
    rotating,
    translations,
    union2,
    dataUrl,
    defaultViewFrame,
    originViewFrame,
    defaultAnimatedPiece,
    makeGrid,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as Text
import GambPang.Animation (
    Animated,
    Drawing,
    Point (..),
    Rigged (..),
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

originViewFrame :: ViewFrame
originViewFrame = defaultViewFrame{lowerLeft = (-250, -250)}

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

makeGrid ::
    Rigged b =>
    Point ->
    Point ->
    Int ->
    Int ->
    (Int -> Int -> Point -> b) ->
    [b]
makeGrid ll ur n m mkObject
    | n >= 2 && m >= 2 = objectGrid
    | otherwise = error "makeGrid"
  where
    objectGrid = [mkObject i j (mkLocation ll ur n m i j) | i <- [1 .. n], j <- [1 .. m]]

mkLocation :: Point -> Point -> Int -> Int -> Int -> Int -> Point
mkLocation ll ur n m i j =
    Point
        { pointX = llx + (fromIntegral i - 1) * hSep
        , pointY = lly + (fromIntegral j - 1) * vSep
        }
  where
    Point llx lly = ll
    Point urx ury = ur

    hSep = (urx - llx) / (fromIntegral n - 1)
    vSep = (ury - lly) / (fromIntegral m - 1)
