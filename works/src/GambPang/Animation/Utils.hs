{-# LANGUAGE OverloadedStrings #-}

module GambPang.Animation.Utils (
    dataUrl,
    defaultViewFrame,
    originViewFrame,
    defaultAnimatedPiece,
    translations,
    grating,
    translationField,
    scaleField,
    unitField,
    unitBump,
    Pointed (..),
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as Text
import GambPang.Animation (
    Animated,
    Drawing,
    Grid (..),
    Motion,
    Point (..),
    Rectangle,
    Rigged (..),
    Vector (..),
    ViewFrame (..),
    makeGrid,
    negateV,
    norm,
    normalize,
    point,
    pointToVector,
    scale,
    translate,
    valueAtPoint,
 )
import qualified GambPang.Animation.Drawing as D

import GambPang.Animation.ColorStyle (
    ColorStyle,
    mellow,
 )
import GambPang.Animation.Drawing (Shape)
import GambPang.Animation.Field2D (Field2D)
import GambPang.Animation.Piece (AnimatedPiece (..), AnimationSource (AnimatedDrawing))

defaultViewFrame :: ViewFrame
defaultViewFrame =
    ViewFrame
        { lowerLeft = (0, 0)
        , viewFrameWidth = 500
        , viewFrameHeight = 500
        , endTime = 1
        }

originViewFrame :: ViewFrame
originViewFrame = defaultViewFrame{lowerLeft = (-250, -250)}

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

grating ::
    Rectangle ->
    Int ->
    Int ->
    Shape ->
    Drawing color ->
    Drawing color
grating r n m s a = D.union . makeGrid $ Grid r n m applyMask
  where
    applyMask _ _ p = D.mask (makeMask p) a
    makeMask p = translate (pointToVector p) s

translationField ::
    Rigged a =>
    Animated (Field2D Vector) ->
    Point ->
    Motion a
translationField a p = mkTranslation <$> a
  where
    mkTranslation = translate . valueAtPoint p

scaleField ::
    Rigged a =>
    Animated (Field2D Double) ->
    Point ->
    Motion a
scaleField a p = mkScale <$> a
  where
    mkScale f =
        let v = pointToVector p
         in translate v
                . scale (valueAtPoint p f)
                . translate (negateV v)

unitField :: Field2D Vector
unitField = mkUnit <$> point
  where
    mkUnit (Point 0 0) = Vector 0 0
    mkUnit p = normalize (pointToVector p)

unitBump :: Field2D Double
unitBump = gaussian . norm . pointToVector <$> point
  where
    gaussian x = exp . negate $ x ^ (2 :: Int)

data Pointed a = Pointed
    { basePoint :: Point
    , object :: a
    }
    deriving (Eq, Show)

instance Functor Pointed where
    fmap f (Pointed bp o) = Pointed bp $ f o

instance Rigged a => Rigged (Pointed a) where
    transform t (Pointed bp o) =
        Pointed
            { basePoint = transform t bp
            , object = transform t o
            }
