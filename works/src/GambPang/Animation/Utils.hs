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
    grating,
    translationField,
    scaleField,
    unitField,
    unitBump,
    Pointed (..),
    midpoint,
    rotatingP,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as Text
import GambPang.Animation (
    Animated,
    Drawing,
    Motion,
    Point (..),
    Rigged (..),
    Vector (..),
    ViewFrame (..),
    negateV,
    norm,
    normalize,
    point,
    pointToVector,
    rotate,
    rotateO,
    scale,
    time,
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

union2 :: Drawing color -> Drawing color -> Drawing color
union2 x y = D.union [x, y]

rotating ::
    Rigged a =>
    -- | Rate of rotation
    Double ->
    Motion a
rotating r = rot <$> time
  where
    rot t = rotateO (2 * r * t * pi)

rotatingP :: Rigged a => Point -> Double -> Motion a
rotatingP p r = rot <$> time
  where
    rot t = rotate p (2 * r * t * pi)

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

grating ::
    Point ->
    Point ->
    Int ->
    Int ->
    Shape ->
    Drawing color ->
    Drawing color
grating ll ur n m s a = D.union $ makeGrid ll ur n m applyMask
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

midpoint :: Point -> Point -> Point
midpoint (Point x1 y1) (Point x2 y2) = Point mx my
  where
    mx = (x1 + x2) / 2
    my = (y1 + y2) / 2
