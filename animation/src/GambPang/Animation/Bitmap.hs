module GambPang.Animation.Bitmap (
    ViewFrame (..),
    pixelPoint,
    unitPixel,
    colorPixel,
    BoundingBox (..),
    boundingBoxPixels,
) where

import Codec.Picture (PixelRGBA8 (..))
import Data.Colour (Colour)
import Data.Colour.RGBSpace (RGB (..))
import Data.Colour.SRGB (toSRGB24)
import Data.Word (Word8)

import GambPang.Animation.LinearAlgebra (Point (Point))
import GambPang.Animation.Scene (Time)

data ViewFrame = ViewFrame
    { lowerLeft :: (Int, Int)
    , viewFrameWidth :: Int
    , viewFrameHeight :: Int
    , endTime :: Time
    }
    deriving (Eq, Show)

-- | Transparent black pixel
unitPixel :: PixelRGBA8
unitPixel = PixelRGBA8 0 0 0 0

colorPixel :: Colour Double -> Word8 -> PixelRGBA8
colorPixel = (PixelRGBA8 <$> channelRed <*> channelGreen <*> channelBlue) . toSRGB24

data BoundingBox = BoundingBox
    { boxLowerLeft :: (Int, Int)
    , boxUpperRight :: (Int, Int)
    }
    deriving (Eq, Show)

pixelPoint :: Int -> Int -> Point
pixelPoint x y = Point (fromIntegral x + 0.5) (fromIntegral y + 0.5)

boundingBoxPixels :: BoundingBox -> [(Int, Int)]
boundingBoxPixels BoundingBox{boxLowerLeft = (xMin, yMin), boxUpperRight = (xMax, yMax)} =
    [(x, y) | x <- [xMin .. xMax], y <- [yMin .. yMax]]
