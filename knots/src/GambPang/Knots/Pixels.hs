{-# LANGUAGE TypeApplications #-}

module GambPang.Knots.Pixels (
    readPixel,
) where

import Codec.Picture (PixelRGBA8 (PixelRGBA8))
import Data.Colour.RGBSpace (RGB (..))
import Data.Colour.SRGB (sRGB24read, toSRGB24)

readPixel :: String -> PixelRGBA8
readPixel =
    (PixelRGBA8 <$> channelRed <*> channelGreen <*> channelBlue <*> pure 0xFF)
        . toSRGB24 @Double
        . sRGB24read
