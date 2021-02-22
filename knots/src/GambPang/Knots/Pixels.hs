{-# LANGUAGE TypeApplications #-}

module GambPang.Knots.Pixels (
    readPixel,
    rotate,
) where

import Codec.Picture (
    Image,
    PixelRGBA8 (PixelRGBA8),
    generateImage,
    imageHeight,
    imageWidth,
    pixelAt,
 )
import Data.Colour (
    AlphaColour,
    affineCombo,
    alphaChannel,
    black,
    over,
    transparent,
    withOpacity,
 )
import Data.Colour.RGBSpace (RGB (..))
import Data.Colour.SRGB (sRGB24, sRGB24read, toSRGB24)

-- | Decode a hex string to a pixel
readPixel :: String -> PixelRGBA8
readPixel =
    (PixelRGBA8 <$> channelRed <*> channelGreen <*> channelBlue <*> pure 0xFF)
        . toSRGB24 @Double
        . sRGB24read

-- | Rotate and crop a knot drawing
rotate ::
    -- | Angle
    Double ->
    -- | Default pixel (out of range)
    PixelRGBA8 ->
    -- | Source image
    Image PixelRGBA8 ->
    Image PixelRGBA8
rotate a defPixel src = generateImage getPx w h
  where
    w = imageWidth src
    hw = w `quot` 2

    h = imageHeight src
    hh = h `quot` 2

    getPx = curry $ calcPixel defPixel src . calcPixelWeights . recenter (applyRotation a)
    recenter f (i, j) =
        let (x, y) = f (i - hw, j - hh)
         in (x + fromIntegral hw, y + fromIntegral hh)

applyRotation :: Double -> (Int, Int) -> (Double, Double)
applyRotation a (i, j) =
    ( cos a * fromIntegral @_ @Double i - sin a * fromIntegral j
    , sin a * fromIntegral @_ @Double i + cos a * fromIntegral j
    )

-- Calculate the position as a weighted sum over integral positions
calcPixelWeights :: (Double, Double) -> [(Int, Int, Double)]
calcPixelWeights (x, y) = [upperLeft, upperRight, lowerLeft, lowerRight]
  where
    (xi, xf) = properFraction x
    (yi, yf) = properFraction y

    xH = 0.5 + xf
    xL = 0.5 - xf
    yH = 0.5 + yf
    yL = 0.5 - yf

    upperLeft
        | xf <= 0.5 && yf <= 0.5 = (xi - 1, yi, xL * yH)
        | xf > 0.5 && yf <= 0.5 = (xi, yi, xH * yH)
        | xf <= 0.5 && yf > 0.5 = (xi - 1, yi + 1, xL * yL)
        | otherwise = (xi, yi + 1, xL * yH)

    upperRight
        | xf <= 0.5 && yf <= 0.5 = (xi, yi, xH * yH)
        | xf > 0.5 && yf <= 0.5 = (xi + 1, yi, xL * yH)
        | xf <= 0.5 && yf > 0.5 = (xi, yi + 1, xH * yL)
        | otherwise = (xi + 1, yi + 1, xL * yL)

    lowerLeft
        | xf <= 0.5 && yf <= 0.5 = (xi - 1, yi - 1, xL * yL)
        | xf > 0.5 && yf <= 0.5 = (xi, yi - 1, xH * yL)
        | xf <= 0.5 && yf > 0.5 = (xi - 1, yi, xL * yH)
        | otherwise = (xi, yi, xH * yH)

    lowerRight
        | xf <= 0.5 && yf <= 0.5 = (xi, yi - 1, xH * yL)
        | xf > 0.5 && yf <= 0.5 = (xi + 1, yi - 1, xL * yL)
        | xf <= 0.5 && yf > 0.5 = (xi, yi, xH * yH)
        | otherwise = (xi + 1, yi, xL * yH)

calcPixel ::
    PixelRGBA8 ->
    Image PixelRGBA8 ->
    [(Int, Int, Double)] ->
    PixelRGBA8
calcPixel defPixel src ws = toPixel $ affineCombo weightedPixel transparent
  where
    weightedPixel = toColor <$> ws
    weight = sum $ getWeight <$> ws
    getWeight (_, _, w) = w
    toColor (i, j, w) = (w / weight, pixelToColour $ fetchPixel defPixel src i j)

fetchPixel :: PixelRGBA8 -> Image PixelRGBA8 -> Int -> Int -> PixelRGBA8
fetchPixel defPixel img i j
    | i >= 0 && i < imageWidth img && j >= 0 && j < imageHeight img = pixelAt img i j
    | otherwise = defPixel

pixelToColour :: PixelRGBA8 -> AlphaColour Double
pixelToColour (PixelRGBA8 r g b a) = withOpacity c $ fromIntegral a / 0xFF
  where
    c = sRGB24 r g b

toPixel :: AlphaColour Double -> PixelRGBA8
toPixel c = PixelRGBA8 r g b a
  where
    a = floor $ 0xFF * alphaChannel c
    RGB r g b = toSRGB24 $ c `over` black
