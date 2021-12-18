module GambPang.Knots.Image (
    imageToPattern,
) where

import Codec.Picture (Image, Pixel8, imageHeight, imageWidth)
import Codec.Picture.Types (pixelFold)
import qualified Data.Set as Set
import GambPang.Knots.Pattern (
    EdgePosition (..),
    Pattern (..),
    getEdge,
    newPattern,
 )

-- | Convert an image to a pattern by interpreting bright pixels as blocked tiles
imageToPattern ::
    -- | Threshold pixel
    Pixel8 ->
    -- | Starting image
    Image Pixel8 ->
    Pattern
imageToPattern threshold img =
    basePattern{blockedEdges = blockedEdges basePattern <> imgMaskedEdges}
  where
    basePattern = newPattern w h
    w = imageWidth img
    h = imageHeight img

    imgMaskedEdges = pixelFold (argCycle analyzePixel) mempty img
    analyzePixel x y thePixel
        | thePixel >= threshold =
            ( <>
                Set.fromList
                    [ getEdge (1 + x + y) (w + y - x) TopEdge
                    , getEdge (1 + x + y) (w + y - x) RightEdge
                    , getEdge (2 + x + y) (w + y - x + 1) BottomEdge
                    , getEdge (2 + x + y) (w + y - x + 1) LeftEdge
                    ]
            )
        | otherwise = id
    argCycle f x1 x2 x3 x4 = f x2 x3 x4 x1
