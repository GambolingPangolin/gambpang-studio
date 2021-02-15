{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module GambPang.Knots.Pattern (
    Pattern (..),
    defaultBlockedEdges,
    addComputedBlockedEdges,
    getBaseArea,
    Edge (..),
    getEdge,
    edgeLocation,
    EdgePosition (..),
    blocked,
    renderPattern,
    newPattern,
    PatternRenderError (..),
) where

import Codec.Picture (
    Image,
    PixelRGBA8 (..),
    generateImage,
    imageHeight,
    imageWidth,
    pixelAt,
    readPixel,
    writePixel,
 )
import Codec.Picture.Types (MutableImage, freezeImage, thawImage)
import Control.Monad.ST (ST, runST)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word8)
import GambPang.Knots.Tiles (Tile (Tile), TileConfig, TileType (..), tile)

data Pattern = Pattern
    { width :: Int
    , height :: Int
    , blockedTiles :: Set (Int, Int)
    , blockedEdges :: Set Edge
    }
    deriving (Eq, Show)

newPattern :: Int -> Int -> Pattern
newPattern w h =
    Pattern
        { width = w
        , height = h
        , blockedTiles = mempty
        , blockedEdges = defaultBlockedEdges mempty w h
        }

addComputedBlockedEdges :: Pattern -> Pattern
addComputedBlockedEdges p =
    p{blockedEdges = p.blockedEdges <> defaultBlockedEdges p.blockedTiles p.width p.height}

defaultBlockedEdges :: Set (Int, Int) -> Int -> Int -> Set Edge
defaultBlockedEdges mask w h =
    Set.fromList $
        mconcat
            [getEdge (i - 1) (j - 1) <$> excludedEdges i j | (i, j) <- getBaseArea w h]
  where
    excludedEdges i j =
        bool id (TopEdge :) (hasTopEdge i j)
            . bool id (LeftEdge :) (hasLeftEdge i j)
            . bool id (BottomEdge :) (hasBottomEdge i j)
            . bool id (RightEdge :) (hasRightEdge i j)
            $ mempty

    hasTopEdge i j = inRegion i j && not (inRegion i (j + 1))
    hasLeftEdge i j = inRegion i j && not (inRegion (i - 1) j)
    hasBottomEdge i j = inRegion i j && not (inRegion i (j - 1))
    hasRightEdge i j = inRegion i j && not (inRegion (i + 1) j)

    inRegion i j = inBaseRegion w h i j && not (isMasked i j)
    isMasked i j = (i, j) `Set.member` mask

inBaseRegion :: Int -> Int -> Int -> Int -> Bool
inBaseRegion w h i j = i + j <= 2 * mn + mx + 1 && i + j >= mx + 1 && abs (i - j) <= mx
  where
    mn = min w h
    mx = max w h

-- | An edge is represented by its midpoint, where the center of pixel @(i,j)@ is @(i, j)@
newtype Edge = Edge (Int, Int)
    deriving (Eq, Ord, Show)

edgeLocation :: Edge -> (Int, Int)
edgeLocation (Edge x) = x

data EdgePosition = TopEdge | LeftEdge | BottomEdge | RightEdge
    deriving (Eq, Enum, Ord, Show)

getEdge ::
    -- | Horizontal pixel position
    Int ->
    -- | Vertical pixel position
    Int ->
    EdgePosition ->
    Edge
getEdge i j = \case
    TopEdge -> Edge (2 * i, 2 * j + 1)
    LeftEdge -> Edge (2 * i - 1, 2 * j)
    BottomEdge -> Edge (2 * i, 2 * j - 1)
    RightEdge -> Edge (2 * i + 1, 2 * j)

blocked :: Pattern -> Int -> Int -> EdgePosition -> Bool
blocked patt i j pos = getEdge i j pos `Set.member` patt.blockedEdges

blockedSet :: Pattern -> Int -> Int -> [EdgePosition]
blockedSet patt i j = filter isBlocked [TopEdge, LeftEdge, BottomEdge, RightEdge]
  where
    isBlocked = blocked patt i j

renderPattern ::
    TileConfig ->
    Pattern ->
    Maybe (Image PixelRGBA8) ->
    Either PatternRenderError (Image PixelRGBA8)
renderPattern tileConf patt mBgImage = buildImage <$> getTiles patt baseArea
  where
    baseArea = adjust <$> getBaseArea patt.width patt.height
    adjust (i, j) = (i - 1, j - 1)

    bgImage = fromMaybe (defaultBgImage tileConf patt) mBgImage

    buildImage ts = runST $ do
        img <- thawImage bgImage
        mapM_ (doOverlay img) ts
        freezeImage img

    doOverlay :: MutableImage s PixelRGBA8 -> (Int, Int, Tile) -> ST s ()
    doOverlay img (i, j, t) = overlay (i * w) (j * w) (tile tileConf t) img
    w = tileConf.width

getBaseArea :: Int -> Int -> [(Int, Int)]
getBaseArea w h
    | w < h = getBaseArea h w
    | otherwise = mconcat region
  where
    region = row <$> [1 .. w + h]
    row j
        | j <= h = (,j) <$> [w + 1 - j .. w + j]
        | j > h && j <= w = (,j) <$> [w + 1 - j .. w + 1 - j + 2 * h]
        | otherwise = (,j) <$> [j - w .. 2 * h + 1 - (j - w)]

getTiles :: Pattern -> [(Int, Int)] -> Either PatternRenderError [(Int, Int, Tile)]
getTiles patt = traverse getTile . filter (not . isBlocked)
  where
    getTile (i, j) = (i,j,) <$> tileForLocation patt i j
    isBlocked = (`Set.member` patt.blockedTiles)

defaultBgImage :: TileConfig -> Pattern -> Image PixelRGBA8
defaultBgImage conf patt = generateImage mkPixel w w
  where
    mkPixel _ _ = conf.background
    w = conf.width * (patt.width + patt.height)

tileForLocation :: Pattern -> Int -> Int -> Either PatternRenderError Tile
tileForLocation patt i j
    | null theBlockedEdges = pure $ Tile Crossing (i + j `mod` 2)
    | theBlockedEdges == [TopEdge, BottomEdge] = pure $ Tile Straight 0
    | theBlockedEdges == [LeftEdge, RightEdge] = pure $ Tile Straight 1
    | theBlockedEdges == [TopEdge, LeftEdge] = pure $ Tile Elbow 2
    | theBlockedEdges == [LeftEdge, BottomEdge] = pure $ Tile Elbow 1
    | theBlockedEdges == [BottomEdge, RightEdge] = pure $ Tile Elbow 0
    | theBlockedEdges == [TopEdge, RightEdge] = pure $ Tile Elbow 3
    | otherwise = Left $ InvalidBlockSet i j
  where
    theBlockedEdges = blockedSet patt i j

data PatternRenderError = InvalidBlockSet Int Int
    deriving (Eq, Show)

overlay :: Int -> Int -> Image PixelRGBA8 -> MutableImage s PixelRGBA8 -> ST s ()
overlay i0 j0 imgOver imgUnder = mapM_ (uncurry updatePixel) overlayPixels
  where
    overlayPixels = [(i - 1, j - 1) | i <- [1 .. imageWidth imgOver], j <- [1 .. imageHeight imgOver]]
    updatePixel i j = do
        originalPixel <- readPixel imgUnder (i0 + i) (j0 + j)
        writePixel imgUnder (i0 + i) (j0 + j) $
            pixelAt imgOver i j `pixelOver` originalPixel

pixelOver :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
pixelOver (PixelRGBA8 rA gA bA aA) (PixelRGBA8 rB gB bB aB) = PixelRGBA8 r g b aB
  where
    r = convex aA rA rB
    g = convex aA gA gB
    b = convex aA bA bB

convex :: Word8 -> Word8 -> Word8 -> Word8
convex a x y =
    floor @Double . (/ 0xff) $
        fromIntegral x * fromIntegral a + fromIntegral y * fromIntegral (0xff - a)
