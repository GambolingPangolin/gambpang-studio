{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module GambPang.Knots.Pattern (
    Pattern (..),
    defaultBlockedEdges,
    Edge,
    getEdge,
    EdgePosition (..),
    blocked,
    renderPattern,
    newPattern,
    PatternRenderError (..),
) where

import Codec.Picture (
    Image,
    Pixel,
    PixelRGBA8,
    generateImage,
    imageHeight,
    imageWidth,
    pixelAt,
    writePixel,
 )
import Codec.Picture.Types (MutableImage, freezeImage, thawImage)
import Control.Monad.ST (ST, runST)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import GambPang.Knots.Tiles (Tile (Tile), TileConfig, TileType (..), tile)

data Pattern = Pattern
    { diagonal :: Int
    , blockedTiles :: Set (Int, Int)
    , blockedEdges :: Set Edge
    }
    deriving (Eq, Show)

newPattern :: Int -> Pattern
newPattern d =
    Pattern
        { diagonal = d
        , blockedTiles = mempty
        , blockedEdges = defaultBlockedEdges d
        }

defaultBlockedEdges :: Int -> Set Edge
defaultBlockedEdges d = tops <> lefts <> bottoms <> rights
  where
    tops =
        Set.fromList $
            [getEdge (i - 1) (j - 1) TopEdge | i <- [1 .. 2 * d], j <- [1 .. 2 * d], hasTopEdge i j]
    lefts =
        Set.fromList $
            [getEdge (i - 1) (j - 1) LeftEdge | i <- [1 .. 2 * d], j <- [1 .. 2 * d], hasLeftEdge i j]
    bottoms =
        Set.fromList $
            [getEdge (i - 1) (j - 1) BottomEdge | i <- [1 .. 2 * d], j <- [1 .. 2 * d], hasBottomEdge i j]
    rights =
        Set.fromList $
            [getEdge (i - 1) (j - 1) RightEdge | i <- [1 .. 2 * d], j <- [1 .. 2 * d], hasRightEdge i j]

    hasTopEdge i j = inBaseRegion d i j && not (inBaseRegion d i (j + 1))
    hasLeftEdge i j = inBaseRegion d i j && not (inBaseRegion d (i - 1) j)
    hasBottomEdge i j = inBaseRegion d i j && not (inBaseRegion d i (j - 1))
    hasRightEdge i j = inBaseRegion d i j && not (inBaseRegion d (i + 1) j)

inBaseRegion :: Int -> Int -> Int -> Bool
inBaseRegion d i j = abs (fromIntegral i - x) + abs (fromIntegral j - x) <= x
  where
    x = fromIntegral d + 0.5 :: Double

-- | An edge is represented by its midpoint, where the center of pixel @(i,j)@ is @(i, j)@
newtype Edge = Edge (Int, Int)
    deriving (Eq, Ord, Show)

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
    baseArea = getBaseArea patt.diagonal
    bgImage = fromMaybe (defaultBgImage tileConf patt) mBgImage

    buildImage ts = runST $ do
        img <- thawImage bgImage
        mapM_ (doOverlay img) ts
        freezeImage img

    doOverlay :: MutableImage s PixelRGBA8 -> (Int, Int, Tile) -> ST s ()
    doOverlay img (i, j, t) = overlay (i * w) (j * w) (tile tileConf t) img
    w = tileConf.width

getBaseArea :: Int -> [(Int, Int)]
getBaseArea d = [(i - 1, j - 1) | i <- [1 .. 2 * d], j <- [1 .. 2 * d], inBaseRegion d i j]

getTiles :: Pattern -> [(Int, Int)] -> Either PatternRenderError [(Int, Int, Tile)]
getTiles patt = traverse getTile . filter (not . isBlocked)
  where
    getTile (i, j) = (i,j,) <$> tileForLocation patt i j
    isBlocked = (`Set.member` patt.blockedTiles)

defaultBgImage :: TileConfig -> Pattern -> Image PixelRGBA8
defaultBgImage conf patt = generateImage mkPixel w w
  where
    mkPixel _ _ = conf.background
    w = conf.width * 2 * patt.diagonal

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

overlay :: Pixel a => Int -> Int -> Image a -> MutableImage s a -> ST s ()
overlay i0 j0 imgOver imgUnder = mapM_ (uncurry updatePixel) overlayPixels
  where
    overlayPixels = [(i - 1, j - 1) | i <- [1 .. imageWidth imgOver], j <- [1 .. imageHeight imgOver]]
    updatePixel i j = writePixel imgUnder (i0 + i) (j0 + j) $ pixelAt imgOver i j
