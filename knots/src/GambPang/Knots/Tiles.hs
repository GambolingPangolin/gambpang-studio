{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module GambPang.Knots.Tiles (
    Tile (..),
    TileType (..),
    Position,
    TileConfig (..),
    tile,
) where

import Codec.Picture (Image, PixelRGBA8, generateImage)

type Position = Int

data Tile = Tile
    { tileType :: TileType
    , position :: Position
    }
    deriving (Eq, Show)

data TileType = Elbow | Crossing | Straight
    deriving (Eq, Show)

data TileConfig = TileConfig
    { foreground :: PixelRGBA8
    , background :: PixelRGBA8
    , width :: Int
    }
    deriving (Eq, Show)

tile :: TileConfig -> Tile -> Image PixelRGBA8
tile conf t = case t.tileType of
    Elbow -> elbow conf t.position
    Crossing -> crossing conf t.position
    Straight -> straight conf t.position

elbow :: TileConfig -> Position -> Image PixelRGBA8
elbow conf p = generateImage generate w w
  where
    generate
        | p `mod` 4 == 0 = upLeft
        | p `mod` 4 == 1 = upRight
        | p `mod` 4 == 2 = downRight
        | otherwise = downLeft
    upLeft i j
        | innerThird w i && lastThird w j = conf.foreground
        | firstThird w i && innerThird w j = conf.foreground
        | innerThird w i && innerThird w j = conf.foreground
        | otherwise = conf.background
    upRight i j = upLeft (w - i) j
    downRight = flip upLeft
    downLeft i j = upLeft i (w - j)
    w = conf.width

crossing :: TileConfig -> Position -> Image PixelRGBA8
crossing conf p = generateImage generate w w
  where
    generate
        | even p = horizontal
        | otherwise = vertical
    horizontal i j
        | innerThird w i = conf.foreground
        | innerThird w j && not (thickInnerThird w i) = conf.foreground
        | otherwise = conf.background
    vertical = flip horizontal
    w = conf.width

straight :: TileConfig -> Position -> Image PixelRGBA8
straight conf p = generateImage generate w w
  where
    generate
        | even p = horizontal
        | otherwise = vertical
    horizontal _ j
        | innerThird w j = conf.foreground
        | otherwise = conf.background
    vertical = flip horizontal
    w = conf.width

firstThird :: Int -> Int -> Bool
firstThird w x = 3 * x < w

innerThird :: Int -> Int -> Bool
innerThird w x = 3 * x >= w && 3 * x <= 2 * w

thickInnerThird :: Int -> Int -> Bool
thickInnerThird w x = 9 * x >= 2 * w && 9 * x <= 7 * w

lastThird :: Int -> Int -> Bool
lastThird w x = 3 * x > 2 * w
