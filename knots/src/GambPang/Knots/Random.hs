{-# LANGUAGE TypeApplications #-}

module GambPang.Knots.Random (
    randomPattern,
    randomByImage,
) where

import Codec.Picture (Image, Pixel8, imageHeight, imageWidth)
import Codec.Picture.Types (pixelFoldM)
import Control.Monad (foldM)
import Data.Bool (bool)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import GambPang.Knots.Pattern (
    Edge (Edge),
    EdgePosition (..),
    Pattern,
    blockedEdges,
    getEdge,
    newPattern,
 )
import System.Random (randomRIO)

randomPattern ::
    Double ->
    -- | width
    Int ->
    -- | height
    Int ->
    IO Pattern
randomPattern p w h = foldM onEdge (newPattern w h) allEdges
  where
    allEdges = [Edge (i, j) | i <- edgeIndices, j <- edgeIndices, odd (i + j)]
    edgeIndices = [1 .. 2 * (w + h) + 1]
    onEdge thePattern e = bool id (addEdge e) <$> sample e <*> pure thePattern
    addEdge e thePattern = thePattern{blockedEdges = Set.insert e $ blockedEdges thePattern}
    sample (Edge (_, j)) = (< threshold j) <$> randomRIO (0, 1)
    threshold = (p *) . sqrt . sawtooth
    sawtooth j
        | j <= w + h + 1 = fromIntegral j / fromIntegral (w + h + 1)
        | otherwise = fromIntegral (2 * (w + h + 1) - j) / fromIntegral (w + h - 1)

randomByImage :: Image Pixel8 -> IO Pattern
randomByImage img = do
    imgMaskedEdges <- pixelFoldM analyzePixel mempty img
    pure basePattern{blockedEdges = blockedEdges basePattern <> imgMaskedEdges}
  where
    basePattern = newPattern w h
    w = imageWidth img
    h = imageHeight img

    analyzePixel accumEdges x y thePixel =
        (accumEdges <>) . Set.fromList . catMaybes
            <$> sequence
                [ sample thePixel $ getEdge (1 + x + y) (w + y - x) TopEdge
                , sample thePixel $ getEdge (1 + x + y) (w + y - x) RightEdge
                , sample thePixel $ getEdge (2 + x + y) (w + y - x + 1) BottomEdge
                , sample thePixel $ getEdge (2 + x + y) (w + y - x + 1) LeftEdge
                ]
    sample thePixel x = bool Nothing (Just x) . (> (fromIntegral thePixel / 255)) <$> randomRIO @Double (0, 1)
