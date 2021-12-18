module GambPang.Knots.Random (
    randomPattern,
) where

import Control.Monad (foldM)
import Data.Bool (bool)
import qualified Data.Set as Set
import GambPang.Knots.Pattern (Edge (Edge), Pattern, blockedEdges, newPattern)
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
    onEdge thePattern e = bool id (addEdge e) <$> sample <*> pure thePattern
    addEdge e thePattern = thePattern{blockedEdges = Set.insert e $ blockedEdges thePattern}
    sample = (< p) <$> randomRIO (0, 1)
