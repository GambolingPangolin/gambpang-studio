{-# LANGUAGE DeriveFunctor #-}

module GambPang.Animation.Drawing.Grid (
    Grid (..),
    makeGrid,
) where

import GambPang.Animation.LinearAlgebra (Point (..))
import GambPang.Animation.Rectangle (Rectangle (..))
import GambPang.Animation.Rigging (Rigged (..))

data Grid a = Grid
    { gridBoundary :: Rectangle
    , gridRows :: Int
    , gridCols :: Int
    , -- | @gridVertex rowIx colIx center@
      gridVertex :: Int -> Int -> Point -> a
    }
    deriving (Functor)

instance Rigged (Grid a) where
    transform t g = g{gridBoundary = transform t $ gridBoundary g}

-- | Generate values corresponding to the vertices of a rectangular grid
makeGrid :: Grid a -> [a]
makeGrid (Grid (Rectangle ll ur) n m mkObject)
    | n <= 0 || m <= 0 = mempty
    | otherwise = objectGrid
  where
    objectGrid = [mkObject i j (mkLocation ll ur n m i j) | i <- [1 .. n], j <- [1 .. m]]

mkLocation :: Point -> Point -> Int -> Int -> Int -> Int -> Point
mkLocation ll ur n m i j =
    Point
        { pointX = if n == 1 then llx else llx + (fromIntegral i - 1) * hSep
        , pointY = if m == 1 then lly else lly + (fromIntegral j - 1) * vSep
        }
  where
    Point llx lly = ll
    Point urx ury = ur

    hSep = (urx - llx) / (fromIntegral n - 1)
    vSep = (ury - lly) / (fromIntegral m - 1)
