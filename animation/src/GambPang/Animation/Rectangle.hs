-- | High level abstraction for working with rectangles outside the context of a drawing
module GambPang.Animation.Rectangle (
    Rectangle (..),
    fromWidthHeight,
    centeredRectangle,
) where

import GambPang.Animation.LinearAlgebra (Point (Point))
import GambPang.Animation.Rigging (Rigged (..))

data Rectangle = Rectangle
    { lowerLeft :: Point
    , upperRight :: Point
    }
    deriving (Eq, Show)

instance Rigged Rectangle where
    transform t r =
        Rectangle
            { lowerLeft = transform t $ lowerLeft r
            , upperRight = transform t $ upperRight r
            }

fromWidthHeight :: Point -> Double -> Double -> Rectangle
fromWidthHeight ll@(Point llx lly) w h = Rectangle ll ur
  where
    ur = Point (llx + w) (lly + h)

centeredRectangle :: Double -> Double -> Rectangle
centeredRectangle w h =
    Rectangle
        { lowerLeft = Point llx lly
        , upperRight = Point urx ury
        }
  where
    llx = negate urx
    lly = negate ury

    urx = w / 2
    ury = h / 2
