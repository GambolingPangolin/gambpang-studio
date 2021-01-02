-- | High level abstraction for working with rectangles outside the context of a drawing
module GambPang.Animation.Rectangle (
    Rectangle (..),
    height,
    width,
    center,
    fromWidthHeight,
    centeredRectangle,
    unitRectangle,
) where

import GambPang.Animation.LinearAlgebra (Point (..), midpoint)

data Rectangle = Rectangle
    { lowerLeft :: Point
    , upperRight :: Point
    }
    deriving (Eq, Show)

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

width :: Rectangle -> Double
width (Rectangle ll ur) = pointX ur - pointX ll

height :: Rectangle -> Double
height (Rectangle ll ur) = pointY ur - pointX ll

center :: Rectangle -> Point
center (Rectangle ll ur) = midpoint ll ur

-- | The rectangle with corners at @(-1, -1)@ and @(1, 1)@
unitRectangle :: Rectangle
unitRectangle =
    Rectangle
        { lowerLeft = Point (-1) (-1)
        , upperRight = Point 1 1
        }
