module GambPang.Animation.Field2D (
    Field2D (..),
    PixelField,
    valueAtPoint,
    fill,
    disc,
    rectangle,
) where

import Codec.Picture (PixelRGBA8)

import GambPang.Animation.LinearAlgebra (
    Point (Point),
    applyAffineT,
    invertAffine,
 )
import GambPang.Animation.Rigging (Rigged (..))
import GambPang.Animation.Utils (power)

-- | A value parameterized by points in the plane
newtype Field2D a = Field2D (Point -> a)

valueAtPoint :: Point -> Field2D a -> a
valueAtPoint pt (Field2D f) = f pt

instance Functor Field2D where
    fmap f (Field2D px) = Field2D $ f . px

instance Rigged (Field2D a) where
    transform a (Field2D s) = Field2D $ s . applyAffineT (invertAffine a)

type PixelField = Field2D PixelRGBA8

fill :: a -> Field2D a
fill = Field2D . const

disc ::
    Double ->
    -- | Background
    a ->
    -- | Foreground
    a ->
    Field2D a
disc r bg fg = Field2D theDisc
  where
    theDisc (Point x y)
        | power 2 x + power 2 y <= power 2 r = fg
        | otherwise = bg

rectangle ::
    -- | Width
    Double ->
    -- | Height
    Double ->
    -- | Background
    a ->
    -- | Foreground
    a ->
    Field2D a
rectangle w h bg fg = Field2D theRectangle
  where
    theRectangle (Point x y)
        | 0 <= x && x <= w && 0 <= y && y <= h = fg
        | otherwise = bg
