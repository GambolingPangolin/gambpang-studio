module GambPang.Animation.LinearAlgebra (
    -- * Coordinates and displacement
    Point (..),
    origin,
    displacement,
    pointToVector,
    Vector (..),
    negateV,
    displace,
    dual,
    norm,
    normalize,

    -- * Transformations
    AffineTransformation (..),
    invertAffine,
    translation,
    rotation,
    scaling,
    reflection,
    applyAffineT,
    applyAffineTV,
) where

import GambPang.Animation.Utils (power)

data Point = Point {pointX :: Double, pointY :: Double}
    deriving (Eq, Show)

origin :: Point
origin = Point 0 0

pointToVector :: Point -> Vector
pointToVector = Vector <$> pointX <*> pointY

displace :: Point -> Vector -> Point
displace p v = Point (x + dx) (y + dy)
  where
    Point x y = p
    Vector dx dy = v

displacement ::
    -- | Starting point
    Point ->
    -- | Ending point
    Point ->
    Vector
displacement p1 p2 = Vector dx dy
  where
    Point x1 y1 = p1
    Point x2 y2 = p2
    dx = x2 - x1
    dy = y2 - y1

data Vector = Vector {displacementX :: Double, displacementY :: Double}
    deriving (Eq, Show)

instance Semigroup Vector where
    Vector vx1 vy1 <> Vector vx2 vy2 = Vector (vx1 + vx2) (vy1 + vy2)

instance Monoid Vector where
    mempty = Vector 0 0

negateV :: Vector -> Vector
negateV (Vector x y) = Vector (negate x) (negate y)

normalize :: Vector -> Vector
normalize v@(Vector dx dy) = Vector dx' dy'
  where
    dx' = dx / n
    dy' = dy / n
    n = norm v

norm :: Vector -> Double
norm (Vector dx dy) = sqrt $ power 2 dx + power 2 dy

dual :: Vector -> Point -> Double
dual (Vector dx dy) (Point x y) = dy * x - dx * y

data AffineTransformation = AffineTransformation
    { affine11 :: Double
    , affine12 :: Double
    , affine13 :: Double
    , affine21 :: Double
    , affine22 :: Double
    , affine23 :: Double
    }
    deriving (Eq, Show)

instance Semigroup AffineTransformation where
    t1 <> t2 =
        AffineTransformation
            { affine11 = affine11 t1 * affine11 t2 + affine12 t1 * affine21 t2
            , affine12 = affine11 t1 * affine12 t2 + affine12 t2 * affine22 t2
            , affine13 = affine11 t1 * affine13 t2 + affine12 t1 * affine23 t2 + affine13 t1
            , affine21 = affine21 t1 * affine11 t2 + affine22 t1 * affine21 t2
            , affine22 = affine21 t1 * affine12 t2 + affine22 t1 * affine22 t2
            , affine23 = affine21 t1 * affine13 t2 + affine22 t1 * affine23 t2 + affine23 t1
            }

instance Monoid AffineTransformation where
    mempty =
        AffineTransformation
            { affine11 = 1
            , affine12 = 0
            , affine13 = 0
            , affine21 = 0
            , affine22 = 1
            , affine23 = 0
            }

invertAffine :: AffineTransformation -> AffineTransformation
invertAffine a =
    AffineTransformation
        { affine11 = b11
        , affine12 = b12
        , affine13 = negate $ b11 * affine13 a + b12 * affine23 a
        , affine21 = b21
        , affine22 = b22
        , affine23 = negate $ b21 * affine13 a + b22 * affine23 a
        }
  where
    b11 = affine22 a / d
    b12 = negate $ affine12 a / d
    b21 = negate $ affine21 a / d
    b22 = affine11 a / d
    d = det a

det :: AffineTransformation -> Double
det a = affine11 a * affine22 a - affine12 a * affine21 a

applyAffineT :: AffineTransformation -> Point -> Point
applyAffineT t (Point x y) = Point x' y'
  where
    x' = affine11 t * x + affine12 t * y + affine13 t
    y' = affine21 t * x + affine22 t * y + affine23 t

applyAffineTV :: AffineTransformation -> Vector -> Vector
applyAffineTV t (Vector dx dy) = Vector dx' dy'
  where
    dx' = affine11 t * dx + affine12 t * dy
    dy' = affine21 t * dx + affine22 t * dy

translation :: Vector -> AffineTransformation
translation (Vector dx dy) = mempty{affine13 = dx, affine23 = dy}

rotation :: Double -> AffineTransformation
rotation a =
    AffineTransformation
        { affine11 = cos a
        , affine12 = negate $ sin a
        , affine13 = 0
        , affine21 = sin a
        , affine22 = cos a
        , affine23 = 0
        }

scaling :: Double -> Double -> AffineTransformation
scaling sx sy = mempty{affine11 = sx, affine22 = sy}

reflection :: Vector -> Point -> AffineTransformation
reflection v p = translation vp <> ref <> translation (negateV vp)
  where
    vp = pointToVector p
    ref =
        mempty
            { affine11 = 1 - 2 * nvx * nvx
            , affine21 = negate $ 2 * nvx * nvy
            , affine12 = negate $ 2 * nvy * nvx
            , affine22 = 1 - 2 * nvy * nvy
            }
    Vector nvx nvy = normalize v
