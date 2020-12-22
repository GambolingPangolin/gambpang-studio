module GambPang.Animation.Rigging (
    Rigged (..),
    Motion,
    translate,
    rotateO,
    rotate,
    scale,
    scaleXY,
    reflect,
    followPath,
    cameraPan,
) where

import GambPang.Animation.LinearAlgebra (
    AffineTransformation,
    Point,
    Vector,
    applyAffineT,
    applyAffineTV,
    displacement,
    negateV,
    pointToVector,
    reflection,
    rotation,
    scaling,
    translation,
 )
import GambPang.Animation.Path (Path)
import GambPang.Animation.Scene (Animated (..))

class Rigged a where
    transform :: AffineTransformation -> a -> a

instance Rigged a => Rigged (Animated a) where
    transform a = fmap $ transform a

translate :: Rigged a => Vector -> a -> a
translate = transform . translation

-- | Rotate about the origin (counterclockwise)
rotateO :: Rigged a => Double -> a -> a
rotateO = transform . rotation

-- | Rotate about the provided point (counterclockwise)
rotate :: Rigged a => Point -> Double -> a -> a
rotate p a = translate vp . rotateO a . translate (negateV vp)
  where
    vp = pointToVector p

scaleXY ::
    Rigged a =>
    -- | x-scaling factor
    Double ->
    -- | y-scaling factor
    Double ->
    a ->
    a
scaleXY sx sy = transform $ scaling sx sy

scale :: Rigged a => Double -> a -> a
scale a = scaleXY a a

-- | Reflect across a line
reflect ::
    Rigged a =>
    -- | Normal vector
    Vector ->
    -- | Point on the line
    Point ->
    a ->
    a
reflect v = transform . reflection v

instance Rigged Point where
    transform = applyAffineT

instance Rigged Vector where
    transform = applyAffineTV

type Motion a = Animated (a -> a)

followPath :: Rigged a => Path -> Point -> Motion a
followPath p p0 = translate . displacement p0 <$> p

cameraPan :: Rigged a => Path -> Animated a -> Animated a
cameraPan path scene = translate <$> pathV <*> scene
  where
    pathV = negateV . pointToVector <$> path
