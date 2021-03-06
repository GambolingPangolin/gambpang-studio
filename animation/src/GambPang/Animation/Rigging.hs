module GambPang.Animation.Rigging (
    Rigged (..),
    Motion,
    translate,
    translating,
    rotateO,
    rotate,
    scale,
    scaling,
    scaleXY,
    reflect,
    followPath,
    cameraPan,
    rotatingO,
    rotating,
    rescale,
    unitRescale,
) where

import GambPang.Animation.Animated (Animated, frame, time)
import GambPang.Animation.LinearAlgebra (
    AffineTransformation,
    Point,
    Vector (..),
    applyAffineT,
    applyAffineTV,
    displacement,
    negateV,
    pointToVector,
    reflection,
    rotation,
    translation,
 )
import qualified GambPang.Animation.LinearAlgebra as LA
import GambPang.Animation.Path (Path)
import GambPang.Animation.Rectangle (Rectangle (..), center, height, unitRectangle, width)

class Rigged a where
    transform :: AffineTransformation -> a -> a

instance Rigged a => Rigged (Animated a) where
    transform a = fmap $ transform a

instance (Rigged a, Rigged b) => Rigged (a, b) where
    transform a (x, y) = (transform a x, transform a y)

instance Rigged Rectangle where
    transform t r =
        Rectangle
            { lowerLeft = transform t $ lowerLeft r
            , upperRight = transform t $ upperRight r
            }

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
scaleXY sx sy = transform $ LA.scaling sx sy

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

translating ::
    Rigged a =>
    -- | Rate of translation
    Double ->
    -- | Direction of translation
    Vector ->
    Motion a
translating r (Vector dx dy) = tr <$> time
  where
    tr t = translate (dv t)
    dv t = Vector (t * r * dx) (t * r * dy)

-- | Rotate about the origin
rotatingO ::
    Rigged a =>
    -- | Rate of rotation
    Double ->
    Motion a
rotatingO r = rot <$> time
  where
    rot t = rotateO (2 * r * t * pi)

rotating ::
    Rigged a =>
    -- | Center of rotation
    Point ->
    -- | Rate of rotation
    Double ->
    Motion a
rotating p r = rot <$> time
  where
    rot t = rotate p (2 * r * t * pi)

scaling ::
    Rigged a =>
    -- | Rate of scaling
    Double ->
    Motion a
scaling r = scale . (r *) <$> time

-- | Given a point and a rectangle, produce a new point with the same relation to the animation frame
rescale :: Rigged a => Rectangle -> Animated a -> Animated a
rescale r0 a = doRescale <$> frame <*> a
  where
    doRescale r1 = translate (dc r1) . scaleR r1 . translate (negateV $ dc r0)
    dc = pointToVector . center
    scaleR r1 = scaleXY (width r1 / width r0) (height r1 / height r0)

-- | Rescale in the context of the unit rectangle
unitRescale :: Rigged a => Animated a -> Animated a
unitRescale = rescale unitRectangle
