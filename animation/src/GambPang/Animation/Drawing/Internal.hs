{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module GambPang.Animation.Drawing.Internal (
    Drawing (..),
    union,
    draw,
    mask,
    exclude,
    transform,
    mapColor,
    Shape (..),
    disc,
    ellipse,
    rectangle,
    polygon,
    transformShape,

    -- * Rendering
    renderDrawing,
    renderShape,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import GambPang.Animation.Bitmap (BoundingBox (..), boundingBoxPixels, pixelPoint)
import GambPang.Animation.LinearAlgebra (
    AffineTransformation,
    Point (..),
    Vector (Vector),
    affine11,
    affine12,
    affine21,
    affine22,
    applyAffineT,
    applyAffineTV,
    displace,
    displacement,
    dual,
    invertAffine,
    negateV,
    origin,
 )
import GambPang.Animation.Rigging (Rigged (..))

data Drawing color
    = DrawShape color Shape
    | -- | Earlier items overlap later items
      Union [Drawing color]
    | Mask Shape (Drawing color)
    | Exclude Shape (Drawing color)
    deriving (Eq, Show)

instance Rigged (Drawing color) where
    transform = transformDrawing

mapColor :: (color1 -> color2) -> Drawing color1 -> Drawing color2
mapColor f = \case
    DrawShape c s -> DrawShape (f c) s
    Union ds -> Union $ mapColor f <$> ds
    Mask s d -> Mask s $ mapColor f d
    Exclude s d -> Exclude s $ mapColor f d

draw :: color -> Shape -> Drawing color
draw = DrawShape

union :: [Drawing color] -> Drawing color
union = Union

mask :: Shape -> Drawing color -> Drawing color
mask = Mask

exclude :: Shape -> Drawing color -> Drawing color
exclude = Exclude

transformDrawing :: AffineTransformation -> Drawing color -> Drawing color
transformDrawing t = \case
    DrawShape c s -> DrawShape c $ transformShape t s
    Union ds -> Union $ transformDrawing t <$> ds
    Mask m s -> Mask (transformShape t m) (transformDrawing t s)
    Exclude m s -> Exclude (transformShape t m) (transformDrawing t s)

data EllipseSpec = EllipseSpec Double Double Double Double
    deriving (Eq, Show)

data Shape
    = Ellipse Point EllipseSpec
    | Parallelogram Point Vector Vector
    | Polygon [Point]
    deriving (Eq, Show)

instance Rigged Shape where
    transform = transformShape

disc ::
    -- | Center
    Point ->
    -- | Radius
    Double ->
    Shape
disc p r = Ellipse p es
  where
    es = EllipseSpec (1 / r) 0 0 (1 / r)

ellipse ::
    -- | Center
    Point ->
    -- | H-radius
    Double ->
    -- | V-radius
    Double ->
    Shape
ellipse p r1 r2 = Ellipse p es
  where
    es = EllipseSpec (1 / r1) 0 0 (1 / r2)

rectangle ::
    -- | Width
    Double ->
    -- | Height
    Double ->
    Shape
rectangle w h = Parallelogram origin vx vy
  where
    vx = Vector w 0
    vy = Vector 0 h

polygon ::
    -- | Vertices
    [Point] ->
    Shape
polygon = Polygon

transformShape :: AffineTransformation -> Shape -> Shape
transformShape t = \case
    Ellipse p es -> Ellipse (applyAffineT t p) $ transformEllipse t es
    Parallelogram p v1 v2 -> Parallelogram (applyAffineT t p) (applyAffineTV t v1) (applyAffineTV t v2)
    Polygon ps -> Polygon $ applyAffineT t <$> ps

transformEllipse :: AffineTransformation -> EllipseSpec -> EllipseSpec
transformEllipse t es = EllipseSpec a' b' c' d'
  where
    EllipseSpec a b c d = es
    a' = a * affine11 t' + b * affine21 t'
    b' = a * affine12 t' + b * affine22 t'
    c' = c * affine11 t' + d * affine21 t'
    d' = c * affine12 t' + d * affine22 t'
    t' = invertAffine t

renderDrawing :: Drawing color -> Map (Int, Int) color
renderDrawing = \case
    DrawShape c s -> Map.fromList $ (,c) <$> renderShape s
    Union ds -> Map.unions $ renderDrawing <$> ds
    Mask m d -> Map.filterWithKey applyMask $ renderDrawing d
      where
        maskPixels = Set.fromList $ renderShape m
        applyMask p _ = p `Set.member` maskPixels
    Exclude m d -> Map.filterWithKey excludeRegion $ renderDrawing d
      where
        excludePixels = Set.fromList $ renderShape m
        excludeRegion p _ = not $ p `Set.member` excludePixels

renderShape :: Shape -> [(Int, Int)]
renderShape = \case
    Ellipse p es -> objectPixels (ellipseBoundingBox p es) (ellipsePointTest p es)
    Parallelogram p v1 v2 -> polygonPixels $ parallelogramVertices p v1 v2
    Polygon ps -> polygonPixels ps

ellipsePointTest :: Point -> EllipseSpec -> Point -> Bool
ellipsePointTest p (EllipseSpec a b c d) q = l1 dx dy ^ (2 :: Int) + l2 dx dy ^ (2 :: Int) <= 1
  where
    Vector dx dy = displacement p q
    l1 x y = a * x + b * y
    l2 x y = c * x + d * y

ellipseBoundingBox :: Point -> EllipseSpec -> BoundingBox
ellipseBoundingBox p es =
    BoundingBox
        { boxLowerLeft = (xMin, yMin)
        , boxUpperRight = (xMax, yMax)
        }
  where
    EllipseSpec a b c d = es
    m = abs $ a * d - c * b

    v1 = Vector (4 * b / m) $ 4 * negate a / m
    v2 = Vector (4 * d / m) $ 4 * negate c / m

    ps =
        displace p
            <$> [ v1 <> v2
                , negateV v1 <> v2
                , v1 <> negateV v2
                , negateV (v1 <> v2)
                ]

    xs = pointX <$> ps
    ys = pointY <$> ps

    xMin = floor $ minimum xs
    yMin = floor $ minimum ys
    xMax = ceiling $ maximum xs
    yMax = ceiling $ maximum ys

parallelogramVertices :: Point -> Vector -> Vector -> [Point]
parallelogramVertices p v1@(Vector x1 y1) v2@(Vector x2 y2)
    | x1 * y2 - x2 * y1 < 0 =
        [p, displace p v1, displace p (v1 <> v2), displace p v2]
    | otherwise =
        [p, displace p v2, displace p (v1 <> v2), displace p v1]

polygonPixels :: [Point] -> [(Int, Int)]
polygonPixels ps = objectPixels (polygonBoundingBox ps) (polygonPointTest ps)

polygonPointTest :: [Point] -> Point -> Bool
polygonPointTest [] _ = error "polygonPointTest: not enough points"
polygonPointTest ps q = inHull ps || inHull (reverse ps)
  where
    test (p1, p2) = let l = dual (displacement p1 p2) in l q >= l p1
    inHull points = all test $ zip points (drop 1 $ cycle points)

polygonBoundingBox :: [Point] -> BoundingBox
polygonBoundingBox ps =
    BoundingBox
        { boxLowerLeft = (xMin, yMin)
        , boxUpperRight = (xMax, yMax)
        }
  where
    xs = pointX <$> ps
    ys = pointY <$> ps

    xMin = floor $ minimum xs
    yMin = floor $ minimum ys

    xMax = ceiling $ maximum xs
    yMax = ceiling $ maximum ys

objectPixels :: BoundingBox -> (Point -> Bool) -> [(Int, Int)]
objectPixels bb test = filter (test . uncurry pixelPoint) $ boundingBoxPixels bb
