{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module GambPang.Animation.Dots (
    animations,
    dots1,
    dots2,
    dots3,
    dots4,
    dots5,
    dots6,
    dots7,
    dots8,
    dots9,
    dots10,
    dots11,
    dots12,
    dots13,
    dots14,
    dots15,
    dots16,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import GambPang.Animation (
    Animated (..),
    Path,
    Point (..),
    Time (..),
    Vector (..),
    backwards,
    circularPath,
    followPath,
    makeCircular,
    negateV,
    norm,
    normalize,
    origin,
    pathProgram,
    piecewiseLinear,
    point,
    pointToVector,
    rotateO,
    shiftEarlier,
    shiftLater,
    time,
    translate,
 )
import GambPang.Animation.Drawing (Drawing)
import qualified GambPang.Animation.Drawing as D

import GambPang.Animation.ColorStyle (
    ColorStyle (..),
    PaletteChoice,
    calico,
    californiacoast,
    france,
    lux,
    snowy,
    sunrise,
    terracotta,
 )
import GambPang.Animation.Field2D (Field2D)
import GambPang.Animation.Piece (
    AnimatedPiece (..),
    AnimationSource (AnimatedDrawing),
    applyPaletteChoice,
 )
import GambPang.Animation.Utils (
    defaultAnimatedPiece,
    defaultViewFrame,
    grating,
    makeGrid,
    originViewFrame,
    rotating,
    scaleField,
    translationField,
 )

animations :: PaletteChoice -> Map Text AnimatedPiece
animations paletteChoice =
    applyPaletteChoice paletteChoice
        <$> Map.fromList
            [ ("dots-1", dots1)
            , ("dots-2", dots2)
            , ("dots-3", dots3)
            , ("dots-4", dots4)
            , ("dots-5", dots5)
            , ("dots-6", dots6)
            , ("dots-7", dots7)
            , ("dots-8", dots8)
            , ("dots-9", dots9)
            , ("dots-10", dots10)
            , ("dots-11", dots11)
            , ("dots-12", dots12)
            , ("dots-13", dots13)
            , ("dots-14", dots14)
            , ("dots-15", dots15)
            , ("dots-16", dots16)
            ]

dots1 :: AnimatedPiece
dots1 = defaultAnimatedPiece . translate v' $ rotating 1 <*> pure s0
  where
    d = D.draw Foreground $ D.disc origin 10
    s0 = translate v d
    v = Vector 0 175
    v' = Vector 250 250

dots2 :: AnimatedPiece
dots2 = defaultAnimatedPiece $ translate v $ union2 <$> movingDot <*> pure field
  where
    n = 5
    v = Vector offset offset
    offset = 50
    gapSize = 400 / fromIntegral (n - 1)
    field = dotField gapSize $ uniformDotFieldSpec n 5 Foreground

    union2 x y = D.union [x, y]

    movingDot = followPath path origin <*> pure d

    d = D.draw HighlightA $ D.disc origin 20
    path =
        piecewiseLinear $
            pathProgram
                (Time 1)
                ( Point 0 0
                    :| [ Point 400 0
                       , Point 400 400
                       , Point 0 400
                       , Point 0 0
                       ]
                )

dotField ::
    -- | Gap
    Double ->
    Map (Int, Int) (Double, c) ->
    Drawing c
dotField s colors = D.union $ Map.foldlWithKey addDot mempty colors
  where
    addDot dots (x, y) (r, c) = D.draw c (D.disc (center x y) r) : dots
    center x y = Point (s * fromIntegral x) (s * fromIntegral y)

uniformDotFieldSpec :: Int -> Double -> c -> Map (Int, Int) (Double, c)
uniformDotFieldSpec n r c =
    Map.fromList $ [((x, y), (r, c)) | x <- [0 .. n - 1], y <- [0 .. n - 1]]

dots3 :: AnimatedPiece
dots3 = defaultAnimatedPiece . translate v . fmap D.union $ traverse mkDot [0 .. 10]
  where
    mkDot i = shiftEarlier (Time $ i / 10) dot
    dot = followPath sinPath p0 <*> pure d
    sinPath = Animated $ \(Time t) -> let s = toCircular t in Point (500 * s) (200 * sin (2 * pi * s))
    v = Vector 0 250
    d = D.draw Foreground $ D.disc origin 10
    p0 = origin
    toCircular = snd . properFraction @_ @Int

dots4 :: AnimatedPiece
dots4 =
    AnimatedPiece
        { source =
            AnimatedDrawing $
                translate v . D.union
                    <$> sequenceA
                        [ dotElement
                        , rotateO (2 * pi / 3) dotElement
                        , rotateO (4 * pi / 3) dotElement
                        ]
        , viewFrame = defaultViewFrame
        , frameCount = 200
        , framesPerSec = 33
        , palette = snowy
        }
  where
    dotElement = rotateO <$> (mkAngle <$> time) <*> pure d
    d = D.draw Foreground $ D.disc pCenter 20
    mkAngle (Time t)
        | t <= 0.5 = 2 * m * pi * progress (2 * t)
        | otherwise = negate $ 2 * m * progress (2 * t - 1)
    progress t = sin (pi * t)
    pCenter = Point 100 0
    v = Vector 250 250
    m = 2

-- | In which dots coil around an annulus
dots5 :: AnimatedPiece
dots5 = piece{viewFrame = originViewFrame, frameCount = 500}
  where
    piece = defaultAnimatedPiece $ withSwapping isAboveAnnulus (pure annulus) coilingDot
    annulus = D.exclude innerCircle $ D.draw Foreground outerCircle
    outerCircle = D.disc origin rOuter
    innerCircle = D.disc origin rInner

    rInner = 150
    rOuter = 200
    rCoiler = 20

    coiler = D.draw HighlightA $ D.disc origin rCoiler

    rInnerPath = rInner - rCoiler - 5
    rOuterPath = rOuter + rCoiler + 5

    coilingDot = followPath (toroidalPath rInnerPath rOuterPath verticalWindingNumber 1) origin <*> pure coiler
    isAboveAnnulus (Time t) = sin (2 * pi * verticalWindingNumber * t) > 0

    verticalWindingNumber = 5

    withSwapping criterion x y = implementLayering criterion <$> time <*> x <*> y

    implementLayering criterion t x y
        | criterion t = D.union [y, x]
        | otherwise = D.union [x, y]

toroidalPath ::
    -- | Inner radius
    Double ->
    -- | Outer radius
    Double ->
    -- | Winding number (vertical)
    Double ->
    -- | Winding number (longitudinal)
    Double ->
    Path
toroidalPath r1 r2 n m = Animated $ \(Time t) -> toroidalProjection r1 r2 (a1 t, a2 t)
  where
    a1 = (2 * pi * m *)
    a2 = (2 * pi * n *)

toroidalProjection ::
    -- | Inner radius
    Double ->
    -- | Outer radius
    Double ->
    (Double, Double) ->
    Point
toroidalProjection r1 r2 (a1, a2) = Point x y
  where
    x = r * cos a1
    y = r * sin a1
    r = r1 + (r2 - r1) * u
    u = (1 + cos a2) / 2

-- In which dots move under a grating
dots6 :: AnimatedPiece
dots6 = piece{viewFrame = originViewFrame, frameCount = 200, framesPerSec = 20}
  where
    piece = defaultAnimatedPiece $ D.union <$> sequenceA [thisGrating <$> hamster, pure dotMatrix]

    dotMatrix = D.union $ makeGrid ll ur 7 7 topDot

    thisGrating = grating ll ur 7 7 (D.disc origin 20)
    hamster = followPath (circularPath (Time 1) origin 150) origin <*> pure dot

    dot = D.draw Foreground $ D.disc origin 80
    topDot i j p = D.draw (getColor i j) $ D.disc p 10
    getColor i j
        | even (i + j) = HighlightA
        | otherwise = HighlightB

    ll = Point (-200) (-200)
    ur = Point 200 200

-- | In which dots are displaced by an ocillating field
dots7 :: AnimatedPiece
dots7 = piece{viewFrame = originViewFrame, palette = terracotta}
  where
    piece = defaultAnimatedPiece $ D.union <$> sequenceA dots
    dot c p = D.draw c $ D.disc p 5
    wobblingDot c p = translationField (displacementField 7.5) p <*> pure (dot c p)
    dots = makeGrid ll ur 10 10 $ \i j -> wobblingDot (getColor i j)
    ll = Point (-200) (-200)
    ur = Point 200 200

    getColor i j
        | (i + j) `mod` 3 == 0 = Foreground
        | (i + j) `mod` 3 == 1 = HighlightA
        | otherwise = HighlightB

displacementField :: Double -> Animated (Field2D Vector)
displacementField a = mkField <$> time
  where
    mkField (Time t) = getVector t <$> point
    getVector t p =
        let v = pointToVector p
            u = normalize v
            s = (a *) . sin . (2 * pi *) $ norm v / 250 + t
            dx = s * displacementX u
            dy = s * displacementY u
         in Vector dx dy

-- | In which dots at the top scroll by faster than dots on the bottom
dots8 :: AnimatedPiece
dots8 = piece{palette = france}
  where
    piece = defaultAnimatedPiece $ flowDots dotCount rowCount rowParams
    rowParams = zipWith3 FlowRowParams colors [1 .. rowCount] $ fromIntegral <$> [1 .. rowCount]
    colors = cycle [HighlightA, Foreground, HighlightB]
    dotCount = 12
    rowCount = 10

data FlowRowParams = FlowRowParams
    { frpColor :: ColorStyle
    , frpRowIndex :: Int
    , frpRowSpeed :: Double
    }
    deriving (Eq, Show)

flowDots :: Int -> Int -> [FlowRowParams] -> Animated (Drawing ColorStyle)
flowDots dotCount rowCount = fmap D.union . traverse mkRow
  where
    mkRow frp = D.union . dots (frpColor frp) (frpRowIndex frp) (frpRowSpeed frp) <$> time
    dots c i s t = dot c i s t <$> [-1 .. fromIntegral dotCount]
    dot c i s (Time t) j = D.draw c $ D.disc (getPosition i j s t) 10
    getPosition i j s t = Point (getX j s t) (getY i)

    getX j s t =
        let f = floor $ s * t
            offset = s * t - fromIntegral @Int f
         in (j + offset) * colWidth
    getY i = fromIntegral i * rowHeight

    rowHeight = 500 / (fromIntegral rowCount + 1)
    colWidth = 500 / fromIntegral dotCount

-- | In which the flow is fast in the middle and slow on the outsides
dots9 :: AnimatedPiece
dots9 = piece{palette = sunrise}
  where
    piece = defaultAnimatedPiece $ flowDots dotCount rowCount rowParams
    dotCount = 12
    rowCount = 11
    rowParams = zipWith3 FlowRowParams colors [1 .. rowCount] speeds
    colors =
        mconcat
            [ replicate 3 Foreground
            , replicate 2 HighlightA
            , replicate 1 HighlightB
            , replicate 2 HighlightA
            , replicate 3 Foreground
            ]

    speeds = [1 .. 6] <> reverse [1 .. 5]

-- | In which dots move along concentric circles
dots10 :: AnimatedPiece
dots10 = piece{viewFrame = originViewFrame, palette = californiacoast}
  where
    piece = defaultAnimatedPiece $ D.union <$> traverse mkRing [0 .. 10 :: Int]
    mkRing i
        | even i = ring i
        | otherwise = backwards $ ring i

    dots i = D.union $ dot (getColor i) . getPoint (getR i) <$> getAngles i
    dot c p = D.draw c $ D.disc p 10
    ring i = rotating (getRate i) <*> pure (dots i)

    getRate i = 1 / fromIntegral (getCount i)
    getAngles i =
        let n = getCount i
         in (2 * pi *) . (/ fromIntegral n) . fromIntegral <$> [1 .. n]
    getR i = 30 * fromIntegral i + 50
    getPoint r a = Point (r * cos a) (r * sin a)
    getColor i
        | i `mod` 3 == 0 = Foreground
        | i `mod` 3 == 1 = HighlightA
        | otherwise = HighlightB
    getCount i = 2 * i + smallestCircleCount
    smallestCircleCount = 8

-- | In which dots travel around square obstacles
dots11 :: AnimatedPiece
dots11 = piece{palette = lux, viewFrame = originViewFrame}
  where
    piece =
        defaultAnimatedPiece $
            D.union <$> sequenceA [pure innerFrame, animatedDots, pure outerFrame]

    innerFrame = squareFrame 50 75 HighlightA
    outerFrame = squareFrame 100 125 HighlightB

    animatedDots = D.union <$> sequenceA [animatedDot, shiftLater (Time 0.2) animatedDot]

    animatedDot = makeCircular (Time 1) $ followPath path origin <*> pure dot
    dot = D.draw Foreground $ D.disc origin 15
    path = getPath <$> time

    getPath t =
        let r = getR t
            a = getA t
         in Point (r * cos a) (r * sin a)
    getR (Time t) = (* 225) . sin $ 2 * pi * t
    getA (Time t)
        | t <= 0.5 = 2 * pi * t
        | otherwise = negate $ 2 * pi * (t - 1)

squareFrame :: Double -> Double -> color -> Drawing color
squareFrame s1 s2 c = D.exclude sq1 $ D.draw c sq2
  where
    sq1 = translate (negateV $ Vector s1 s1) $ D.rectangle (2 * s1) (2 * s1)
    sq2 = translate (negateV $ Vector s2 s2) $ D.rectangle (2 * s2) (2 * s2)

-- | In which a bump propagates through a dot field
dots12 :: AnimatedPiece
dots12 = piece{viewFrame = originViewFrame, palette = calico}
  where
    piece = defaultAnimatedPiece $ D.union <$> sequenceA dots

    dots = makeGrid ll ur 10 10 $ \i j -> wobblingDot (getColor i j)
    ll = Point (-200) (-200)
    ur = Point 200 200

    wobblingDot c p = translationField bumpField p <*> pure (dot c p)
    dot c p = D.draw c $ D.disc p 5

    getColor i j
        | (i + j) `mod` 3 == 0 = Foreground
        | (i + j) `mod` 3 == 1 = HighlightA
        | otherwise = HighlightB

    bumpField = followPath (circularPath (Time 1) origin 165) origin <*> pure bumpField0
    bumpField0 = bumpFieldVector <$> point
    bumpFieldVector (Point 0 0) = Vector 0 0
    bumpFieldVector p =
        let dx = s * ndx
            dy = s * ndy
            vp = pointToVector p
            s = (d *) . exp . negate $ (a * norm vp) ^ (2 :: Int)
            Vector ndx ndy = normalize vp
         in Vector dx dy
    a = 1 / 100
    d = 10

-- | In which dots interleave among some rectangles
dots13 :: AnimatedPiece
dots13 = piece{viewFrame = originViewFrame, frameCount = 200}
  where
    piece = defaultAnimatedPiece $ squareNavigator path
    path = toroidalPath rInnerPath rOuterPath verticalWindingNumber 1

    rInnerPath = 75
    rOuterPath = 200
    verticalWindingNumber = 5

squareNavigator :: Animated Point -> Animated (Drawing ColorStyle)
squareNavigator path = layers <$> time <*> dot
  where
    dot = followPath path origin <*> pure staticDot
    staticDot = D.draw Foreground $ D.disc origin 15

    layers (Time t) d
        | isAbove t = D.union [frames, d]
        | otherwise = D.union [d, frames]

    isAbove t = even @Int . floor $ 8 * t

-- | In which dots fall among some rectangles
dots14 :: AnimatedPiece
dots14 = piece{viewFrame = originViewFrame, frameCount = 200}
  where
    piece = defaultAnimatedPiece $ squareNavigator path

    path =
        makeCircular (Time 1)
            . piecewiseLinear
            . pathProgram (Time 1)
            $ Point mark 0 :| [Point 0 mark, Point (negate mark) 0, Point 0 (negate mark), Point mark 0]

    mark = 160

-- | In which dots fall among some rectangles
dots15 :: AnimatedPiece
dots15 = piece{viewFrame = originViewFrame, frameCount = 200}
  where
    piece = defaultAnimatedPiece $ squareNavigator path

    path =
        makeCircular (Time 1)
            . piecewiseLinear
            . pathProgram (Time 1)
            $ Point 125 125
                :| [ Point 125 (-125)
                   , Point (-125) (-125)
                   , Point (-125) 125
                   , Point 125 125
                   ]

frames :: Drawing ColorStyle
frames = D.union [frameA, frameB, frameC, frameD]
  where
    frameA = translate (Vector 125 125) $ frame HighlightA
    frameB = translate (Vector (-125) 125) $ frame HighlightB
    frameC = translate (Vector 125 (-125)) $ frame HighlightB
    frameD = translate (Vector (-125) (-125)) $ frame HighlightA

    frame c = squareFrame 80 100 c

-- | In which dots are scaled by an ocillating field
dots16 :: AnimatedPiece
dots16 = piece{viewFrame = originViewFrame, palette = terracotta}
  where
    piece = defaultAnimatedPiece $ D.union <$> sequenceA dots
    dot c p = D.draw c $ D.disc p 5
    wobblingDot c p = scaleField (dots16Field 0.5) p <*> pure (dot c p)
    dots = makeGrid ll ur 10 10 $ \i j -> wobblingDot (getColor i j)
    ll = Point (-200) (-200)
    ur = Point 200 200

    getColor i j
        | (i + j) `mod` 3 == 0 = Foreground
        | (i + j) `mod` 3 == 1 = HighlightA
        | otherwise = HighlightB

dots16Field :: Double -> Animated (Field2D Double)
dots16Field a = mkField <$> time
  where
    mkField (Time t) = getScalar t <$> point
    getScalar t p =
        let v = pointToVector p
            s = sin . (2 * pi *) $ norm v / 250 + t
         in 1 + a * s
