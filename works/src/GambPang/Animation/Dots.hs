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
    dots17,
    dots18,
    dots19,
) where

import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import GambPang.Animation (
    Animated,
    Grid (..),
    Path,
    Point (..),
    Vector (..),
    backwards,
    centeredRectangle,
    circularPath,
    compress,
    displace,
    followPath,
    fromWidthHeight,
    makeCircular,
    makeGrid,
    negateV,
    norm,
    normalize,
    origin,
    pathProgram,
    piecewiseLinear,
    point,
    pointToVector,
    rescale,
    rotateO,
    rotatingO,
    scale,
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
    desert,
    france,
    greenroom,
    lux,
    markets,
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
    originViewFrame,
    scaleField,
    translationField,
    unitBump,
    unitField,
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
            , ("dots-17", dots17)
            , ("dots-18", dots18)
            , ("dots-19", dots19)
            ]

-- | In which a dot orbits the center of the frame
dots1 :: AnimatedPiece
dots1 = defaultAnimatedPiece . translate v' $ rotatingO 1 <*> pure s0
  where
    d = D.draw Foreground $ D.disc origin 10
    s0 = translate v d
    v = Vector 0 175
    v' = Vector 250 250

-- | In which a large dot transits the boundary of a grid
dots2 :: AnimatedPiece
dots2 =
    defaultAnimatedPiece
        . rescale (centeredRectangle 500 500)
        $ D.composite [movingDot, pure field]
  where
    field = D.union . makeGrid $ Grid gridR 5 5 mkVertex
    mkVertex _ _ p = D.draw Foreground $ D.disc p 5
    gridR = centeredRectangle 400 400

    movingDot = followPath path origin <*> pure d

    d = D.draw HighlightA $ D.disc origin 20

    p0 = Point (-200) (-200)
    p1 = displace p0 $ Vector 400 0
    p2 = displace p1 $ Vector 0 400
    p3 = displace p2 $ Vector (-400) 0
    p4 = displace p3 $ Vector 0 (-400)

    path = piecewiseLinear $ pathProgram (p0 :| [p1, p2, p3, p4])

-- | In which dots travel a sinusoidal path
dots3 :: AnimatedPiece
dots3 =
    defaultAnimatedPiece
        . rescale thisFrame
        . fmap D.union
        $ traverse mkDot [0 .. 10]
  where
    mkDot i = shiftEarlier (i / 10) dot

    dot = followPath path origin <*> pure d
    path = makeCircular 1 $ sinPath <$> time
    sinPath t = Point (500 * t) (200 * sin (2 * pi * t))

    d = D.draw Foreground $ D.disc origin 10
    thisFrame = fromWidthHeight (Point 0 (-250)) 500 500

-- | In which a trio of dots slows and reverses
dots4 :: AnimatedPiece
dots4 =
    AnimatedPiece
        { source =
            AnimatedDrawing
                . rescale thisFrame
                . D.composite
                $ [ dotElement
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
    mkAngle t
        | t <= 0.5 = 2 * m * pi * progress (2 * t)
        | otherwise = negate $ 2 * m * progress (2 * t - 1)
    progress t = sin (pi * t)
    pCenter = Point 100 0
    m = 2
    thisFrame = centeredRectangle 500 500

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

    coilingDot = followPath path origin <*> pure coiler
    path = toroidalPath rInnerPath rOuterPath verticalWindingNumber 1
    isAboveAnnulus t = sin (2 * pi * verticalWindingNumber * t) > 0

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
toroidalPath r1 r2 n m = mkPath <$> time
  where
    mkPath t = toroidalProjection r1 r2 (a1 t, a2 t)
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

-- | In which dots move under a grating
dots6 :: AnimatedPiece
dots6 = piece{viewFrame = originViewFrame, frameCount = 200, framesPerSec = 20}
  where
    piece = defaultAnimatedPiece $ D.composite [thisGrating <$> hamster, pure dotMatrix]

    dotMatrix = D.union . makeGrid $ Grid gridR 7 7 topDot
    gridR = centeredRectangle 400 400

    thisGrating = grating gridR 7 7 (D.disc origin 20)
    hamster = followPath (circularPath 1 origin 150) origin <*> pure dot

    dot = D.draw Foreground $ D.disc origin 80
    topDot i j p = D.draw (getColor i j) $ D.disc p 10
    getColor i j
        | even (i + j) = HighlightA
        | otherwise = HighlightB

-- | In which dots are displaced by an ocillating field
dots7 :: AnimatedPiece
dots7 = piece{viewFrame = originViewFrame, palette = terracotta}
  where
    piece = defaultAnimatedPiece $ D.composite dots
    dot c p = D.draw c $ D.disc p 5
    wobblingDot c p = translationField (displacementField 7.5) p <*> pure (dot c p)
    dots = makeGrid . Grid gridR 10 10 $ \i j -> wobblingDot (getColor i j)
    gridR = centeredRectangle 400 400

    getColor i j
        | (i + j) `mod` 3 == 0 = Foreground
        | (i + j) `mod` 3 == 1 = HighlightA
        | otherwise = HighlightB

displacementField :: Double -> Animated (Field2D Vector)
displacementField a = mkField <$> time
  where
    mkField t = getVector t <$> point
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
    dot c i s t j = D.draw c $ D.disc (getPosition i j s t) 10
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
    ring i = rotatingO (getRate i) <*> pure (dots i)

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
            D.composite [pure innerFrame, animatedDots, pure outerFrame]

    innerFrame = squareFrame 50 75 HighlightA
    outerFrame = squareFrame 100 125 HighlightB

    animatedDots = D.composite [animatedDot, shiftLater 0.2 animatedDot]

    animatedDot = makeCircular 1 $ followPath path origin <*> pure dot
    dot = D.draw Foreground $ D.disc origin 15
    path = getPath <$> time

    getPath t =
        let r = getR t
            a = getA t
         in Point (r * cos a) (r * sin a)
    getR t = (* 225) . sin $ 2 * pi * t
    getA t
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
    piece = defaultAnimatedPiece $ D.composite dots

    dots = makeGrid . Grid gridR 10 10 $ \i j -> wobblingDot (getColor i j)
    gridR = centeredRectangle 400 400

    wobblingDot c p = translationField movingBumpField p <*> pure (dot c p)
    dot c p = D.draw c $ D.disc p 5

    getColor i j
        | (i + j) `mod` 3 == 0 = Foreground
        | (i + j) `mod` 3 == 1 = HighlightA
        | otherwise = HighlightB

    movingBumpField = followPath (circularPath 1 origin 165) origin <*> pure bumpField
    bumpField = mkBumpField <$> scale a unitBump <*> unitField
    mkBumpField x v = scale (d * x) v
    a = 100
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

    layers t d
        | isAbove t = D.union [quadFrames, d]
        | otherwise = D.union [d, quadFrames]

    isAbove t = even @Int . floor $ 8 * t

-- | In which dots fall among some rectangles
dots14 :: AnimatedPiece
dots14 = piece{viewFrame = originViewFrame, frameCount = 200}
  where
    piece = defaultAnimatedPiece $ squareNavigator path

    path =
        makeCircular 1
            . piecewiseLinear
            . pathProgram
            $ Point mark 0 :| [Point 0 mark, Point (negate mark) 0, Point 0 (negate mark), Point mark 0]

    mark = 160

-- | In which dots fall among some rectangles
dots15 :: AnimatedPiece
dots15 = piece{viewFrame = originViewFrame, frameCount = 200}
  where
    piece = defaultAnimatedPiece $ squareNavigator path

    path =
        makeCircular 1
            . piecewiseLinear
            . pathProgram
            $ Point 125 125
                :| [ Point 125 (-125)
                   , Point (-125) (-125)
                   , Point (-125) 125
                   , Point 125 125
                   ]

quadFrames :: Drawing ColorStyle
quadFrames = D.union [frameA, frameB, frameC, frameD]
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
    piece = defaultAnimatedPiece $ D.composite dots
    dot c p = D.draw c $ D.disc p 5
    wobblingDot c p = scaleField (dots16Field 0.5) p <*> pure (dot c p)
    dots = makeGrid . Grid gridR 10 10 $ \i j -> wobblingDot (getColor i j)
    gridR = centeredRectangle 400 400

    getColor i j
        | (i + j) `mod` 3 == 0 = Foreground
        | (i + j) `mod` 3 == 1 = HighlightA
        | otherwise = HighlightB

dots16Field :: Double -> Animated (Field2D Double)
dots16Field a = mkField <$> time
  where
    mkField t = getScalar t <$> point
    getScalar t p =
        let v = pointToVector p
            s = sin . (2 * pi *) $ norm v / 250 + t
         in 1 + a * s

-- | In which we pan along as a dot describes sinusoidal path
dots17 :: AnimatedPiece
dots17 = piece{viewFrame = originViewFrame, palette = desert}
  where
    piece = defaultAnimatedPiece $ D.composite ([pure leftBoundary, pure rightBoundary] <> dots)

    leftBoundary = translate (negateV $ Vector 250 250) . D.draw Foreground $ D.rectangle 50 500
    rightBoundary = translate (Vector 450 0) leftBoundary

    dA = dot HighlightA
    dB = dot HighlightB
    dots =
        [ shiftEarlier 0.5 dA
        , shiftEarlier 0.25 dB
        , dA
        , shiftLater 0.25 dB
        , shiftLater 0.5 dA
        , shiftLater 0.75 dB
        ]
    dot c = compress (4 / 3) $ followPath path origin <*> pure (staticDot c)
    staticDot c = D.draw c $ D.disc origin 20

    path = mkPath <$> time
    mkPath t = Point (a * sin (2 * pi * t)) (500 * (t - 0.5))
    a = 180

-- | In which dots scale according to a moving bump
dots18 :: AnimatedPiece
dots18 = piece{viewFrame = originViewFrame, palette = greenroom, frameCount = 200}
  where
    piece = defaultAnimatedPiece $ D.composite dots

    dots = makeGrid . Grid gridR 10 10 $ \i j -> wobblingDot (getColor i j)
    gridR = centeredRectangle 400 400

    wobblingDot c p = scaleField movingBumpField p <*> pure (dot c p)
    dot c p = D.draw c $ D.disc p 5

    getColor i j
        | (i + j) `mod` 3 == 0 = Foreground
        | (i + j) `mod` 3 == 1 = HighlightA
        | otherwise = HighlightB

    movingBumpField = followPath (circularPath 1 origin 165) origin <*> pure bumpField
    bumpField = mkScalar <$> scale a unitBump
    mkScalar x = 1 + d * x
    a = 100
    d = 1

-- | In which dots navigate some square layers
dots19 :: AnimatedPiece
dots19 = piece{palette = markets, frameCount = 200}
  where
    piece = defaultAnimatedPiece sortedItems
    frame = squareFrame 100 130

    sortedItems = D.union . fmap snd . sortOn fst <$> sequenceA items
    items = sortableDot : (pure <$> sortableFrames)
    sortableFrames = zip frameCenters frames

    frames = mkFrame <$> frameDefs
    mkFrame (v, c) = translate v $ frame c
    frameDefs = zip frameVs colors
    colors =
        [ Foreground
        , HighlightA
        , Foreground
        , HighlightA
        , Foreground
        , HighlightA
        ]

    frameVs = mkFrameV <$> frameCenters
    frameCenters = mkScalar <$> [0 :: Int .. 4]
    mkFrameV x = Vector x x

    mkScalar i = 140 + fromIntegral i * w
    w = (p2 - p1) / 4
    p1 = 140
    p2 = 360

    dot = followPath dotPath origin <*> pure staticDot
    sortableDot = mkDotEntry <$> time <*> dot
    mkDotEntry t d = (500 * t, d)
    staticDot = D.draw HighlightB $ D.disc origin 30

    dotPath =
        makeCircular 1
            . piecewiseLinear
            . pathProgram
            $ Point 0 0 :| [Point 500 500]
