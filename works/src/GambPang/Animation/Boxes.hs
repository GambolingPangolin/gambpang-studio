{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GambPang.Animation.Boxes (
    animations,
    boxes1,
    boxes2,
    boxes3,
    boxes4,
    boxes5,
    boxes6,
    boxes7,
    boxes8,
    boxes9,
    boxes10,
    boxes11,
    boxes12,
    boxes13,
    boxes14,
) where

import Control.Arrow (Arrow ((***)))
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import GambPang.Animation (
    Animated,
    Drawing,
    Point (..),
    Rigged (..),
    Time,
    Vector (..),
    cameraPan,
    circularPath,
    compress,
    followPath,
    makeCircular,
    negateV,
    norm,
    origin,
    pathProgram,
    piecewiseLinear,
    point,
    pointToVector,
    rotateO,
    rotating,
    rotatingO,
    scale,
    shiftEarlier,
    shiftLater,
    time,
    translate,
 )
import GambPang.Animation.Drawing (Shape)
import qualified GambPang.Animation.Drawing as D

import GambPang.Animation.ColorStyle (ColorStyle (..), PaletteChoice, cubs, markets, ozarks)
import GambPang.Animation.Piece (AnimatedPiece (..), applyPaletteChoice)
import GambPang.Animation.Utils (
    Pointed (..),
    defaultAnimatedPiece,
    grating,
    makeGrid,
    midpoint,
    originViewFrame,
    scaleField,
    translationField,
 )

animations :: PaletteChoice -> Map Text AnimatedPiece
animations paletteChoice =
    applyPaletteChoice paletteChoice
        <$> Map.fromList
            [ ("boxes-1", boxes1)
            , ("boxes-2", boxes2)
            , ("boxes-3", boxes3)
            , ("boxes-4", boxes4)
            , ("boxes-5", boxes5)
            , ("boxes-6", boxes6)
            , ("boxes-7", boxes7)
            , ("boxes-8", boxes8)
            , ("boxes-9", boxes9)
            , ("boxes-10", boxes10)
            , ("boxes-11", boxes11)
            , ("boxes-12", boxes12)
            , ("boxes-13", boxes13)
            , ("boxes-14", boxes14)
            ]

boxes1 :: AnimatedPiece
boxes1 =
    defaultAnimatedPiece
        . center300
        . defaultZoom
        $ D.composite [pure cornerElements, travelers]
  where
    travelers =
        D.composite
            [ mkTraveler ceLL ceLR
            , mkTraveler ceLR ceUR
            , mkTraveler ceUR ceUL
            , mkTraveler ceUL ceLL
            ]

    mkTraveler e1 e2 = followPath (travPath e1 e2) travCenter <*> pure traveler

    travPath e1 e2 =
        piecewiseLinear $ pathProgram 1 (cornerElemPointA e1 :| [cornerElemPointB e2])

cornerElements :: Drawing ColorStyle
cornerElements = D.union $ cornerElemDrawing <$> [ceLL, ceLR, ceUR, ceUL]

ceLL :: CornerElement
ceLL = cornerElement

ceLR :: CornerElement
ceLR = translate vLR . rotateO (pi / 2) $ cornerElement
  where
    vLR = Vector 30 0

ceUR :: CornerElement
ceUR = translate vUR . rotateO pi $ cornerElement
  where
    vUR = Vector 30 30

ceUL :: CornerElement
ceUL = translate vUL . rotateO (3 * pi / 2) $ cornerElement
  where
    vUL = Vector 0 30

traveler :: Drawing ColorStyle
traveler = D.draw HighlightA $ D.rectangle 1 1

travCenter :: Point
travCenter = Point 0.5 0.5

data CornerElement = CornerElement
    { cornerElemDrawing :: Drawing ColorStyle
    , cornerElemPointA :: Point
    , cornerElemPointB :: Point
    , cornerElemCorner :: Point
    }
    deriving (Eq, Show)

instance Rigged CornerElement where
    transform t ce =
        CornerElement
            { cornerElemDrawing = transform t $ cornerElemDrawing ce
            , cornerElemPointA = transform t $ cornerElemPointA ce
            , cornerElemPointB = transform t $ cornerElemPointB ce
            , cornerElemCorner = transform t $ cornerElemCorner ce
            }

cornerElement :: CornerElement
cornerElement =
    CornerElement
        { cornerElemDrawing =
            D.union
                [ darkRect w h
                , translate vU end
                , darkRect h w
                , translate vR end
                ]
        , cornerElemPointA = Point (h + 0.5) (0.5 * w)
        , cornerElemPointB = Point (0.5 * w) (h + 0.5)
        , cornerElemCorner = Point 0.5 0.5
        }
  where
    w = 1
    h = 5
    vU = Vector h 0
    vR = Vector 0 h
    end = D.draw HighlightB $ D.rectangle 1 1

darkRect :: Double -> Double -> Drawing ColorStyle
darkRect w h = D.draw Foreground $ D.rectangle w h

-- | Nested boxes where the inner box rotates
boxes2 :: AnimatedPiece
boxes2 = defaultAnimatedPiece . center300 $ D.composite [rotator, pure backer]
  where
    backer = D.draw HighlightA $ D.rectangle 300 300

    rotator = D.draw HighlightB . D.polygon . rotatorVertices <$> time
    rotatorVertices t =
        scale 300
            <$> [ Point 0 t
                , Point t 1
                , Point 1 (1 - t)
                , Point (1 - t) 0
                ]

boxes3 :: AnimatedPiece
boxes3 =
    defaultAnimatedPiece
        . center300
        . defaultZoom
        . D.composite
        $ [pure cornerElements, travA, travB, travC, travD]
  where
    travA = followPath path travCenter <*> pure traveler
    travB = shiftEarlier 0.05 travA
    travC = shiftEarlier 0.10 travA
    travD = shiftEarlier 0.15 travA
    path =
        makeCircular 1
            . piecewiseLinear
            . pathProgram 1
            $ cornerElemCorner ceLL
                :| [ cornerElemCorner ceLR
                   , cornerElemCorner ceUR
                   , cornerElemCorner ceUL
                   , cornerElemCorner ceLL
                   ]

center300 :: Rigged a => a -> a
center300 = translate $ Vector 100 100

defaultZoom :: Rigged a => a -> a
defaultZoom = scale 10

-- | In which a disc spins quickly
boxes4 :: AnimatedPiece
boxes4 = defaultAnimatedPiece . fastRegion $ annulus 0 200

-- | In which an annulus spins quickly
boxes5 :: AnimatedPiece
boxes5 = defaultAnimatedPiece . fastRegion $ annulus 100 225

annulus :: Ord a => a -> a -> a -> Bool
annulus innerR outerR z = innerR <= z && z <= outerR

fastRegion :: (Double -> Bool) -> Animated (Drawing ColorStyle)
fastRegion inRegion = D.union <$> traverse spinner spinCoords
  where
    spinCoords = [(i, j) | i <- [1 .. 20 :: Int], j <- [1 .. 20 :: Int]]
    spinner (i, j) =
        fmap (translate $ toVector i j) $ rotatingO (rate $ toVector i j) <*> pure (sprite i j)

    sprite i j = translate spriteCenterV . D.draw (getColor i j) $ D.rectangle 15 15
    spriteCenterV = negateV $ Vector (15 / 2) (15 / 2)

    getColor i j
        | even i && even j = HighlightB
        | even i || even j = HighlightA
        | otherwise = Foreground

    toVector i j = Vector (fromIntegral i * s) (fromIntegral j * s)
    s = 500 / 21

    rate v
        | inRegion $ adjNorm v = 2
        | otherwise = 1
    adjNorm v = norm $ v <> negateV (Vector 250 250)

-- | In which boxes migrate from the upper left to the lower right, first growing then shrinking
boxes6 :: AnimatedPiece
boxes6 =
    piece
        { viewFrame = originViewFrame
        , palette = cubs
        , framesPerSec = 35
        , frameCount = 200
        }
  where
    piece = defaultAnimatedPiece . shiftLater 1 . D.composite $ travelers

    travelerPair =
        [ travelingBox Foreground
        , shiftEarlier offset $ travelingBox HighlightA
        ]
    travelers = take (4 * travelerHalfCount + 1) $ do
        i <- [0 ..]
        shiftEarlier (2 * i * offset) <$> travelerPair

    travelerHalfCount = 6
    offset = 1 / (2 * fromIntegral travelerHalfCount)

    travelingBox color = followPath path origin <*> box color

    sideSmall = 20
    maxFactor = 15

    startingPosition = Point (-225) 225
    endingPosition = Point 225 (-225)

    box color = scaling <*> (pure . translate v0 . D.draw color) (D.rectangle sideSmall sideSmall)
    v0 = negateV $ Vector (0.5 * sideSmall) (0.5 * sideSmall)

    path = piecewiseLinear . pathProgram duration $ startingPosition :| [endingPosition]
    scaling = scale . mkScale <$> time

    mkScale t
        | t <= 0 = 1
        | t <= 0.5 = interp (2 * t) 1 maxFactor
        | t <= 1 = interp (2 * t - 1) maxFactor 1
        | otherwise = 1

interp :: Num a => a -> a -> a -> a
interp u a b = (1 - u) * a + u * b

duration :: Time
duration = 1

-- | In which we scroll along a spinning-box field
boxes7 :: AnimatedPiece
boxes7 =
    piece
        { palette = ozarks
        , framesPerSec = 35
        , frameCount = 200
        }
  where
    piece = defaultAnimatedPiece . cameraPan cameraPath $ D.union <$> spinners

    spinners = cameraPath >>= traverse getSpinner . spinnerSet
    spinnerSet (Point x y) = [(i, j) | i <- range x, j <- range y]

    getSpinner (i, j) = getSpinnerCtor i j $ getSpinnerCenter i j
    getSpinnerCtor i j
        | i `mod` 5 == 0 && j `mod` 5 == 0 = spinner 3 HighlightA
        | i `mod` 5 == 0 || j `mod` 5 == 0 = spinner 2 HighlightB
        | otherwise = spinner 1 Foreground
    getSpinnerCenter i j = Point (30 * fromIntegral i) (30 * fromIntegral j)

    -- 500 / 30
    n = 16 :: Int
    range z = let i0 = floor (z / 30) in [i0 - 1 .. i0 + n + 1]

    spinner r c p = translate (pointToVector p) $ rotatingO r <*> pure (box c)
    box c = translate v . D.draw c $ D.rectangle 15 15
    v = negateV $ Vector (15 / 2) (15 / 2)

    cameraPath =
        piecewiseLinear . pathProgram duration $
            origin :| [Point (10 * 30) (5 * 30)]

-- | In which we see a dot under a grating of squares
boxes8 :: AnimatedPiece
boxes8 = piece{viewFrame = originViewFrame}
  where
    piece = defaultAnimatedPiece $ thisGrating <$> runners
    thisGrating = grating ll ur 7 7 rect
    rect = translate (negateV $ Vector 20 20) $ D.rectangle 40 40

    ll = Point (-200) (-200)
    ur = Point 200 200

    runners =
        D.composite
            [ runner Foreground
            , shiftLater 0.33 $ runner HighlightA
            , shiftLater 0.66 $ runner HighlightB
            ]

    runner c = followPath path origin <*> (rotatingO 1 <*> pure (staticRunner c))
    path = circularPath 1 origin 150
    staticRunner c =
        D.union
            [ D.draw c $ D.disc origin 80
            ]

-- | In which a dot travels over boxes where some of the boxes are under and some are over
boxes9 :: AnimatedPiece
boxes9 = piece{frameCount = 200}
  where
    piece = defaultAnimatedPiece $ D.composite (hiBoxes <> dots <> loBoxes)
    (hiBoxes, loBoxes) = (toBoxes *** toBoxes) . partition (even . fst) $ zip [1 :: Int ..] boxes
    toBoxes = fmap (pure . snd)
    boxes = makeGrid ll ur 10 10 $ \_ _ -> box
    box p =
        translate (pointToVector p)
            . translate (negateV $ Vector 15 15)
            . D.draw Foreground
            $ D.rectangle 30 30

    ll = Point 50 50
    ur = Point 450 450

    dots =
        [ dot HighlightA
        , shiftLater 0.1 (dot HighlightB)
        ]

    dot c = followPath path origin <*> pure (staticDot c)
    staticDot c = D.draw c $ D.disc origin 10
    path =
        makeCircular 1
            . piecewiseLinear
            . pathProgram 1
            . fmap (translate $ Vector 25 25)
            $ Point 0 100 :| [Point 100 450, Point 450 350, Point 350 0, Point 0 100]

-- | In which boxes on a grid flow accourding to a vertical sine wave
boxes10 :: AnimatedPiece
boxes10 = defaultAnimatedPiece $ D.composite boxes
  where
    mobileBox c p = translationField hWaves p <*> pure (box c p)

    hWaves = hWaveField <$> time
    hWaveField t = hWaveValue t <$> point
    hWaveValue t (Point _ y) = let v = a * sin (y / 400 + 2 * pi * t) in Vector v (negate v)
    a = 15

    boxes = makeGrid ll ur 10 10 $ \i j -> mobileBox (getColor i j)
    box c p =
        translate (pointToVector p)
            . translate (negateV $ Vector 15 15)
            . D.draw c
            $ D.rectangle 30 30
    getColor i j
        | (i + j) `mod` 3 == 0 = Foreground
        | (i + j) `mod` 3 == 1 = HighlightA
        | otherwise = HighlightB

    ll = Point 50 50
    ur = Point 450 450

-- | In which boxes on a grid flow accourding to a vertical sine wave
boxes11 :: AnimatedPiece
boxes11 = defaultAnimatedPiece $ D.composite boxes
  where
    mobileBox c p = scaleField hWaves p <*> pure (box c p)

    hWaves = hWaveField <$> time
    hWaveField t = hWaveValue t <$> point
    hWaveValue t (Point _ y) = a * sin (y / 400 + 2 * pi * t)
    a = 15

    boxes = makeGrid ll ur 10 10 $ \i j -> mobileBox (getColor i j)
    box c p =
        translate (pointToVector p)
            . translate (negateV $ Vector 15 15)
            . D.draw c
            $ D.rectangle 30 30
    getColor i j
        | (i + j) `mod` 3 == 0 = Foreground
        | (i + j) `mod` 3 == 1 = HighlightA
        | otherwise = HighlightB

    ll = Point 50 50
    ur = Point 450 450

rectP :: Double -> Double -> Pointed Shape
rectP w h =
    Pointed
        { basePoint = origin
        , object = translate (negateV $ Vector (w / 2) (h / 2)) $ D.rectangle w h
        }

swapPointed :: Rigged a => Pointed a -> Pointed a -> Animated (Pointed a, Pointed a)
swapPointed p1 p2 = rotating mp 0.5 <*> pure (p1, p2)
  where
    mp = midpoint (basePoint p1) (basePoint p2)

data FlipBox = FlipBox
    { index :: (Int, Int)
    , flipBox :: Pointed (Drawing ColorStyle)
    }

instance Rigged FlipBox where
    transform a (FlipBox ix p) = FlipBox ix $ transform a p

-- | In which a special box moves around
boxes12 :: AnimatedPiece
boxes12 = piece{palette = markets, frameCount = 200}
  where
    piece = defaultAnimatedPiece $ swapSet locs
    locs = bottom <> right <> top <> left
    bottom = (2,) <$> [2 .. 9]
    right = (,9) <$> [3 .. 9]
    top = reverse $ (9,) <$> [2 .. 8]
    left = reverse $ (,2) <$> [3 .. 8]

boxes13 :: AnimatedPiece
boxes13 = piece{palette = markets, frameCount = 200}
  where
    piece = defaultAnimatedPiece $ swapSet locs
    locs = [(2, 2), (2, 9), (9, 9), (9, 2), (4, 4), (4, 7), (7, 7), (7, 4)]

boxes14 :: AnimatedPiece
boxes14 = piece{palette = markets, frameCount = 500}
  where
    piece = defaultAnimatedPiece $ swapSet locs
    locs = row 3 <> row 4 <> row 5 <> row 6 <> row 7 <> row 8
    row i = adjust i $ (i,) <$> [3 .. 8]
    adjust i
        | even i = reverse
        | otherwise = id

swapSet :: [(Int, Int)] -> Animated (Drawing ColorStyle)
swapSet swaps = D.composite steps
  where
    boxes pos1 pos2 = compress (fromIntegral cFactor) . crop . applyFlip pos1 pos2 . makeGrid ll ur 10 10 $ mkBox pos1

    cFactor = length swaps

    swapPositions = zip swaps $ drop 1 (cycle swaps)

    steps = zipWith shiftLater timeOffsets $ uncurry boxes <$> swapPositions

    timeOffsets = (/ fromIntegral cFactor) . fromIntegral <$> [0 .. cFactor]

    crop a = runCrop <$> time <*> a
    runCrop t d
        | t >= 0 && t <= 1 = d
        | otherwise = D.union mempty

    mkBox pos i j p =
        FlipBox
            { index = (i, j)
            , flipBox = translate (pointToVector p) $ D.draw (getColor pos i j) <$> rectP 20 20
            }

    getColor (i0, j0) i j
        | i == i0 && j == j0 = HighlightA
        | otherwise = Foreground

    ll = Point 50 50
    ur = Point 450 450

applyFlip :: (Int, Int) -> (Int, Int) -> [FlipBox] -> Animated (Drawing ColorStyle)
applyFlip pos1 pos2 bs = combine <$> swapPointed (flipBox b1) (flipBox b2)
  where
    ([b1], bs1) = partition ((== pos1) . index) bs
    ([b2], bs2) = partition ((== pos2) . index) bs1

    combine (p1, p2) = D.union $ object p1 : object p2 : (object . flipBox <$> bs2)
