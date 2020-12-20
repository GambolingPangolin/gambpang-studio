{-# LANGUAGE OverloadedStrings #-}

module GambPang.Animation.Boxes where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import GambPang.Animation (
    Animated (..),
    Drawing,
    Point (..),
    Rigged (..),
    Time (..),
    Vector (..),
    followPath,
    makeCircular,
    negateV,
    norm,
    pathProgram,
    piecewiseLinear,
    rotateO,
    scale,
    shiftEarlier,
    translate,
 )
import qualified GambPang.Animation.Drawing as D

import GambPang.Animation.ColorStyle (ColorStyle (..), PaletteChoice)
import GambPang.Animation.Piece (AnimatedPiece, applyPaletteChoice)
import GambPang.Animation.Utils (defaultAnimatedPiece, rotating, union2)

animations :: PaletteChoice -> Map Text AnimatedPiece
animations paletteChoice =
    applyPaletteChoice paletteChoice . defaultAnimatedPiece
        <$> Map.fromList
            [ ("boxes-1", boxes1)
            , ("boxes-2", boxes2)
            , ("boxes-3", boxes3)
            , ("boxes-4", boxes4)
            , ("boxes-5", boxes5)
            ]

boxes1 :: Animated (Drawing ColorStyle)
boxes1 = center300 . defaultZoom $ union2 cornerElements <$> travelers
  where
    travelers =
        D.union
            <$> sequenceA
                [ mkTraveler ceLL ceLR
                , mkTraveler ceLR ceUR
                , mkTraveler ceUR ceUL
                , mkTraveler ceUL ceLL
                ]

    mkTraveler e1 e2 = followPath (travPath e1 e2) travCenter <*> pure traveler

    travPath e1 e2 =
        piecewiseLinear $ pathProgram (Time 1) (cornerElemPointA e1 :| [cornerElemPointB e2])

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
boxes2 :: Animated (Drawing ColorStyle)
boxes2 = center300 $ union2 <$> Animated rotator <*> pure backer
  where
    backer = D.draw HighlightA $ D.rectangle 300 300

    rotator (Time t) = D.draw HighlightB . D.polygon $ rotatorVertices t
    rotatorVertices t =
        scale 300 300
            <$> [ Point 0 t
                , Point t 1
                , Point 1 (1 - t)
                , Point (1 - t) 0
                ]

boxes3 :: Animated (Drawing ColorStyle)
boxes3 =
    center300 . defaultZoom $
        D.union
            <$> sequenceA
                [pure cornerElements, travA, travB, travC, travD]
  where
    travA = followPath path travCenter <*> pure traveler
    travB = shiftEarlier (Time 0.05) travA
    travC = shiftEarlier (Time 0.10) travA
    travD = shiftEarlier (Time 0.15) travA
    path =
        makeCircular (Time 1)
            . piecewiseLinear
            . pathProgram (Time 1)
            $ cornerElemCorner ceLL
                :| [ cornerElemCorner ceLR
                   , cornerElemCorner ceUR
                   , cornerElemCorner ceUL
                   , cornerElemCorner ceLL
                   ]

center300 :: Rigged a => a -> a
center300 = translate $ Vector 100 100

defaultZoom :: Rigged a => a -> a
defaultZoom = scale 10 10

boxes4 :: Animated (Drawing ColorStyle)
boxes4 = fastRegion $ annulus 0 200

boxes5 :: Animated (Drawing ColorStyle)
boxes5 = fastRegion $ annulus 100 225

annulus :: Ord a => a -> a -> a -> Bool
annulus innerR outerR z = innerR <= z && z <= outerR

fastRegion :: (Double -> Bool) -> Animated (Drawing ColorStyle)
fastRegion inRegion = D.union <$> traverse spinner spinCoords
  where
    spinCoords = [(i, j) | i <- [1 .. 20 :: Int], j <- [1 .. 20 :: Int]]
    spinner (i, j) = fmap (translate $ toVector i j) $ rotating (rate $ toVector i j) <*> pure sprite

    sprite = translate spriteCenterV . D.draw Foreground $ D.rectangle 15 15
    spriteCenterV = negateV $ Vector (15 / 2) (15 / 2)

    toVector i j = Vector (fromIntegral i * s) (fromIntegral j * s)
    s = 500 / 21

    rate v
        | inRegion $ adjNorm v = 2
        | otherwise = 1
    adjNorm v = norm $ v <> negateV (Vector 250 250)
