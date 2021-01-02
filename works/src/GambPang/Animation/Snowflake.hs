{-# LANGUAGE OverloadedStrings #-}

module GambPang.Animation.Snowflake (
    animations,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Endo (..))
import Data.Text (Text)
import GambPang.Animation (
    Animated,
    Drawing,
    PiecewiseLinearPath (..),
    Point (..),
    Time,
    Vector (..),
    followPath,
    makeCircular,
    origin,
    piecewiseLinear,
    reflect,
    rotateO,
    rotatingO,
    scale,
    shiftLater,
    translate,
 )
import GambPang.Animation.Drawing (Shape)
import qualified GambPang.Animation.Drawing as D

import GambPang.Animation.ColorStyle (ColorStyle (..), PaletteChoice, snowy)
import GambPang.Animation.Piece (AnimatedPiece (palette), applyPaletteChoice)
import GambPang.Animation.Utils (translations)
import qualified GambPang.Animation.Utils as U

animations :: PaletteChoice -> Map Text AnimatedPiece
animations paletteChoice =
    applyPaletteChoice paletteChoice . defaultAnimatedPiece
        <$> Map.fromList
            [ ("snowflake-1", snowflake1)
            , ("snowfield-1", snowfield1)
            ]

snowflakeMask :: Drawing color -> Drawing color
snowflakeMask = D.mask firstSextant
  where
    firstSextant =
        D.polygon
            [ Point 0 0
            , Point (cos theta) (sin theta)
            , Point 1 0
            ]
    theta = 2 * pi / 6

toSnowflake :: Drawing color -> Drawing color
toSnowflake d = D.union $ mkSnowflake <$> [0 .. 2 :: Int]
  where
    mkSnowflake i = rotateO (2 * fromIntegral i * pi / 3) snowflakeThird
    snowflakeThird = D.union [maskedDrawing, reflect (Vector 0 1) origin maskedDrawing]
    maskedDrawing = snowflakeMask d

animatedSnowflake :: Foldable t => t Shape -> Animated (Drawing ColorStyle)
animatedSnowflake cuts = scaleAndCenter <$> (rotatingO 1 <*> pure snowflake)
  where
    snowflake = toSnowflake . applyCuts . D.draw Foreground $ D.disc origin 1
    applyCuts = appEndo $ foldMap (Endo . D.exclude) cuts
    scaleAndCenter = translate v . scale 150
    v = Vector 250 250

snowflake1 :: Animated (Drawing ColorStyle)
snowflake1 =
    animatedSnowflake $
        [rotateO (pi / 6) r]
            <> lineA
            <> (rotateO <$> [a, 2 * a, 3 * a] <*> lineA)
            <> (rotateO <$> [a / 2, 3 * a / 2, 5 * a / 2] <*> lineB)
  where
    r = D.rectangle 0.1 0.1
    r' = rotateO (pi / 4) r
    a = 2 * pi / 18

    lineA =
        translations
            r'
            [ Vector 0.3 0
            , Vector 0.6 0
            , Vector 0.9 0
            ]
    lineB =
        translations
            (scale 0.5 r')
            [Vector 0.15 0, Vector 0.45 0, Vector 0.75 0]

snowfall ::
    -- | Radius
    Double ->
    -- | Start time
    Time ->
    -- | Duration
    Time ->
    Animated (Drawing ColorStyle)
snowfall r s d = followPath fallPath origin <*> pure flake
  where
    flake = D.draw HighlightA $ D.disc origin r
    fallPath = makeCircular 1 . piecewiseLinear $ PiecewiseLinearPath p0 segments
    segments = [(s, p0), (s + d, p1)]
    p0 = Point 0 $ 500 + r
    p1 = Point 0 $ negate r

snowfield1 :: Animated (Drawing ColorStyle)
snowfield1 = D.composite falls
  where
    falls =
        [ mkFall 0.1 50 fallB
        , mkFall 0.8 90 fallB
        , mkFall 0 100 fallA
        , mkFall 0.2 130 fallB
        , mkFall 0.3 150 fallA
        , mkFall 0.25 210 fallB
        , mkFall 0.5 250 fallB
        , mkFall 0.7 350 fallA
        , mkFall 0.9 375 fallB
        , mkFall 0.8 400 fallA
        , mkFall 0.1 425 fallA
        , mkFall 0.6 475 fallB
        ]
    fallA = snowfall 5 0 0.5
    fallB = snowfall 2 0 0.75

    mkFall t x = shiftLater t . translateX x

    translateX x = translate (Vector x 0)

defaultAnimatedPiece :: Animated (Drawing ColorStyle) -> AnimatedPiece
defaultAnimatedPiece d = (U.defaultAnimatedPiece d){palette = snowy}
