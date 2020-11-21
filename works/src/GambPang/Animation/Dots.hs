{-# LANGUAGE TypeApplications #-}

module GambPang.Animation.Dots (
    animations,
    dots1,
    dots2,
    dots3,
) where

import Codec.Picture (Image, PixelRGBA8 (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import GambPang.Animation (
    Animated (..),
    Point (..),
    Time (..),
    Vector (..),
    followPath,
    origin,
    pathProgram,
    piecewiseLinear,
    rotateO,
    shift,
    time,
    translate,
 )
import GambPang.Animation.Drawing (Drawing)
import qualified GambPang.Animation.Drawing as D

import GambPang.Animation.ColorStyle (
    ColorStyle (..),
    PaletteChoice,
    snowy,
 )
import GambPang.Animation.Utils (defaultRender, renderWithFrames, rotating)

animations :: PaletteChoice -> Map String [Image PixelRGBA8]
animations paletteChoice =
    Map.fromList
        [ ("dots-1", dotsRender dots1)
        , ("dots-2", dotsRender $ dots2 5)
        , ("dots-3", dotsRender dots3)
        , ("dots-4", renderWithFrames snowy paletteChoice 200 dots4)
        ]
  where
    dotsRender = defaultRender snowy paletteChoice

dots1 :: Animated (Drawing ColorStyle)
dots1 = translate v' $ rotating 1 <*> pure s0
  where
    d = D.draw Foreground $ D.disc origin 10
    s0 = translate v d
    v = Vector 0 175
    v' = Vector 250 250

dots2 :: Int -> Animated (Drawing ColorStyle)
dots2 n = translate v $ union2 <$> movingDot <*> pure field
  where
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

dots3 :: Animated (Drawing ColorStyle)
dots3 = translate v . fmap D.union $ traverse mkDot [0 .. 10]
  where
    mkDot i = shift (Time $ i / 10) dot
    dot = followPath sinPath p0 <*> pure d
    sinPath = Animated $ \(Time t) -> let s = toCircular t in Point (500 * s) (200 * sin (2 * pi * s))
    v = Vector 0 250
    d = D.draw Foreground $ D.disc origin 10
    p0 = origin
    toCircular = snd . properFraction @_ @Int

dots4 :: Animated (Drawing ColorStyle)
dots4 =
    translate v . D.union
        <$> sequenceA
            [ dotElement
            , rotateO (2 * pi / 3) dotElement
            , rotateO (4 * pi / 3) dotElement
            ]
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
