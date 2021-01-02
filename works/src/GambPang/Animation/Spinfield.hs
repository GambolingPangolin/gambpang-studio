{-# LANGUAGE OverloadedStrings #-}

module GambPang.Animation.Spinfield (
    animations,
    spinfield1,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import GambPang.Animation (
    Point (Point),
    Vector (Vector),
    origin,
    pointToVector,
    rotatingO,
    translate,
 )
import qualified GambPang.Animation.Drawing as D

import GambPang.Animation.ColorStyle (ColorStyle (..), PaletteChoice, redandblack)
import GambPang.Animation.Piece (AnimatedPiece (palette), applyPaletteChoice)
import GambPang.Animation.Utils (defaultAnimatedPiece, makeGrid)

animations :: PaletteChoice -> Map Text AnimatedPiece
animations paletteChoice =
    applyPaletteChoice paletteChoice
        <$> Map.fromList
            [ ("spinfield-1", spinfield1)
            ]

-- | In which dots on a grid spin
spinfield1 :: AnimatedPiece
spinfield1 = piece{palette = redandblack}
  where
    piece = defaultAnimatedPiece $ D.composite (makeGrid ll ur 6 6 mkSpinner)
    ll = Point 50 50
    ur = Point 450 450

    mkSpinner i j
        | test i j = fastSpinner
        | otherwise = slowSpinner

    fastSpinner p = spinner p 2
    slowSpinner p = spinner p 1

    spinner p r = translate (pointToVector p) $ rotatingO r <*> pure staticSpinner

    staticSpinner =
        D.union
            [ translate (Vector 0 10) $ D.draw HighlightA smallDisc
            , translate (Vector 0 (-10)) $ D.draw HighlightB smallDisc
            , D.draw Foreground bigDisc
            ]

    smallDisc = D.disc origin 8
    bigDisc = D.disc origin 20

    test i j = sqL i j || sqU i j || sqR i j || sqD i j

    sqL i j = i <= 2 && j >= 3 && j <= 4
    sqU i j = i >= 3 && i <= 4 && j >= 5
    sqR i j = i >= 5 && j >= 3 && j <= 4
    sqD i j = i >= 3 && i <= 4 && j <= 2
