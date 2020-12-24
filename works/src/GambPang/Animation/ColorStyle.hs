{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GambPang.Animation.ColorStyle (
    -- * Style types
    ColorStyle (..),
    Palette,
    PaletteChoice (..),
    parsePalette,

    -- * Palettes
    palettes,
    californiacoast,
    cubs,
    france,
    mellow,
    nightlights,
    ozarks,
    redandblack,
    snowy,
    sunrise,
    verdant,
    vegetablegarden,
    terracotta,
) where

import Control.Exception (Exception)
import Data.Colour (Colour)
import qualified Data.Colour.Names as Names
import Data.Colour.SRGB (sRGB24read)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

data ColorStyle = Background | Foreground | HighlightA | HighlightB
    deriving (Eq, Ord, Enum, Show)

type Palette = ColorStyle -> Colour Double

data PaletteChoice = DefaultPalette | PaletteChoice Palette

newtype StyleException = UnknownPalette Text
    deriving (Eq, Show)

instance Exception StyleException

parsePalette :: Text -> Either StyleException PaletteChoice
parsePalette name = maybe unknown (pure . PaletteChoice) $ Map.lookup name palettes
  where
    unknown = Left $ UnknownPalette name

palettes :: Map Text Palette
palettes =
    Map.fromList
        [ ("californiacoast", californiacoast)
        , ("cubs", cubs)
        , ("france", france)
        , ("mellow", mellow)
        , ("nightlights", nightlights)
        , ("ozarks", ozarks)
        , ("redandblack", redandblack)
        , ("snowy", snowy)
        , ("sunrise", sunrise)
        , ("terracotta", terracotta)
        , ("vegetablegarden", vegetablegarden)
        , ("verdant", verdant)
        ]

californiacoast :: Palette
californiacoast = \case
    Background -> sRGB24read "#cb9967"
    Foreground -> sRGB24read "#48675a"
    HighlightA -> sRGB24read "#f0e4d3"
    HighlightB -> sRGB24read "#d0ebfe"

cubs :: Palette
cubs = \case
    Background -> Names.blue
    Foreground -> Names.red
    HighlightA -> Names.grey
    HighlightB -> Names.white

france :: Palette
france = \case
    Background -> Names.black
    Foreground -> Names.white
    HighlightA -> Names.blue
    HighlightB -> Names.red

mellow :: Palette
mellow = \case
    Background -> Names.beige
    Foreground -> Names.chocolate
    HighlightA -> Names.orange
    HighlightB -> Names.gold

nightlights :: Palette
nightlights = \case
    Background -> Names.black
    Foreground -> Names.blue
    HighlightA -> Names.yellow
    HighlightB -> Names.white

ozarks :: Palette
ozarks = \case
    Background -> sRGB24read "#303a21"
    Foreground -> sRGB24read "#bcbdb9"
    HighlightA -> sRGB24read "#9aa15b"
    HighlightB -> sRGB24read "#bcd1e7"

redandblack :: Palette
redandblack = \case
    Background -> Names.red
    Foreground -> Names.black
    HighlightA -> Names.darkred
    HighlightB -> Names.brown

snowy :: Palette
snowy = \case
    Background -> Names.navy
    Foreground -> Names.silver
    HighlightA -> Names.white
    HighlightB -> Names.cyan

sunrise :: Palette
sunrise = \case
    Background -> sRGB24read "#49383e"
    Foreground -> sRGB24read "#fe8a52"
    HighlightA -> sRGB24read "#fdd981"
    HighlightB -> sRGB24read "#fefcdf"

terracotta :: Palette
terracotta = \case
    Background -> sRGB24read "#7c3a20"
    Foreground -> sRGB24read "#59291b"
    HighlightA -> sRGB24read "#c57252"
    HighlightB -> sRGB24read "#e58666"

vegetablegarden :: Palette
vegetablegarden = \case
    Background -> Names.darkgreen
    Foreground -> Names.red
    HighlightA -> Names.orange
    HighlightB -> Names.yellow

verdant :: Palette
verdant = \case
    Background -> Names.darkgreen
    Foreground -> Names.sienna
    HighlightA -> Names.black
    HighlightB -> Names.green
