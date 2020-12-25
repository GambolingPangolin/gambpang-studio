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
    calico,
    californiacoast,
    caribbean,
    cubs,
    desert,
    france,
    greyscale,
    greenroom,
    lux,
    markets,
    mellow,
    nightlights,
    ozarks,
    poland,
    primary,
    redandblack,
    royal,
    snowy,
    sunrise,
    sunset,
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
        [ ("calico", calico)
        , ("californiacoast", californiacoast)
        , ("caribbean", caribbean)
        , ("cubs", cubs)
        , ("desert", desert)
        , ("france", france)
        , ("greyscale", greyscale)
        , ("greenroom", greenroom)
        , ("lux", lux)
        , ("markets", markets)
        , ("mellow", mellow)
        , ("nightlights", nightlights)
        , ("ozarks", ozarks)
        , ("poland", poland)
        , ("primary", primary)
        , ("redandblack", redandblack)
        , ("royal", royal)
        , ("snowy", snowy)
        , ("sunrise", sunrise)
        , ("sunset", sunset)
        , ("terracotta", terracotta)
        , ("vegetablegarden", vegetablegarden)
        , ("verdant", verdant)
        ]

calico :: Palette
calico = \case
    Background -> Names.white
    Foreground -> Names.orange
    HighlightA -> Names.silver
    HighlightB -> Names.black

californiacoast :: Palette
californiacoast = \case
    Background -> sRGB24read "#cb9967"
    Foreground -> sRGB24read "#48675a"
    HighlightA -> sRGB24read "#f0e4d3"
    HighlightB -> sRGB24read "#d0ebfe"

caribbean :: Palette
caribbean = \case
    Background -> sRGB24read "#02dcff"
    Foreground -> sRGB24read "#41393a"
    HighlightA -> sRGB24read "#dbe91f"
    HighlightB -> sRGB24read "#32620e"

desert :: Palette
desert = \case
    Background -> sRGB24read "#fea456"
    Foreground -> sRGB24read "#8c6b28"
    HighlightA -> sRGB24read "#e8a884"
    HighlightB -> sRGB24read "#c8d9e0"

cubs :: Palette
cubs = \case
    Background -> Names.white
    Foreground -> Names.blue
    HighlightA -> Names.red
    HighlightB -> Names.grey

france :: Palette
france = \case
    Background -> Names.black
    Foreground -> Names.white
    HighlightA -> Names.blue
    HighlightB -> Names.red

greyscale :: Palette
greyscale = \case
    Background -> Names.white
    Foreground -> Names.lightgrey
    HighlightA -> Names.darkgrey
    HighlightB -> Names.black

greenroom :: Palette
greenroom = \case
    Background -> Names.green
    Foreground -> Names.beige
    HighlightA -> Names.yellow
    HighlightB -> Names.white

lux :: Palette
lux = \case
    Background -> Names.navy
    Foreground -> Names.gold
    HighlightA -> Names.silver
    HighlightB -> Names.white

markets :: Palette
markets = \case
    Background -> sRGB24read "#ececec"
    Foreground -> sRGB24read "#323232"
    HighlightA -> sRGB24read "#dd7168"
    HighlightB -> sRGB24read "#f8f8f8"

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

poland :: Palette
poland = \case
    Background -> sRGB24read "#dc143c"
    Foreground -> Names.white
    HighlightA -> Names.black
    HighlightB -> Names.grey

primary :: Palette
primary = \case
    Background -> Names.white
    Foreground -> Names.blue
    HighlightA -> Names.red
    HighlightB -> Names.yellow

redandblack :: Palette
redandblack = \case
    Background -> Names.red
    Foreground -> Names.black
    HighlightA -> Names.darkred
    HighlightB -> Names.brown

royal :: Palette
royal = \case
    Background -> Names.purple
    Foreground -> Names.silver
    HighlightA -> Names.yellow
    HighlightB -> Names.gold

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

sunset :: Palette
sunset = \case
    Background -> sRGB24read "#571400"
    Foreground -> sRGB24read "#e9750e"
    HighlightA -> sRGB24read "#ffb42a"
    HighlightB -> sRGB24read "#fff78a"

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
