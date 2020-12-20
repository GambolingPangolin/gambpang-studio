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
    mellow,
    snowy,
    verdant,
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
        [ ("mellow", mellow)
        , ("nightlights", nightlights)
        , ("snowy", snowy)
        , ("sunrise", sunrise)
        , ("verdant", verdant)
        ]

mellow :: Palette
mellow = \case
    Background -> Names.beige
    Foreground -> Names.chocolate
    HighlightA -> Names.orange
    HighlightB -> Names.gold

nightlights :: Palette
nightlights = \case
    Background -> Names.black
    Foreground -> Names.navy
    HighlightA -> Names.yellow
    HighlightB -> Names.white

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

verdant :: Palette
verdant = \case
    Background -> Names.darkgreen
    Foreground -> Names.sienna
    HighlightA -> Names.black
    HighlightB -> Names.green
