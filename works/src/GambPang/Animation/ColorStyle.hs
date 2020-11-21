{-# LANGUAGE LambdaCase #-}

module GambPang.Animation.ColorStyle (
    -- * Style types
    ColorStyle (..),
    PaletteChoice (..),
    parsePalette,

    -- * Palettes
    mellow,
    snowy,
    verdant,
) where

import Control.Exception (Exception)
import Data.Colour (Colour)
import qualified Data.Colour.Names as Names

data ColorStyle = Background | Foreground | HighlightA | HighlightB
    deriving (Eq, Ord, Enum, Show)

data PaletteChoice = DefaultPalette | PaletteChoice (ColorStyle -> Colour Double)

newtype StyleException = UnknownPalette String
    deriving (Eq, Show)

instance Exception StyleException

parsePalette :: String -> Either StyleException PaletteChoice
parsePalette name = fmap PaletteChoice $ case name of
    "mellow" -> pure mellow
    "snowy" -> pure snowy
    "verdant" -> pure verdant
    s -> Left $ UnknownPalette s

mellow :: ColorStyle -> Colour Double
mellow = \case
    Background -> Names.beige
    Foreground -> Names.chocolate
    HighlightA -> Names.orange
    HighlightB -> Names.yellow

snowy :: ColorStyle -> Colour Double
snowy = \case
    Background -> Names.navy
    Foreground -> Names.silver
    HighlightA -> Names.white
    HighlightB -> Names.cyan

verdant :: ColorStyle -> Colour Double
verdant = \case
    Background -> Names.darkgreen
    Foreground -> Names.sienna
    HighlightA -> Names.green
    HighlightB -> Names.black
