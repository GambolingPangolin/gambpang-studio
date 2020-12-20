{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GambPang.Animation.ColorStyle (
    -- * Style types
    ColorStyle (..),
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

data ColorStyle = Background | Foreground | HighlightA | HighlightB
    deriving (Eq, Ord, Enum, Show)

data PaletteChoice = DefaultPalette | PaletteChoice (ColorStyle -> Colour Double)

newtype StyleException = UnknownPalette Text
    deriving (Eq, Show)

instance Exception StyleException

parsePalette :: Text -> Either StyleException PaletteChoice
parsePalette name = maybe unknown (pure . PaletteChoice) $ Map.lookup name palettes
  where
    unknown = Left $ UnknownPalette name

palettes :: Map Text (ColorStyle -> Colour Double)
palettes =
    Map.fromList
        [ ("mellow", mellow)
        , ("snowy", snowy)
        , ("verdant", verdant)
        ]

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
