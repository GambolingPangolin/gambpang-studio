{-# LANGUAGE TemplateHaskell #-}

module GambPang.Files (
    lightFontContent,
    heavyFontContent,
) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

lightFontContent :: ByteString
lightFontContent = $(embedFile "data/fonts/Amiri-Regular.ttf")

heavyFontContent :: ByteString
heavyFontContent = $(embedFile "data/fonts/AbrilFatface-Regular.ttf")
