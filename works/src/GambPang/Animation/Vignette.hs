{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module GambPang.Animation.Vignette (
    Vignette (..),
    VignetteDescription,
    vignette,
    vignetteId,
    fromDescription,
) where

import Clay (
    Color,
    Css,
    (?),
 )
import qualified Clay as C
import Clay.Stylesheet (StyleM)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Crypto.Hash (SHA256 (..), hashWith)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.ByteArray (convert)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isAlphaNum)
import Data.Colour (Colour)
import Data.Colour.SRGB (
    RGB (..),
    toSRGB24,
 )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GambPang.Animation.ColorStyle (ColorStyle (..), Palette)
import GambPang.Animation.Piece (AnimatedPiece, renderGif)
import qualified GambPang.Animation.Piece as P
import GambPang.Animation.Utils (dataUrl)
import qualified Lucid as L
import Lucid.Base (Html, commuteHtmlT, toHtml)

style :: (ColorStyle -> Color) -> Css
style palette = do
    C.body ? do
        C.background bg
        C.color fg
    C.h1 ? colorUnderline hlA
    C.h2 ? colorUnderline hlB
    "animation" C.** C.img ? C.border C.solid (C.px 1) fg
  where
    bg = palette Background
    fg = palette Foreground
    hlA = palette HighlightA
    hlB = palette HighlightB

colorUnderline :: Color -> StyleM ()
colorUnderline ulColor = do
    C.textDecoration C.underline
    C.textDecorationColor ulColor

data VignetteDescription = VignetteDescription
    { vDescTitle :: Text
    , vDescGreeting :: Text
    , vDescMessage :: Text
    , vDescAnimationName :: Text
    , vDescPaletteName :: Text
    }

instance FromJSON VignetteDescription where
    parseJSON = withObject "VignetteDescription" $ \obj ->
        VignetteDescription
            <$> obj .: "title"
            <*> obj .: "greeting"
            <*> obj .: "message"
            <*> obj .: "animation"
            <*> obj .: "palette"

data Vignette = Vignette
    { vignetteTitle :: Text
    , vignetteGreeting :: Text
    , vignetteMessage :: Text
    , vignetteAnimation :: AnimatedPiece
    , vignettePalette :: Palette
    }

fromDescription ::
    Map Text Palette ->
    Map Text AnimatedPiece ->
    VignetteDescription ->
    Either Text Vignette
fromDescription palettes animations desc =
    mkVignette
        <$> lookupDetail "palette" (vDescPaletteName desc) palettes
        <*> lookupDetail "animation" (vDescAnimationName desc) animations
  where
    mkVignette p a =
        Vignette
            { vignetteTitle = vDescTitle desc
            , vignetteGreeting = vDescGreeting desc
            , vignetteMessage = vDescMessage desc
            , vignetteAnimation = a
            , vignettePalette = p
            }
    lookupDetail collection key = maybe (onLookupFail collection key) pure . Map.lookup key
    onLookupFail collection key = Left $ "Unable to find" <> collection <> ": " <> key

vignette :: Vignette -> Either Text (Html ())
vignette v = commuteHtmlT . L.html_ $ do
    L.head_ $ do
        L.title_ . toHtml $ vignetteTitle v
        L.style_ mempty
            . toHtml
            . C.render
            . style
            . toCssPalette
            $ palette
    L.body_ $ do
        L.h1_ . toHtml $ vignetteGreeting v
        L.p_ . toHtml $ vignetteMessage v
        L.h2_ . toHtml $ vignetteTitle v
        animationBytes <- lift $ BSL.toStrict <$> renderGif animation
        L.div_ [L.class_ "animation"] $ L.img_ [L.src_ $ dataUrl "image/gif" animationBytes]
  where
    palette = vignettePalette v
    animation = (vignetteAnimation v){P.palette = palette}

toCssPalette :: (a -> Colour Double) -> a -> Color
toCssPalette = (toCssColor .)

toCssColor :: Colour Double -> Color
toCssColor = (C.rgba <$> fI channelRed <*> fI channelGreen <*> fI channelBlue <*> pure 1) . toSRGB24
  where
    fI = (fromIntegral .)

-- | Calculate an approximate identifier.  This only covers the text part of the vignette.
vignetteId :: Vignette -> Text
vignetteId v = reduceCharSet (vignetteGreeting v) <> "-" <> sha256Text 4 content
  where
    reduceCharSet = Text.map toLimitedCharSet
    toLimitedCharSet c
        | isAlphaNum c = c
        | otherwise = '_'
    content = vignetteTitle v <> vignetteGreeting v <> vignetteMessage v

sha256Text :: Int -> Text -> Text
sha256Text n =
    Text.take (2 * n)
        . decodeUtf8
        . B16.encode
        . convert
        . hashWith SHA256
        . encodeUtf8
