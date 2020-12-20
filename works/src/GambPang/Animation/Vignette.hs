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

import Clay (Color, Css, FontFaceSrc (..), (?))
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
import qualified Data.Text.Lazy as TextL
import Lucid (HtmlT)
import qualified Lucid as L
import Lucid.Base (Html, commuteHtmlT, toHtml)

import GambPang.Animation.ColorStyle (ColorStyle (..), Palette)
import GambPang.Animation.Piece (AnimatedPiece, renderGif)
import qualified GambPang.Animation.Piece as P
import GambPang.Animation.Utils (dataUrl)
import GambPang.Files (heavyFontContent, lightFontContent)

-- TODO Fix font loading/styling
style :: (ColorStyle -> Color) -> Css
style palette = do
    C.body ? do
        C.paddingLeft $ C.rem 20
        C.background bg
        C.color fg
        C.fontFamily [lightFont] [C.serif]

    C.h1 ? do
        C.fontFamily [heavyFont] [C.serif]
        C.u ? colorUnderline hlB

    C.h2 ? do
        C.fontFamily [heavyFont] [C.serif]
        C.u ? colorUnderline hlA

    ".content" ? C.paddingLeft (C.rem 5)
    ".animation" ? C.img ? C.border C.solid (C.px 1) fg

    C.fontFace $ do
        C.fontFamily [lightFont] mempty
        C.fontFaceSrc [FontFaceSrcUrl lightFontUrl $ Just C.TrueType]
        C.fontWeight C.normal
        C.fontStyle C.normal

    C.fontFace $ do
        C.fontFamily [heavyFont] mempty
        C.fontFaceSrc [FontFaceSrcUrl heavyFontUrl $ Just C.TrueType]
        C.fontWeight C.normal
        C.fontStyle C.normal
  where
    bg = palette Background
    fg = palette Foreground
    hlA = palette HighlightA
    hlB = palette HighlightB

colorUnderline :: Color -> StyleM ()
colorUnderline ulColor = do
    C.textDecoration C.none
    C.borderBottom C.solid (C.px 3) ulColor

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
        L.style_
            . TextL.toStrict
            . C.render
            . style
            . toCssPalette
            $ palette
    L.body_ $ do
        heading . toHtml $ vignetteGreeting v
        L.p_ [L.class_ "content"] . toHtml $ vignetteMessage v
        subheading . toHtml $ vignetteTitle v
        animationBytes <- lift $ BSL.toStrict <$> renderGif animation
        L.div_ [L.class_ "animation content"] $ L.img_ [L.src_ $ dataUrl "image/gif" animationBytes]
  where
    palette = vignettePalette v
    animation = (vignetteAnimation v){P.palette = palette}

heading :: Applicative m => HtmlT m a -> HtmlT m a
heading = L.h1_ . u_

subheading :: Applicative m => HtmlT m a -> HtmlT m a
subheading = L.h2_ . u_

u_ :: Applicative m => HtmlT m a -> HtmlT m a
u_ = L.term "u"

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

lightFontUrl :: Text
lightFontUrl = dataUrl "font/ttf" lightFontContent

lightFont :: Text
lightFont = "light-font"

heavyFontUrl :: Text
heavyFontUrl = dataUrl "font/ttf" heavyFontContent

heavyFont :: Text
heavyFont = "heavy-font"
