{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GambPang.Animation.Vignette (
    Vignette (..),
    vignette,
    vignetteId,
    vignetteAnimFilePath,
    lightFontUrl,
    heavyFontUrl,
) where

import Clay (Color, Css, FontFaceSrc (..), (?))
import qualified Clay as C
import Clay.Stylesheet (StyleM)
import Crypto.Hash (SHA256 (..), hashWith)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.ByteArray (convert)
import qualified Data.ByteString.Base16 as B16
import Data.Char (isAlphaNum)
import Data.Colour (Colour, blend)
import Data.Colour.SRGB (
    RGB (..),
    toSRGB24,
 )
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TextL
import Lucid (HtmlT)
import qualified Lucid as L
import Lucid.Base (Html, commuteHtmlT, toHtml)

import qualified Data.Colour.Names as Names
import GambPang.Animation.ColorStyle (ColorStyle (..), Palette)

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

    ".content" ? do
        C.width (C.px 500)
        C.textAlign C.justify
        C.fontSize (C.pt 14)

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

data Vignette = Vignette
    { vignetteTitle :: Text
    , vignetteGreeting :: Text
    , vignetteMessage :: Text
    , vignetteAnimationName :: Text
    , vignettePaletteName :: Text
    }

instance FromJSON Vignette where
    parseJSON = withObject "Vignette" $ \obj ->
        Vignette
            <$> obj .: "title"
            <*> obj .: "greeting"
            <*> obj .: "message"
            <*> obj .: "animation"
            <*> obj .: "palette"

vignetteAnimFilePath :: Vignette -> Text
vignetteAnimFilePath v =
    "images/" <> vignetteAnimationName v <> "-" <> vignettePaletteName v <> ".gif"

vignette :: Palette -> Vignette -> Either Text (Html ())
vignette palette v = commuteHtmlT . L.html_ $ do
    L.head_ $ do
        L.title_ . toHtml $ vignetteTitle v
        L.style_
            . TextL.toStrict
            . C.render
            . style
            . toCssPalette
            $ makeLegible palette
    L.body_ $ do
        heading . toHtml $ vignetteGreeting v
        L.div_ [L.class_ "content"] $ do
            L.p_ . toHtml $ vignetteMessage v
            L.div_ [L.class_ "animation"] $ L.img_ [L.src_ $ vignetteAnimFilePath v]

heading :: Applicative m => HtmlT m a -> HtmlT m a
heading = L.h1_ . u_

u_ :: Applicative m => HtmlT m a -> HtmlT m a
u_ = L.term "u"

makeLegible :: Palette -> Palette
makeLegible p = \case
    Background -> blend 0.6 Names.white $ p Background
    Foreground -> blend 0.6 Names.black $ p Foreground
    x -> p x

toCssPalette :: (a -> Colour Double) -> a -> Color
toCssPalette = (toCssColor .)

toCssColor :: Colour Double -> Color
toCssColor =
    (C.rgba <$> fI channelRed <*> fI channelGreen <*> fI channelBlue <*> pure 1) . toSRGB24
  where
    fI = (fromIntegral .)

-- | Calculate an approximate identifier.  This only covers the text part of the vignette.
vignetteId :: Vignette -> Text
vignetteId v = reduceCharSet (vignetteTitle v) <> "-" <> sha256Text 4 content
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
lightFontUrl = "fonts/Amiri-Regular.ttf"

lightFont :: Text
lightFont = "light-font"

heavyFontUrl :: Text
heavyFontUrl = "fonts/AbrilFatface-Regular.ttf"

heavyFont :: Text
heavyFont = "heavy-font"
