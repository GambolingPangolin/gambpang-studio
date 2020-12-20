{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative (many, optional, (<**>))
import Control.Concurrent.Async (async, wait)
import Control.Exception (throwIO)
import Control.Monad (foldM, when, (>=>))
import qualified Data.ByteString.Lazy as BSL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import Lucid (renderToFile)
import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Opt
import System.FilePath ((<.>), (</>))

import qualified GambPang.Animation.Bars as Bars
import qualified GambPang.Animation.Boxes as Boxes
import GambPang.Animation.ColorStyle (PaletteChoice (..), palettes, parsePalette)
import qualified GambPang.Animation.Dots as Dots
import GambPang.Animation.Piece (AnimatedPiece, renderGif)
import qualified GambPang.Animation.Scrollers as Scr
import qualified GambPang.Animation.Snowflake as Snowflake
import GambPang.Animation.Vignette (Vignette, VignetteDescription, fromDescription)
import qualified GambPang.Animation.Vignette as V

data CliOptions = CliOptions
    { listWorks :: Bool
    , buildAnimations :: [Text]
    , palette :: Maybe Text
    , outputDir :: FilePath
    , vignettes :: Maybe FilePath
    }
    deriving (Eq, Show)

cliOptions :: ParserInfo CliOptions
cliOptions = Opt.info (opts <**> Opt.helper) $ Opt.progDesc "Build tool for procedural art"
  where
    opts =
        CliOptions
            <$> optList
            <*> many optAnimation
            <*> optPalette
            <*> optOutput
            <*> optVignette

    optList =
        Opt.switch $
            Opt.short 'l' <> Opt.long "list" <> Opt.help "List the available works"

    optAnimation =
        Opt.strOption $
            Opt.long "animation"
                <> Opt.short 'a'
                <> Opt.help "Name of animation"

    optPalette =
        optional . Opt.strOption $
            Opt.long "palette"
                <> Opt.short 'p'
                <> Opt.help "name of the palette"

    optOutput =
        Opt.strOption $
            Opt.long "output"
                <> Opt.short 'o'
                <> Opt.value "/tmp"
                <> Opt.help "Folder in which to put the output files"

    optVignette =
        optional . Opt.strOption $
            Opt.long "vignettes"
                <> Opt.help "A file containing a vignette manifest to render"

animationTask :: Map Text AnimatedPiece -> FilePath -> Text -> IO ()
animationTask as path name = maybe notFound onRecord $ Map.lookup name as
  where
    onRecord anim = putStrLn outPath >> either onRenderError (BSL.writeFile outPath) (renderGif anim)
    outPath = path </> Text.unpack name <.> "gif"
    notFound = TIO.putStrLn $ "Not found: " <> name
    onRenderError = TIO.putStrLn

vignetteTask :: FilePath -> Vignette -> IO ()
vignetteTask outPath v = do
    TIO.putStrLn $ "Rendering: " <> vignetteTitle v
    either
        onError
        (renderToFile outFile)
        (V.vignette v)
  where
    outFile = outPath </> Text.unpack (V.vignetteId v) <.> "html"
    onError = TIO.putStrLn

main :: IO ()
main = do
    cli <- Opt.execParser cliOptions

    paletteChoice <- maybe (pure DefaultPalette) (either throwIO pure . parsePalette) (palette cli)
    vignetteDefs <- fromMaybe [] <$> traverse getVignetteDefs (vignettes cli)

    let animations = mkAnimations paletteChoice
        outDir = outputDir cli

    when (listWorks cli) $ do
        putStrLn "Animations:"
        mapM_ TIO.putStrLn $ Map.keys animations

    forkMany $ animationTask animations outDir <$> buildAnimations cli
    forkMany $ vignetteTask outDir <$> vignetteDefs

forkMany :: Traversable t => t (IO b) -> IO ()
forkMany tasks = traverse async tasks >>= mapM_ wait

getVignetteDefs :: FilePath -> IO [Vignette]
getVignetteDefs = Y.decodeFileEither @[VignetteDescription] >=> either onDecodeFail onContent
  where
    onDecodeFail err = print err >> pure mempty
    onContent = foldM addVignette mempty
    addVignette vs desc =
        either (notFound vs) (pure . (: vs)) $
            fromDescription palettes animations desc
    notFound vs errMsg = vs <$ TIO.putStrLn errMsg
    animations = mkAnimations DefaultPalette

mkAnimations :: PaletteChoice -> Map Text AnimatedPiece
mkAnimations paletteChoice =
    Bars.animations paletteChoice
        <> Dots.animations paletteChoice
        <> Scr.animations paletteChoice
        <> Boxes.animations paletteChoice
        <> Snowflake.animations paletteChoice
