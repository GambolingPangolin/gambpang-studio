{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative (many, optional, (<**>))
import Control.Concurrent.Async (async, wait)
import Control.Exception (throwIO)
import Control.Monad (foldM, unless, when, (>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (except, runExceptT)
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
import System.Directory (doesFileExist)
import qualified System.Directory as D
import System.FilePath ((<.>), (</>))

import qualified GambPang.Animation.Bars as Bars
import qualified GambPang.Animation.Boxes as Boxes
import GambPang.Animation.ColorStyle (Palette, PaletteChoice (..), palettes, parsePalette)
import qualified GambPang.Animation.Dots as Dots
import GambPang.Animation.Piece (AnimatedPiece, renderGif)
import qualified GambPang.Animation.Piece as P
import qualified GambPang.Animation.Scrollers as Scr
import qualified GambPang.Animation.Snowflake as Snowflake
import qualified GambPang.Animation.Spinfield as Spinfield
import GambPang.Animation.Vignette (Vignette, heavyFontUrl, lightFontUrl)
import qualified GambPang.Animation.Vignette as V
import Paths_gambpang_studio_works (getDataFileName)

data CliOptions = CliOptions
    { listWorks :: Bool
    , buildAnimations :: [Text]
    , cliPalette :: Maybe Text
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

handleVignettes :: FilePath -> [(Vignette, Palette, AnimatedPiece)] -> IO ()
handleVignettes outDir vs = do
    mapM_ mkDir ["fonts", "images"]
    mapM_ (copyFile outDir) [lightFontUrl, heavyFontUrl]
    forkMany $ vignetteTask outDir <$> vs
  where
    mkDir dir = D.createDirectoryIfMissing True $ outDir </> dir

vignetteTask :: FilePath -> (Vignette, Palette, AnimatedPiece) -> IO ()
vignetteTask outPath (v, palette, animation) = do
    TIO.putStrLn $ "Rendering: " <> V.vignetteTitle v
    either onError pure =<< runExceptT tasks
  where
    outFile = outPath </> Text.unpack (V.vignetteId v) <.> "html"
    animPath = outPath </> Text.unpack (V.vignetteAnimFilePath v)
    tasks = do
        haveAnim <- liftIO $ doesFileExist animPath
        unless haveAnim $ liftIO . BSL.writeFile animPath =<< except (renderGif animation)
        liftIO . renderToFile outFile =<< except (V.vignette palette v)
    onError = TIO.putStrLn

main :: IO ()
main = do
    cli <- Opt.execParser cliOptions

    paletteChoice <- maybe (pure DefaultPalette) (either throwIO pure . parsePalette) (cliPalette cli)
    vignetteDefs <- fromMaybe [] <$> traverse getVignetteDefs (vignettes cli)

    let animations = mkAnimations paletteChoice
        outDir = outputDir cli

    when (listWorks cli) $ do
        putStrLn "Animations:"
        mapM_ TIO.putStrLn $ Map.keys animations

    animAsyncs <- async . forkMany $ animationTask animations outDir <$> buildAnimations cli
    vignetteAsyncs <- async $ handleVignettes outDir vignetteDefs
    mapM_ wait [animAsyncs, vignetteAsyncs]

copyFile :: FilePath -> Text -> IO ()
copyFile outDir path = do
    dataPath <- getDataFileName . Text.unpack $ "data/" <> path
    D.copyFile dataPath outPath
  where
    outPath = outDir </> Text.unpack path

forkMany :: Traversable t => t (IO b) -> IO ()
forkMany tasks = traverse async tasks >>= mapM_ wait

getVignetteDefs :: FilePath -> IO [(Vignette, Palette, AnimatedPiece)]
getVignetteDefs = Y.decodeFileEither @[Vignette] >=> either onDecodeFail onContent
  where
    onDecodeFail err = print err >> pure mempty
    onContent = foldM addVignette mempty

    addVignette vs v = either (notFound vs) (pure . (: vs)) $ do
        let paletteName = V.vignettePaletteName v
            animName = V.vignetteAnimationName v
        palette <- maybe (Left paletteName) pure $ Map.lookup paletteName palettes
        animation <- maybe (Left animName) pure $ Map.lookup animName animations
        pure (v, palette, animation{P.palette = palette})

    notFound vs errMsg = vs <$ TIO.putStrLn errMsg
    animations = mkAnimations DefaultPalette

mkAnimations :: PaletteChoice -> Map Text AnimatedPiece
mkAnimations paletteChoice =
    foldMap
        ($ paletteChoice)
        [ Bars.animations
        , Dots.animations
        , Scr.animations
        , Boxes.animations
        , Snowflake.animations
        , Spinfield.animations
        ]
