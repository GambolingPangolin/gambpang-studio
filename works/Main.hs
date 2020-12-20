{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (many, optional, (<**>))
import Control.Concurrent.Async (async, wait)
import Control.Exception (throwIO)
import Control.Monad (when)
import qualified Data.ByteString.Lazy as BSL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Opt
import System.FilePath ((<.>), (</>))

import qualified GambPang.Animation.Bars as Bars
import qualified GambPang.Animation.Boxes as Boxes
import GambPang.Animation.ColorStyle (PaletteChoice (..), parsePalette)
import qualified GambPang.Animation.Dots as Dots
import GambPang.Animation.Piece (AnimatedPiece, renderGif)
import qualified GambPang.Animation.Scrollers as Scr
import qualified GambPang.Animation.Snowflake as Snowflake

data CliOptions = CliOptions
    { listWorks :: Bool
    , buildAnimations :: [Text]
    , palette :: Maybe Text
    , outputDir :: FilePath
    }
    deriving (Eq, Show)

cliOptions :: ParserInfo CliOptions
cliOptions = Opt.info (opts <**> Opt.helper) $ Opt.progDesc "Build tool for procedural art"
  where
    opts = CliOptions <$> optList <*> many optAnimation <*> optPalette <*> optOutput
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
                <> Opt.help "Folder in which to put the animations"

animations :: PaletteChoice -> Map Text AnimatedPiece
animations paletteChoice =
    Bars.animations paletteChoice
        <> Dots.animations paletteChoice
        <> Scr.animations paletteChoice
        <> Boxes.animations paletteChoice
        <> Snowflake.animations paletteChoice

animationTask :: Map Text AnimatedPiece -> FilePath -> Text -> IO ()
animationTask as path name = maybe notFound onRecord $ Map.lookup name as
  where
    onRecord anim = putStrLn outPath >> either onRenderError (BSL.writeFile outPath) (renderGif anim)
    outPath = path </> Text.unpack name <.> "gif"
    notFound = TIO.putStrLn $ "Not found: " <> name
    onRenderError = putStrLn

main :: IO ()
main = do
    cli <- Opt.execParser cliOptions

    paletteChoice <- maybe (pure DefaultPalette) (either throwIO pure . parsePalette) (palette cli)
    let as = animations paletteChoice

    when (listWorks cli) $ do
        putStrLn "Animations:"
        mapM_ TIO.putStrLn $ Map.keys as
    if null (buildAnimations cli) && not (listWorks cli)
        then putStrLn "Nothing to do."
        else forkMany $ animationTask as (outputDir cli) <$> buildAnimations cli

forkMany :: Traversable t => t (IO b) -> IO ()
forkMany tasks = traverse async tasks >>= mapM_ wait
