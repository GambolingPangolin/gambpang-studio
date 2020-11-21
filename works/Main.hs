module Main where

import Codec.Picture (Image, PixelRGBA8)
import Control.Applicative (many, optional, (<**>))
import Control.Concurrent.Async (async, wait)
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Map (Map)
import qualified Data.Map as Map
import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Opt
import System.FilePath ((<.>), (</>))

import GambPang.Animation (exportGif)
import qualified GambPang.Animation.Bars as Bars
import qualified GambPang.Animation.Boxes as Boxes
import GambPang.Animation.ColorStyle (PaletteChoice (..), parsePalette)
import qualified GambPang.Animation.Dots as Dots
import qualified GambPang.Animation.Scrollers as Scr
import qualified GambPang.Animation.Snowflake as Snowflake

data CliOptions = CliOptions
    { listWorks :: Bool
    , buildAnimations :: [String]
    , palette :: Maybe String
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

animations :: PaletteChoice -> Map String [Image PixelRGBA8]
animations paletteChoice =
    Bars.animations paletteChoice
        <> Dots.animations paletteChoice
        <> Scr.animations paletteChoice
        <> Boxes.animations paletteChoice
        <> Snowflake.animations paletteChoice

animationTask :: Map String [Image PixelRGBA8] -> FilePath -> String -> IO ()
animationTask as path name = maybe notFound onRecord $ Map.lookup name as
  where
    notFound = putStrLn $ "Not found: " <> name
    onRecord anim = putStrLn outPath >> exportGif outPath 5 anim
    outPath = path </> name <.> "gif"

main :: IO ()
main = do
    cli <- Opt.execParser cliOptions

    paletteChoice <- maybe (pure DefaultPalette) (either throwIO pure . parsePalette) (palette cli)
    let as = animations paletteChoice

    when (listWorks cli) $ do
        putStrLn "Animations:"
        mapM_ putStrLn $ Map.keys as
    if null (buildAnimations cli) && not (listWorks cli)
        then putStrLn "Nothing to do."
        else forkMany $ animationTask as (outputDir cli) <$> buildAnimations cli

forkMany :: Traversable t => t (IO b) -> IO ()
forkMany tasks = traverse async tasks >>= mapM_ wait
