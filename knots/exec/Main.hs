{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Main where

import Codec.Picture (DynamicImage (ImageRGBA8), PixelRGBA8, savePngImage)
import Control.Applicative ((<**>), (<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Data.Bifunctor (first)
import qualified Data.Text.IO as TIO
import GambPang.Knots.Pattern (
    Pattern,
    PatternRenderError,
    addComputedBlockedEdges,
    newPattern,
    renderPattern,
 )
import GambPang.Knots.Pattern.Encoding (
    WorksheetError,
    decodePattern,
    parseWorksheet,
    toWorksheet,
 )
import GambPang.Knots.Pixels (readPixel)
import GambPang.Knots.Tiles (TileConfig (..))
import qualified Options.Applicative as Opt

data InputSource = SpecFile FilePath | WorksheetFile FilePath
    deriving (Eq, Show)

data RenderOptions = RenderOptions
    { inputFile :: InputSource
    , outputFile :: FilePath
    , foreground :: PixelRGBA8
    , background :: PixelRGBA8
    }
    deriving (Eq, Show)

data GenerateOptions = GenerateOptions
    { width :: Int
    , height :: Int
    , outputFile :: FilePath
    }
    deriving (Eq, Show)

data Command = Render RenderOptions | GenerateWorksheet GenerateOptions

getOptions :: IO Command
getOptions = Opt.execParser $ Opt.info (opts <**> Opt.helper) desc
  where
    desc = Opt.progDesc "Knot maker"
    opts =
        Opt.subparser $
            Opt.command "render" renderCommand <> Opt.command "generate" genCommand

    renderCommand = Render <$> Opt.info (renderOpts <**> Opt.helper) renderDesc
    renderDesc = Opt.progDesc "Render a knot"
    renderOpts =
        RenderOptions
            <$> optInput
            <*> optRenderOutputFile
            <*> optForeground
            <*> optBackground

    optInput = optSpec <|> optPattern
    optSpec =
        fmap SpecFile . Opt.strOption $
            Opt.long "spec"
                <> Opt.short 's'
                <> Opt.help "The path to the knot specification"
                <> Opt.value "knot.spec"
    optPattern =
        fmap WorksheetFile . Opt.strOption $
            Opt.long "pattern"
                <> Opt.short 'p'
                <> Opt.help "The path to the knot pattern"
                <> Opt.value "knot.pattern"

    optRenderOutputFile =
        Opt.strOption $
            Opt.long "output"
                <> Opt.short 'o'
                <> Opt.help "The path to the output file"
                <> Opt.value "knot.png"
    optForeground =
        fmap readPixel . Opt.strOption $
            Opt.long "foreground"
                <> Opt.short 'f'
                <> Opt.help "The foreground color"
                <> Opt.value "#FFFFFF"
    optBackground =
        fmap readPixel . Opt.strOption $
            Opt.long "background"
                <> Opt.short 'b'
                <> Opt.help "The background color"
                <> Opt.value "#000000"

    genCommand = GenerateWorksheet <$> Opt.info (genOptions <**> Opt.helper) genDesc
    genDesc = Opt.progDesc "Generate a worksheet"
    genOptions = GenerateOptions <$> optWidth <*> optHeight <*> optGenOutputFile
    optWidth =
        Opt.option Opt.auto $
            Opt.long "width"
                <> Opt.short 'w'
                <> Opt.value 10
                <> Opt.help "Width of pattern"
    optHeight =
        Opt.option Opt.auto $
            Opt.long "height"
                <> Opt.short 'h'
                <> Opt.value 10
                <> Opt.help "Height of pattern"
    optGenOutputFile =
        Opt.strOption $
            Opt.long "output"
                <> Opt.short 'o'
                <> Opt.help "The path to the output file"
                <> Opt.value "knot.pattern"

main :: IO ()
main =
    getOptions >>= \case
        Render opts -> runExceptT (createImage opts) >>= either print ignore
        GenerateWorksheet opts ->
            TIO.writeFile opts.outputFile . toWorksheet $ newPattern opts.width opts.height
  where
    ignore = const $ pure ()

createImage :: RenderOptions -> ExceptT KnotError IO ()
createImage conf = do
    spec <- addComputedBlockedEdges <$> loadSpec conf.inputFile
    except (first KnotRenderError $ renderPattern tileConf spec Nothing)
        >>= lift . savePngImage conf.outputFile . ImageRGBA8
  where
    tileConf =
        TileConfig
            { foreground = conf.foreground
            , background = conf.background
            , width = 20
            }

loadSpec :: InputSource -> ExceptT KnotError IO Pattern
loadSpec = \case
    SpecFile file -> lift (TIO.readFile file) >>= except . first ParseError . decodePattern
    WorksheetFile file -> lift (TIO.readFile file) >>= except . first WorksheetError . parseWorksheet

data KnotError
    = KnotRenderError PatternRenderError
    | ParseError String
    | WorksheetError WorksheetError
    deriving (Eq, Show)
