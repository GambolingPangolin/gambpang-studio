{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Main where

import Codec.Picture (
    DynamicImage (ImageRGBA8),
    Image,
    PixelRGBA8,
    decodePng,
    imageHeight,
    imageWidth,
    savePngImage,
 )
import Control.Applicative ((<**>), (<|>))
import Control.Exception (Exception, throwIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.Text.IO as TIO
import GambPang.Knots.Image (imageToPattern)
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
import GambPang.Knots.Pixels (crop, readPixel, rotate, toGrayscale)
import GambPang.Knots.Random (randomPattern)
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

data OutputType = OutputImage | OutputWorksheet
    deriving (Eq, Show)

data ConvertOptions = ConvertOptions
    { convertInput :: FilePath
    , convertOutputType :: OutputType
    , convertOutput :: FilePath
    , convertForeground :: PixelRGBA8
    , convertBackground :: PixelRGBA8
    }
    deriving (Eq, Show)

data RandomOptions = RandomOptions
    { randomOutput :: FilePath
    , randomWidth :: Int
    , randomHeight :: Int
    , randomDensity :: Double
    , randomForeground :: PixelRGBA8
    , randomBackground :: PixelRGBA8
    }
    deriving (Eq, Show)

data Command
    = Render RenderOptions
    | GenerateWorksheet GenerateOptions
    | Convert ConvertOptions
    | Random RandomOptions
    deriving (Eq, Show)

getOptions :: IO Command
getOptions = Opt.execParser $ Opt.info (opts <**> Opt.helper) desc
  where
    desc = Opt.progDesc "Knot maker"
    opts =
        Opt.hsubparser $
            Opt.command "render" renderCommand
                <> Opt.command "generate" genCommand
                <> Opt.command "convert" convertCommand
                <> Opt.command "random" randomCommand

    renderCommand = Render <$> Opt.info renderOpts renderDesc
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
                <> Opt.showDefault
    optPattern =
        fmap WorksheetFile . Opt.strOption $
            Opt.long "pattern"
                <> Opt.short 'p'
                <> Opt.help "The path to the knot pattern"
                <> Opt.value "knot.pattern"
                <> Opt.showDefault

    optRenderOutputFile =
        Opt.strOption $
            Opt.long "output"
                <> Opt.short 'o'
                <> Opt.help "The path to the output file"
                <> Opt.value "knot.png"
                <> Opt.showDefault
    optForeground =
        fmap readPixel . Opt.strOption $
            Opt.long "foreground"
                <> Opt.short 'f'
                <> Opt.help "The foreground color"
                <> Opt.value "#FFFFFF"
                <> Opt.showDefault
    optBackground =
        fmap readPixel . Opt.strOption $
            Opt.long "background"
                <> Opt.short 'b'
                <> Opt.help "The background color"
                <> Opt.value "#000000"
                <> Opt.showDefault

    genCommand = GenerateWorksheet <$> Opt.info genOptions genDesc
    genDesc = Opt.progDesc "Generate a worksheet"
    genOptions = GenerateOptions <$> optWidth <*> optHeight <*> optOutputFile
    optWidth =
        Opt.option Opt.auto $
            Opt.long "width"
                <> Opt.short 'w'
                <> Opt.value 10
                <> Opt.help "Width of pattern"
                <> Opt.showDefault
    optHeight =
        Opt.option Opt.auto $
            Opt.long "height"
                <> Opt.short 'h'
                <> Opt.value 10
                <> Opt.help "Height of pattern"
                <> Opt.showDefault

    convertCommand = Convert <$> Opt.info convertOptions convertDesc
    convertDesc = Opt.progDesc "Convert an image to a celtic knot"
    convertOptions =
        ConvertOptions
            <$> optInputImage
            <*> optConvertOutputType
            <*> optOutputFile
            <*> optForeground
            <*> optBackground
    optInputImage =
        Opt.strOption $
            Opt.long "input"
                <> Opt.short 'i'
                <> Opt.help "The image to convert"
    optConvertOutputType =
        Opt.flag OutputImage OutputWorksheet $
            Opt.long "worksheet" <> Opt.help "Produce a worksheet instead of an image"
    optOutputFile =
        Opt.strOption $
            Opt.long "output"
                <> Opt.short 'o'
                <> Opt.value "knot.png"
                <> Opt.help "The path at which to output the knot image"
                <> Opt.showDefault

    randomCommand = Random <$> Opt.info randomOptions randomDesc
    randomDesc = Opt.progDesc "Generate a random knot"
    randomOptions =
        RandomOptions
            <$> optOutputFile
            <*> optWidth
            <*> optHeight
            <*> optDensity
            <*> optForeground
            <*> optBackground
    optDensity =
        Opt.option Opt.auto $
            Opt.short 'd'
                <> Opt.long "density"
                <> Opt.value 0.6
                <> Opt.help "Density of blocked edges"
                <> Opt.showDefault

main :: IO ()
main =
    getOptions >>= \case
        Render opts -> runExceptT (createImage opts) >>= either print ignore
        GenerateWorksheet opts ->
            TIO.writeFile opts.outputFile . toWorksheet $ newPattern opts.width opts.height
        Convert opts -> convertImage opts
        Random opts -> randomKnot opts
  where
    ignore = const $ pure ()

createImage :: RenderOptions -> ExceptT KnotError IO ()
createImage conf = do
    spec <- addComputedBlockedEdges <$> loadSpec conf.inputFile
    except (first KnotRenderError $ renderPattern tileConf spec Nothing)
        >>= lift . savePngImage conf.outputFile . postProcess spec conf.background
  where
    tileConf =
        TileConfig
            { foreground = conf.foreground
            , background = conf.background
            , width = 20
            }

convertImage :: ConvertOptions -> IO ()
convertImage opts =
    BS.readFile (convertInput opts)
        >>= either (throwIO . ImageDecodeError) (onPattern . onImage) . decodePng
  where
    onImage = imageToPattern 0x80 . toGrayscale
    onPattern thePattern = case convertOutputType opts of
        OutputImage ->
            either
                (throwIO . KnotRenderError)
                pure
                (renderPattern tileConf thePattern Nothing)
                >>= savePngImage (convertOutput opts)
                    . postProcess thePattern (convertBackground opts)
        OutputWorksheet -> TIO.putStrLn $ toWorksheet thePattern

    tileConf =
        TileConfig
            { foreground = convertForeground opts
            , background = convertBackground opts
            , width = 10
            }

randomKnot :: RandomOptions -> IO ()
randomKnot opts = do
    thePattern <- randomPattern opts.randomDensity opts.randomWidth opts.randomHeight
    either
        (throwIO . KnotRenderError)
        pure
        (renderPattern tileConf thePattern Nothing)
        >>= savePngImage opts.randomOutput
            . postProcess thePattern opts.randomBackground
  where
    tileConf =
        TileConfig
            { foreground = opts.randomForeground
            , background = opts.randomBackground
            , width = 20
            }

{- | Knots are naturally oriented diagonally, so we need to rotate the image and
 crop it
-}
postProcess ::
    Pattern ->
    PixelRGBA8 ->
    Image PixelRGBA8 ->
    DynamicImage
postProcess thePattern bg sourceImage = ImageRGBA8 processedImage
  where
    rotatedImage = rotate (pi / 4) bg sourceImage
    w = imageWidth sourceImage
    knotW = ceiling (fromIntegral (tileW * thePattern.height) * sqrt @Double 2) + tileW

    h = imageHeight sourceImage
    knotH = ceiling (fromIntegral (tileW * thePattern.width) * sqrt @Double 2) + tileW

    minX = (w - knotW) `quot` 2
    minY = (h - knotH) `quot` 2

    processedImage = crop (minX, minY) (w - minX, h - minY) rotatedImage

    tileW = 20 -- FIXME

loadSpec :: InputSource -> ExceptT KnotError IO Pattern
loadSpec = \case
    SpecFile file -> lift (TIO.readFile file) >>= except . first ParseError . decodePattern
    WorksheetFile file -> lift (TIO.readFile file) >>= except . first WorksheetError . parseWorksheet

data KnotError
    = KnotRenderError PatternRenderError
    | ParseError String
    | WorksheetError WorksheetError
    | ImageDecodeError String
    | UnsupportedPixelType
    deriving (Eq, Show)

instance Exception KnotError
