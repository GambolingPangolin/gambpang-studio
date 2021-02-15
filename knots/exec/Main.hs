{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Main where

import Codec.Picture (DynamicImage (ImageRGBA8), PixelRGBA8 (PixelRGBA8), savePngImage)
import Control.Applicative ((<**>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Data.Bifunctor (first)
import qualified Data.Text.IO as TIO
import GambPang.Knots.Pattern (Pattern, PatternRenderError, defaultBlockedEdges, renderPattern)
import GambPang.Knots.Pattern.Encoding (decodePattern)
import GambPang.Knots.Tiles (TileConfig (..))
import qualified Options.Applicative as Opt

data Options = Options
    { specFile :: FilePath
    , outputFile :: FilePath
    }
    deriving (Eq, Show)

getOptions :: IO Options
getOptions = Opt.execParser $ Opt.info (opts <**> Opt.helper) desc
  where
    desc = Opt.progDesc "Knot maker"
    opts = Options <$> optSpec <*> optOutputFile
    optSpec =
        Opt.strOption $
            Opt.long "spec"
                <> Opt.short 's'
                <> Opt.help "The path to the knot specification"
                <> Opt.value "knot.spec"
    optOutputFile =
        Opt.strOption $
            Opt.long "output"
                <> Opt.short 'o'
                <> Opt.help "The path to the output file"
                <> Opt.value "knot.png"

main :: IO ()
main = getOptions >>= runExceptT . createImage >>= either print ignore
  where
    ignore = const $ pure ()

createImage :: Options -> ExceptT KnotError IO ()
createImage conf = do
    rawSpec <- loadSpec conf.specFile
    let spec = rawSpec{blockedEdges = defaultBlockedEdges rawSpec.diagonal <> rawSpec.blockedEdges}
    except (first KnotRenderError $ renderPattern defaultConfig spec Nothing)
        >>= lift . savePngImage conf.outputFile . ImageRGBA8

loadSpec :: FilePath -> ExceptT KnotError IO Pattern
loadSpec file = lift (TIO.readFile file) >>= except . first ParseError . decodePattern

defaultConfig :: TileConfig
defaultConfig =
    TileConfig
        { foreground = PixelRGBA8 0xff 0xff 0xff 0xff
        , background = PixelRGBA8 0x00 0x00 0x00 0xff
        , width = 20
        }

data KnotError = KnotRenderError PatternRenderError | ParseError String
    deriving (Eq, Show)
