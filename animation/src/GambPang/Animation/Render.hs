module GambPang.Animation.Render (
    exportGif,
    renderAnimField2D,
    renderAnimDrawing,
    sampleDrawings,
) where

import Codec.Picture (
    GifLooping (LoopingForever),
    Image,
    Pixel (writePixel),
    PixelRGBA8 (..),
    encodeGifAnimation,
    generateImage,
    pixelMap,
 )
import Codec.Picture.Types (
    MutableImage,
    TransparentPixel (dropTransparency),
    freezeImage,
    thawImage,
 )
import Control.Monad (foldM, void)
import Control.Monad.ST (ST, runST)
import Control.Parallel.Strategies (parList, rpar, using)
import qualified Data.ByteString.Lazy as BSL
import Data.Colour (Colour)
import qualified Data.Map.Merge.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import GambPang.Animation.Bitmap (
    ViewFrame (..),
    colorPixel,
    pixelPoint,
 )
import GambPang.Animation.Drawing.Internal (Drawing, mapColor, renderDrawing)
import GambPang.Animation.Field2D (Field2D, valueAtPoint)
import GambPang.Animation.Scene (
    Animated (..),
    Time (..),
    valueAtTime,
 )

exportGif :: FilePath -> Int -> [Image PixelRGBA8] -> IO ()
exportGif outPath delay =
    either error (BSL.writeFile outPath)
        . encodeGifAnimation delay LoopingForever
        . fmap (pixelMap dropTransparency)

-- | Use an extremely naive rasterization method, refine later
renderAnimField2D ::
    -- | Number of frames
    Int ->
    -- | What portion of the scene to render
    ViewFrame ->
    Animated (Field2D PixelRGBA8) ->
    [Image PixelRGBA8]
renderAnimField2D n vf a = genImage <$> timeSamples vf n
  where
    genImage t = generateImage (getFrame t) (viewFrameWidth vf) (viewFrameHeight vf)
    getFrame t px py = valueAtPoint (getPoint px py) $ valueAtTime t a
    getPoint px py = pixelPoint (adjustX vf px) (adjustY vf py)

timeSamples :: ViewFrame -> Int -> [Time]
timeSamples vf n =
    Time . (unTime (endTime vf) *) . (/ fromIntegral n) . fromIntegral <$> [0 .. n]

data AnimatedDrawingState = AnimatedDrawingState
    { frames :: [Image PixelRGBA8]
    , pixels :: Map (Int, Int) (Colour Double)
    }

initialADState :: AnimatedDrawingState
initialADState = AnimatedDrawingState mempty mempty

animationFrames :: AnimatedDrawingState -> [Image PixelRGBA8]
animationFrames = reverse . frames

renderAnimDrawing ::
    -- | Number of frames
    Int ->
    ViewFrame ->
    -- | Background color
    Colour Double ->
    (color -> Colour Double) ->
    Animated (Drawing color) ->
    [Image PixelRGBA8]
renderAnimDrawing n vf bg getColor (Animated a) = runST $ do
    img <- thawImage initialImage
    animationFrames <$> foldM (calcImage vf bg img) initialADState drawings
  where
    initialImage =
        generateImage (\_ _ -> colorPixel bg 0xFF) (viewFrameWidth vf) (viewFrameHeight vf)
    drawings = sampleDrawings n vf getColor a

sampleDrawings ::
    Int ->
    ViewFrame ->
    (color1 -> color2) ->
    (Time -> Drawing color1) ->
    [Drawing color2]
sampleDrawings n vf getColor a = (mapColor getColor . a <$> timeSamples vf n) `using` parList rpar

calcImage ::
    ViewFrame ->
    Colour Double ->
    MutableImage s PixelRGBA8 ->
    AnimatedDrawingState ->
    Drawing (Colour Double) ->
    ST s AnimatedDrawingState
calcImage vf bg img prev drawing = do
    applyDiff vf updatedPixels img
    thisImage <- freezeImage img
    pure
        AnimatedDrawingState
            { frames = thisImage : frames prev
            , pixels = thisPixels
            }
  where
    thisPixels = renderDrawing drawing
    updatedPixels = crop $ diffPixels bg (pixels prev) thisPixels
    crop = Map.filterWithKey inFrame
    inFrame (x, y) _ =
        0 <= adjustX vf x
            && adjustX vf x < viewFrameWidth vf
            && 0 <= adjustY vf y
            && adjustY vf y < viewFrameHeight vf

diffPixels ::
    Colour Double ->
    Map (Int, Int) (Colour Double) ->
    Map (Int, Int) (Colour Double) ->
    Map (Int, Int) PixelRGBA8
diffPixels bg = Map.merge stalePixel newPixel overlapPixel
  where
    stalePixel = Map.mapMissing $ \_ _ -> colorPixel bg 0xFF
    newPixel = Map.mapMissing $ const (`colorPixel` 0xFF)
    overlapPixel = Map.zipWithMaybeMatched $ const onMatch
    onMatch old new
        | old == new = Nothing
        | otherwise = Just $ colorPixel new 0xFF

applyDiff :: Pixel a => ViewFrame -> Map (Int, Int) a -> MutableImage s a -> ST s ()
applyDiff vf diff img = void $ Map.traverseWithKey updatePixel diff
  where
    updatePixel (x, y) = writePixel img (adjustX vf x) (adjustY vf y)

adjustX :: ViewFrame -> Int -> Int
adjustX vf x = x - minX
  where
    (minX, _) = lowerLeft vf

adjustY :: ViewFrame -> Int -> Int
adjustY vf y = h - 1 - y + minY
  where
    h = viewFrameHeight vf
    (_, minY) = lowerLeft vf
