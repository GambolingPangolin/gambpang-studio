module GambPang.Animation (
    -- * Drafting
    Drawing,
    Point (..),
    origin,
    midpoint,
    Vector (..),
    negateV,
    norm,
    normalize,
    displacement,
    pointToVector,
    Field2D,
    point,
    valueAtPoint,
    Grid (..),
    makeGrid,

    -- * Rigging and transformation
    AffineTransformation,
    Rigged (..),
    translate,
    translating,
    rotateO,
    rotatingO,
    rotate,
    rotating,
    reflect,
    scale,
    scaling,
    scaleXY,

    -- * Animation
    Animated,
    getStill,
    Time,
    Motion,
    time,
    timeControl,
    backwards,
    expand,
    compress,
    shiftEarlier,
    shiftLater,
    cameraPan,

    -- * Framing
    Rectangle,
    unitRectangle,
    centeredRectangle,
    fromWidthHeight,
    width,
    height,
    center,
    rescale,
    unitRescale,

    -- * Paths
    Path,
    followPath,
    pathProgram,
    PiecewiseLinearPath (..),
    piecewiseLinear,
    circularPath,
    makeCircular,

    -- * Bitmap
    pixelPoint,
    colorPixel,
    unitPixel,

    -- * Rendering
    ViewFrame (..),
    exportGif,
    renderAnimField2D,
    renderAnimDrawing,
) where

import GambPang.Animation.Animated (
    Animated,
    Time,
    backwards,
    compress,
    expand,
    getStill,
    rescale,
    shiftEarlier,
    shiftLater,
    time,
    timeControl,
    unitRescale,
 )
import GambPang.Animation.Bitmap (
    ViewFrame (..),
    colorPixel,
    pixelPoint,
    unitPixel,
 )
import GambPang.Animation.Drawing (Drawing)
import GambPang.Animation.Drawing.Grid (Grid (..), makeGrid)
import GambPang.Animation.Field2D (Field2D, point, valueAtPoint)
import GambPang.Animation.LinearAlgebra (
    AffineTransformation,
    Point (..),
    Vector (..),
    displacement,
    midpoint,
    negateV,
    norm,
    normalize,
    origin,
    pointToVector,
 )
import GambPang.Animation.Path (
    Path,
    PiecewiseLinearPath (..),
    circularPath,
    makeCircular,
    pathProgram,
    piecewiseLinear,
 )
import GambPang.Animation.Rectangle (
    Rectangle,
    center,
    centeredRectangle,
    fromWidthHeight,
    height,
    unitRectangle,
    width,
 )
import GambPang.Animation.Render (
    exportGif,
    renderAnimDrawing,
    renderAnimField2D,
 )
import GambPang.Animation.Rigging (
    Motion,
    Rigged (..),
    cameraPan,
    followPath,
    reflect,
    rotate,
    rotateO,
    rotating,
    rotatingO,
    scale,
    scaleXY,
    scaling,
    translate,
    translating,
 )
