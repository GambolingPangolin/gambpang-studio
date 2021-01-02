module GambPang.Animation (
    -- * Drafting
    Drawing,
    Point (..),
    origin,
    Vector (..),
    negateV,
    norm,
    normalize,
    displacement,
    pointToVector,
    Field2D,
    point,
    valueAtPoint,
    Rectangle,
    centeredRectangle,
    fromWidthHeight,
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
    Time,
    Path,
    followPath,
    pathProgram,
    PiecewiseLinearPath (..),
    piecewiseLinear,
    circularPath,
    makeCircular,
    Animated,
    valueAtTime,
    Motion,
    time,
    timeControl,
    backwards,
    expand,
    compress,
    shiftEarlier,
    shiftLater,
    cameraPan,

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
    centeredRectangle,
    fromWidthHeight,
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
import GambPang.Animation.Scene (
    Animated,
    Time,
    backwards,
    compress,
    expand,
    shiftEarlier,
    shiftLater,
    time,
    timeControl,
    valueAtTime,
 )
