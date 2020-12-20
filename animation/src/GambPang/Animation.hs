module GambPang.Animation (
    -- * Scene construction
    Point (..),
    origin,
    Vector (..),
    negateV,
    norm,
    normalize,
    displacement,
    pointToVector,
    Field2D (..),
    point,
    valueAtPoint,
    Drawing,

    -- * Rigging and transformation
    AffineTransformation,
    Rigged (..),
    translate,
    rotateO,
    rotate,
    reflect,
    scale,

    -- * Animation
    Time (..),
    Path,
    followPath,
    pathProgram,
    PiecewiseLinearPath (..),
    piecewiseLinear,
    circularPath,
    makeCircular,
    Animated (..),
    valueAtTime,
    Motion,
    time,
    timeControl,
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

import GambPang.Animation.Bitmap
import GambPang.Animation.Drawing
import GambPang.Animation.Field2D
import GambPang.Animation.LinearAlgebra
import GambPang.Animation.Path
import GambPang.Animation.Render
import GambPang.Animation.Rigging
import GambPang.Animation.Scene
