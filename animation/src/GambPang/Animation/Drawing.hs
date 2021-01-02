module GambPang.Animation.Drawing (
    -- * Drafting
    Drawing,
    union,
    draw,
    mask,
    exclude,
    transform,
    Shape,
    disc,
    ellipse,
    rectangle,
    polygon,

    -- * Utils
    composite,

    -- * Rendering
    renderDrawing,
) where

import GambPang.Animation.Drawing.Internal
import GambPang.Animation.Time (Animated)

composite :: [Animated (Drawing c)] -> Animated (Drawing c)
composite = fmap union . sequenceA
