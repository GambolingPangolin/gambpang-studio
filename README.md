# gambpang-studio

This is my attempt to create a system for generating looping, geometric, GIF
animations.  My goal here to solve the design problems for myself and implement a
rasterization system.  In this system we express animations using the `Animation a` 
functor, which is a `Reader` monad for time.  We describe snapshots in time
using `Drawing`, a simple DSL for arranging primitive colored shapes.  Various
objects implement the `Rigged` typeclass, which makes it possible to apply
affine transformations.

For a more sophisticated animation package with an active
community please have a look at [reanimate][1].

The project has two packages: `gambpang-animation` and `gambpang-works`, which
contain core abstractions and example animations respectively.

[1]: https://github.com/reanimate/reanimate
