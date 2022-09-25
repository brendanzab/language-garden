# An embedded DSL for building signed-distance functions

This is an experiemental embedded DSL for building [signed distance functions
(SDFs)](https://en.wikipedia.org/wiki/Signed_distance_function). Signed distance
functions are a powerful technique for rendering procedural graphics in parallel
on the GPU, without the use of mesh geometry.

This DSL was motivated by noticing that some SDFs were higher-order – which
is not supported in shader languages like GLSL, and that the threading of [UV
coordinates](https://en.wikipedia.org/wiki/UV_mapping) resembled environment
passing, which could benefit from some syntactic sugar.

[Tagless-final style](https://okmij.org/ftp/tagless-final/) is used, which makes
it straightforward to compile the DSL to GLSL fragment shaders that can be
copied into [Shadertoy](https://www.shadertoy.com). The resulting GLSL code can
be currently seen [in the CLI tests](./tests.t).

The resulting DSL is clunkier than I’d like. This is due to OCaml’s odd approach
to custom operators (which don’t allow for custom precedences), and lack of
implicit overloading. Still, I think it’s an interesting proof-of-concept!

# Resources

- [The Book of Shaders](https://thebookofshaders.com/)
- [Inigo Quilez: 2D distance functions](https://iquilezles.org/articles/distfunctions2d/)
- [Inigo Quilez: distance functions](https://iquilezles.org/articles/distfunctions/)
- [Nathan Vaughn’s Shadertoy Tutorial](https://inspirnathan.com/posts/47-shadertoy-tutorial-part-1/)
- [hg_sdf: A glsl library for building signed distance functions](https://mercury.sexy/hg_sdf/)
- [Bauble: A toy for composing signed distance functions in Janet](https://bauble.studio/)
