# An embedded DSL for compiling signed-distance functions

TL;DR: I was doing [a tutorial](https://inspirnathan.com/posts/47-shadertoy-tutorial-part-1/)
on how to use procedural graphics on the GPU and was irritated by the lack of
higher-order functions, parametric polymorphism, and an effect system in GLSL. 🥲

[Signed distance functions] (SDFs)  are a powerful approach for rendering
procedural graphics in parallel on the GPU, without the use of mesh geometry.
For more details see the  [resources](#resources) listed below. This is an
embedded DSL for composing SDFs in OCaml and compiling them to shader programs.

An example scene can be found in [Main.ml](./Main.ml), and the signature of
available signed distance functions can be found in [Sdf.ml](./Sdf.ml). The
resulting DSL is clunkier than I’d like. This is due to OCaml’s odd approach to
custom operators (which don’t allow for custom precedences), and lack of
implicit overloading. Still, I think it’s an interesting proof-of-concept!

Internally we use [tagless-final style] to implement the DSL. This means that
shader expressions are properly type-checked, and compilation to GLSL shaders is
reasonably straightforward. The resulting GLSL code can currently be seen [in
the CLI tests](./tests.t). In the future alternate back-ends for targets like
[HLSL], [MSL] and [SPIR-V] could also be implemented.

[Signed distance functions]: https://en.wikipedia.org/wiki/Signed_distance_function
[tagless-final style]: https://okmij.org/ftp/tagless-final/
[Shadertoy]: https://www.shadertoy.com
[HLSL]: https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl
[MSL]: https://developer.apple.com/metal/Metal-Shading-Language-Specification.pdf
[SPIR-V]: https://www.khronos.org/spir/

## Resources

- [The Book of Shaders](https://thebookofshaders.com/)
- [Inigo Quilez: 2D distance functions](https://iquilezles.org/articles/distfunctions2d/)
- [Inigo Quilez: distance functions](https://iquilezles.org/articles/distfunctions/)
- [Nathan Vaughn’s Shadertoy Tutorial](https://inspirnathan.com/posts/47-shadertoy-tutorial-part-1/)
- [hg_sdf: A glsl library for building signed distance functions](https://mercury.sexy/hg_sdf/)
- [Bauble: A toy for composing signed distance functions in Janet](https://bauble.studio/)
