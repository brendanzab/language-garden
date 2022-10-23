# An embedded DSL for rendering procedural graphics on the GPU

TL;DR: I was doing [a tutorial](https://inspirnathan.com/posts/47-shadertoy-tutorial-part-1/)
on how to use procedural graphics on the GPU and was irritated by the lack of
higher-order functions, parametric polymorphism, and an effect system in GLSL. 🥲

[Signed distance functions] (SDFs) are a powerful approach for rendering
procedural graphics in parallel on the GPU, without the use of mesh geometry.
For more details see the  [resources](#resources) listed below. This is an
embedded DSL for composing SDFs in OCaml and compiling them to shader programs.

<!-- $MDX file=examples/Readme.ml,part=scene -->
```ocaml
(** A scene to render, assuming UV coordinates in (-0.5, 0.5) *)
let scene : (vec3f repr) Env.t  =
  (* Some shapes defined using signed distance functions *)
  let* s1 = circle !!0.3 |> move (S.vec2 !!0.0 !!0.0) in
  let* s2 = square !!0.2 |> move (S.vec2 !!0.2 !!0.0) in

  (* Combine the two shapes, meeting at a rounded edge *)
  let shape = union_round s1 s2 !!0.05 in

  (* Colours to use in the background and foreground *)
  let background_color = S.vec3 !!0.35 !!0.45 !!0.50 in
  let shape_color = S.vec3 !!1.0 !!1.0 !!1.0 in

  (* The final output colour to render at the current UV coordinate. *)
  Env.pure (background_color |> overlay ~shape:shape ~color:shape_color)
```

The resulting DSL is clunkier than I’d like. This is due to OCaml’s odd approach
to custom operators (which don’t allow for custom precedences), and lack of
implicit overloading. Still, I think it’s an interesting proof-of-concept!

A full example can be found in [examples/Basic.ml](./examples/Basic.ml), which
renders the following image using the sequential, CPU-based back-end:

![The rendered output of Main.ml](./test/readme.png)

## Implementation details

Internally an approach called [tagless-final style] is used to implement the DSL.
This helps make supporting either compilation to GLSL shaders or rendering
directly on the CPU reasonably straightforward. In the future alternate
back-ends for targets like [HLSL], [MSL] and [SPIR-V] could also be implemented.

The compiled GLSL code can be seen at [test/basic.glsl](./test/basic.glsl).
For now the compiler assigns the result of each intermediate computation to a
new local variable, attempting to avoid introducing duplicate computations.
I’m not sure if this is the most efficient approach, or whether attempting to
compile to higher-level code would work better (this would probably depend on
the how GLSL code is compiled or interpreted by the GPU driver).

[Signed distance functions]: https://en.wikipedia.org/wiki/Signed_distance_function
[tagless-final style]: https://okmij.org/ftp/tagless-final/
[Shadertoy]: https://www.shadertoy.com
[HLSL]: https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl
[MSL]: https://developer.apple.com/metal/Metal-Shading-Language-Specification.pdf
[SPIR-V]: https://www.khronos.org/spir/

## Ideas for future work

- [ ] Conditionals
- [ ] 3D SDFs
- [ ] Examples of raymarching
- [ ] Typed coordinate systems
- [ ] PPX to improve ergonomics of embedded DSL
- [ ] Replace monads with algebraic effects
- [ ] Optimise compiled shaders (eg. constant folding)
- [ ] SPIR-V Backend
- [ ] Browser-based shader previews
- [ ] Extenal DSL with improved ergonomics
- [ ] Browser-based shader playground

## Resources

- [The Book of Shaders](https://thebookofshaders.com/)
- [Inigo Quilez: 2D distance functions](https://iquilezles.org/articles/distfunctions2d/)
- [Inigo Quilez: distance functions](https://iquilezles.org/articles/distfunctions/)
- [Nathan Vaughn’s Shadertoy Tutorial](https://inspirnathan.com/posts/47-shadertoy-tutorial-part-1/)
- [HG_SDF: A glsl library for building signed distance functions](https://mercury.sexy/hg_sdf/)
- [Bauble: A toy for composing signed distance functions in Janet](https://bauble.studio/)
