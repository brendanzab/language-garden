# Declarative graphics DSL

This is domain specific language for drawing 2-dimensional graphics. The goal is
to describe drawings in terms of simple, composable components, with no hidden
drawing state[^1].

## Todo list

- [x] Embedded DSL
- [ ] External DSL
- [ ] Web playground

Diagram support

- [x] 2D diagrams
- [ ] 2D diagrams with envelope-based alignment
- [ ] 2.5D diagrams
- [ ] 3D diagrams

Additional backends

- [ ] CPU Backend
- [ ] SVG Backend
- [ ] GPU Backend

## Related work

This was inspired the [diagrams][haskell-diagrams] library for Haskell.
A similar library for Elm was described by Pontus Granström in [“Diagrammar:
Simply Make Interactive Diagrams”][diagrammer-talk]. The following Elm packages
were also inspiring:

- [evancz/elm-graphics](https://github.com/evancz/elm-graphics) ([Docs](https://package.elm-lang.org/packages/evancz/elm-graphics/latest/Collage))
- [timjs/elm-collage](https://github.com/timjs/elm-collage) ([Docs](https://package.elm-lang.org/packages/timjs/elm-collage/latest/))
- [vilterp/elm-diagrams](https://github.com/vilterp/elm-diagrams) ([Docs](https://package.elm-lang.org/packages/vilterp/elm-diagrams/latest/))

The [pict][racket-pict] library for Racket also implements some similar ideas.

[^1]: in contrast to imperative drawing APIs like [Processing][processing],
  [Cairo][cairo] or [HTML Canvas][html-canvas].

[cairo]: https://www.cairographics.org/
[diagrammer-talk]: https://www.youtube.com/watch?v=gT9Xu-ctNqI
[haskell-diagrams]: https://diagrams.github.io/
[html-canvas]: https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API
[processing]: https://processing.org
[racket-pict]: https://docs.racket-lang.org/pict
