# Document template language

This is a small, statically typed typed document authoring language.
Templates are type-checked and translated to a simple functional language,
which means that type errors occur on the templates themselves, as opposed to in
the generated code, or at runtime.

The language was inspired by [Forester](https://github.com/jonsterling/ocaml-forester)
and [Mylink](https://github.com/kalyani-tt/mylink) which use functional
languages as a basis for document processing, but unlike this language are
dynamically typed.
Another source of inspiration was [“A Core Calculus for Documents”](https://blog.brownplt.org/2023/12/28/document-calculus.html),
which uses evaluation rules to translate templates into other primitives in
the core language, as opposed to elaboration.

The language currently naively concatenates strings together, but it might be a
good idea to explore using document trees in the future.

## Example

<!-- TODO: use mdx to text the following example -->

Input document:

```text
${let place := "world"}

Hello ${place}!
```

Elaborated document:

```
let place : Text := "world";
"\n\nHello " + place + "!"
```

Normalised term:

```
Hello world!
```

## Todo List

- [x] Initial concrete syntax
- [x] String templates
- [ ] Lists and list combinators
- [ ] Imports and cross-document references
- [ ] Markdown-inspired syntax hooks (headings, links, lists, etc.)
- [ ] Labelled function parameters
- [ ] Parameterised templates
- [ ] Metadata fields (for information like title, author, date, tags)
- [ ] Attributed node templates (with namespaced nodes?)
- [ ] Improve syntax (perhaps something similar to Scribble?)
- [ ] Back-ends (HTML, Markdown, TeX)

## Inspiration

Papers:

- [A Core Calculus for Documents – Or, Lambda: The Ultimate Document](https://blog.brownplt.org/2023/12/28/document-calculus.html)
  ([Code](https://github.com/cognitive-engineering-lab/document-calculus))

Lightweight markup languages:

- [Djot](https://djot.net/): A simpler alternative to markdown with less ambiguities
- [Subtext](https://github.com/subconsciousnetwork/subtext): Tiny markup language geared towards index-card note-taking
- [Cooklang](https://cooklang.org/): Recipe markup language

String interpolation:

- [String interpolation examples on Wikipedia](https://en.wikipedia.org/wiki/String_interpolation#Examples)
- [ppx_string_interpolate](https://github.com/sheijk/ppx_string_interpolate)
- [ppx_string](https://github.com/janestreet/ppx_string)

Template languages:

- [Typst](https://typst.app/)
- [Scribble](https://docs.racket-lang.org/scribble/):
  Racket dialect for creating prose documents
- [Pollen](https://docs.racket-lang.org/pollen/) ([Code](https://git.matthewbutterick.com/mbutterick/pollen)):
  Racket dialect for creating digital books
- [Papyri](https://kaya3.github.io/papyri/)
- [Haml](https://haml.info/)
- [ERB](https://docs.ruby-lang.org/en/master/ERB.html) ([Code](https://github.com/ruby/erb))
- [Slim](https://slim-template.github.io/) ([Docs](https://rubydoc.info/gems/slim/frames))
- [ESH](https://github.com/jirutka/esh): Simple templating engine based on shell

Template literals and document EDSLs:

- [Unison documentation literals](https://www.unison-lang.org/docs/usage-topics/documentation/)
- [Javascript template literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [JSX](https://facebook.github.io/jsx/)
- [Hiccup](https://github.com/weavejester/hiccup)
- [Elm HTML](https://package.elm-lang.org/packages/elm/html/latest/)
- [Verso](https://github.com/leanprover/verso/): An Authoring Tool for Lean ([Talk](https://www.youtube.com/watch?v=dv_vmVs3SQQ))
- [Ur/Web](http://impredicative.com/ur/)
- [ASP.NET Razor](https://aspnetcore.readthedocs.io/en/stable/mvc/views/razor.html)
- [Markdoc](https://markdoc.dev/): Markdown-based document authoring framework.
- [Svelte](https://svelte.dev/)

Quasiquoting:

- [Haskell Wiki: Quasiquotation](https://wiki.haskell.org/Quasiquotation)
- [SWI-Prolog: library(quasi_quotations)](https://www.swi-prolog.org/pldoc/man?section=quasiquotations)

Wiki markup and personal knowledge management:

- [Oscean](https://wiki.xxiivv.com/site/oscean.html)
- [Forester](https://github.com/jonsterling/ocaml-forester)
- [Mylink](https://github.com/kalyani-tt/mylink) ([Example](https://github.com/kalyani-tt/kalyani-tt.github.io))

Attributed node languages:

- [Sofu](https://sofu.sourceforge.net)
- [SDLang: Simple Declarative Language](https://sdlang.org/)
- [KDL Document Language](https://kdl.dev/)

Data serialisation languages:

- [edn](https://github.com/edn-format/edn)
- [yaml](https://yaml.org/)
- [NestedText](https://nestedtext.org/)

Configuration languages:

- [TOML](https://toml.io/)
- [HCL](https://github.com/hashicorp/hcl)
