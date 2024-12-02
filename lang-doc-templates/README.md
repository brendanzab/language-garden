# Document template language

This is a small, statically typed typed document authoring language.
Templates are type-checked and translated to a simple functional language,
which means that type errors occur on the templates themselves, as opposed to in
the generated code, or at runtime.

The language was inspired by [Forester](https://github.com/jonsterling/ocaml-forester)
and [Mylink](https://github.com/kalyani-tt/mylink) which use functional
languages as a basis for document processing.
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
- [ ] External renderers ([KaTeX](https://katex.org/), [Graphvis](https://graphviz.org/),
      [Penrose](https://penrose.cs.cmu.edu), [PlantUML](https://plantuml.com/),
      syntax highlighting, railroad diagrams, reference lists, etc)
- [ ] Datalog-style query language
- [ ] Back-ends (HTML, Markdown, TeX, RSS)

## Inspiration

Papers:

- [A Core Calculus for Documents – Or, Lambda: The Ultimate Document](https://blog.brownplt.org/2023/12/28/document-calculus.html)
  ([Paper](https://doi.org/10.1145/3632865))
  ([Talk](https://www.youtube.com/watch?v=yC4ja0Zines))
  ([Code](https://github.com/cognitive-engineering-lab/document-calculus))

Lightweight markup languages:

- [Djot](https://djot.net/): A simpler alternative to markdown with less ambiguities
- [Org Mode](https://orgmode.org/): Document editing language for Emacs
- [Subtext](https://github.com/subconsciousnetwork/subtext): Tiny markup language geared towards index-card note-taking
- [Cooklang](https://cooklang.org/): Recipe markup language
- [mexdown](https://github.com/smasher164/mexdown): A markdown-like language that allows commands to generate parts of a document

String interpolation:

- [String interpolation examples on Wikipedia](https://en.wikipedia.org/wiki/String_interpolation#Examples)
- [ppx_string_interpolate](https://github.com/sheijk/ppx_string_interpolate)
- [ppx_string](https://github.com/janestreet/ppx_string)

Programmable markup languages:

- [Typst](https://typst.app/)
- [Scribble](https://docs.racket-lang.org/scribble/):
  Racket dialect for creating prose documents
- [Pollen](https://docs.racket-lang.org/pollen/) ([Code](https://git.matthewbutterick.com/mbutterick/pollen)):
  Racket dialect for creating digital books
- [Papyri](https://kaya3.github.io/papyri/)
- [Patoline](https://patoline.github.io/) ([Code](https://github.com/patoline/patoline))
  Typesetting system with embedded OCaml code

Template languages:

- [Haml](https://haml.info/)
- [ERB](https://docs.ruby-lang.org/en/master/ERB.html) ([Code](https://github.com/ruby/erb))
- [Slim](https://slim-template.github.io/) ([Docs](https://rubydoc.info/gems/slim/frames))
- [ESH](https://github.com/jirutka/esh): Simple templating engine based on shell

String/Node/Document Literals:

- [JSX](https://facebook.github.io/jsx/)
- [Hiccup](https://github.com/weavejester/hiccup)
- [Elm HTML](https://package.elm-lang.org/packages/elm/html/latest/)
- [kotlinx.html](https://github.com/Kotlin/kotlinx.html)
- [ScalaTags](https://com-lihaoyi.github.io/scalatags/)
- [Unison documentation literals](https://www.unison-lang.org/docs/usage-topics/documentation/)
- [Javascript template literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)

Document DSLs:

- [Verso](https://github.com/leanprover/verso/): An Authoring Tool for Lean ([Talk](https://www.youtube.com/watch?v=dv_vmVs3SQQ))
- [Ur/Web](http://impredicative.com/ur/)
- [ASP.NET Razor](https://aspnetcore.readthedocs.io/en/stable/mvc/views/razor.html)
- [Markdoc](https://markdoc.dev/): Markdown-based document authoring framework.
- [Svelte](https://svelte.dev/)

Quasiquoting:

- [Haskell Wiki: Quasiquotation](https://wiki.haskell.org/Quasiquotation)
- [SWI-Prolog: library(quasi_quotations)](https://www.swi-prolog.org/pldoc/man?section=quasiquotations)

Wiki markup and personal knowledge management:

- [Forester](https://github.com/jonsterling/ocaml-forester)
- [Mylink](https://github.com/kalyani-tt/mylink)
- [Omake](https://rubenerd.com/xmlns-omake/): cardfile format written in XML, presented with an XSL transform
- [Org-roam](https://www.orgroam.com/): Emacs-based knowledge management with Org Mode
- [Oscean](https://wiki.xxiivv.com/site/oscean.html): Wiki engine written entirely in assembler
- [yon](https://m15o.ichi.city/yon/): Wiki editor in a standalone HTML file with no dependencies
- [TiddlyWiki](https://tiddlywiki.com/): a non-linear personal web notebook

Attributed node languages:

- [Sofu](https://sofu.sourceforge.net)
- [SDLang: Simple Declarative Language](https://sdlang.org/)
- [KDL Document Language](https://kdl.dev/)
- [RELAX NG compact syntax](https://relaxng.org/compact-tutorial-20030326.html)
- [XDuce](https://xduce.sourceforge.net/) ([Github mirror](https://github.com/ozzymcduff/XDuce))
- [svg, idk](https://x.nas.sr/svg-editor/) ([Mastodon thread](https://merveilles.town/@nasser/112300138172061442))

Data serialisation languages:

- [edn](https://github.com/edn-format/edn)
- [yaml](https://yaml.org/)
- [NestedText](https://nestedtext.org/)

Configuration languages:

- [TOML](https://toml.io/)
- [HCL](https://github.com/hashicorp/hcl)
- [Pkl](https://pkl-lang.org/)

Type checking XML Papers:

- [XDuce: A Statically Typed XML Processing Language](https://classes.cs.uoregon.edu/04W/cis607dsl/papers/xduce-toit.pdf)
- [A typed representation for HTML and XML documents in Haskell](https://doi.org/10.1017/S0956796802004392 )
- [Typing XHTML Web Applications in ML](https://elsman.com/pdf/padl2004.pdf)
- [The Design Space of Type Checkers for XML Transformation Languages](https://www.cs.au.dk/~amoeller/papers/xmltypes/xmltypes.pdf)
- [Many holes in Hindley-Milner](https://homepages.inf.ed.ac.uk/slindley/papers/many-holes-draft2008.pdf)
