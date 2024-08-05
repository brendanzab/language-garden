# Type preserving compilation for a simply typed functional language

The goal of this project is to demonstrate compilation of a simply typed
functional language to various low-level target languages. We aim to preserve
types as long as possible, up to code-generation time.

## Compiler overview

| Language      | Description                                                     |
| ------------- | --------------------------------------------------------------- |
| [`Core`]      | Simply typed lambda calculus with booleans and integers         |
| [`Anf`]       | STLC with an explicit evaluation order (A-normal form)          |
| [`Clos`]      | STLC in A-normal form with explicit closures                    |
| [`Monadic`]   | STLC with a separation between values and effectful expressions |

[`Core`]: ./Core.ml
[`Anf`]: ./Anf.ml
[`Clos`]: ./Clos.ml
[`Monadic`]: ./Monadic.ml

| Translation        |   | Source       |   | Target        |
| ------------------ | - | ------------ | - | ------------- |
| [`CoreToAnf`]      | : | [`Core`]     | → | [`Anf`]       |
| [`CoreToMonadic`]  | : | [`Core`]     | → | [`Monadic`]   |

[`CoreToAnf`]: ./CoreToAnf.ml
[`CoreToMonadic`]: ./CoreToMonadic.ml

## Todo list

Intermediate languages:

- [ ] Surface language
- [x] Core language ([`Core`])
- [x] ANF language ([`Anf`])
- [x] ANF + CC language ([`Clos`])
- [x] Monadic language ([`Monadic`])
- [ ] SSA language
- [ ] TAL language

Translations:

- [ ] Lexer
- [ ] Parser
- [ ] Elaboration
- [x] ANF-translation ([`CoreToAnf`](CoreToAnf.ml))
- [x] Monadic translation ([`CoreToMonadic`](CoreToMonadic.ml))
- [ ] Closure conversion
- [ ] Hoisting
- [ ] Single static assignment

Code generation targets:

- [ ] WASM codegen
- [ ] LLVM codegen
- [ ] Cranelift codegen

Other features:

- [ ] Type checking for IRs
- [ ] Example based tests
- [ ] Type-directed property tests
- [ ] Optimisations on intermediate languages

## Proposed compiler pipeline

```text
    ╭────────────────╮
    │  Surface.expr  │
    ╰────────────────╯
            │
    Surface.Elab.synth
            │
            ▼
    ╭────────────────╮
    │   Core.expr    │
    ╰────────────────╯
            │
            ├─────────────────────────────╮
            │                             │
   Core.ToAnf.translate         Core.ToMonadic.translate
            │                             │
            ▼                             ▼
    ╭────────────────╮            ╭────────────────╮
    │    Anf.expr    │            │  Monadic.expr  │
    ╰────────────────╯            ╰────────────────╯
            │
  Anf.ToAnfClos.translate
            │
            │     ╭───────────────╮
            ▼     ▼               │
    ╭────────────────╮            │
    │    Clos.expr   │   Clos.Hoist.translate
    ╰────────────────╯            │
            │     │               │
            │     ╰───────────────╯
            │
            ├─────────────────┬──── Clos.ToWasm.translate ─────────▶︎ ...
            │                 │
            │                 ├──── Clos.ToLlvm.translate ─────────▶︎ ...
            │                 │
            │                 ╰──── Clos.ToCranelift.translate ────▶︎ ...
            │
   Clos.ToSsa.translate
            │
            ▼
    ╭────────────────╮
    │   Ssa.prog     │
    ╰────────────────╯
            │
   Ssa.ToTal.translate
            │
    ╭────────────────╮
    │   Tal.prog     │
    ╰────────────────╯
            │
            ╰──── Tal.To<AsmTarget>.translate ────▶︎ ...
```
