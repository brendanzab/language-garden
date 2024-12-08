# Type preserving compilation for a simply typed functional language

The goal of this project is to demonstrate compilation of a simply typed
functional language to various low-level target languages. We aim to preserve
types as long as possible, up to code-generation time.

## Compiler overview

| Language      | Description                                                     |
| ------------- | --------------------------------------------------------------- |
| [`Surface`]   | User-friendly surface language                                  |
| [`Core`]      | Simple, minimal core language                                   |
| [`Anf`]       | STLC with an explicit evaluation order (A-normal form)          |
| [`Clos`]      | STLC in A-normal form with explicit closures                    |
| [`Monadic`]   | STLC with a separation between values and effectful expressions |

[`Surface`]: ./surface.ml
[`Core`]: ./core.ml
[`Anf`]: ./anf.ml
[`Clos`]: ./clos.ml
[`Monadic`]: ./monadic.ml

| Translation          |   | Source       |   | Target        |
| -------------------- | - | ------------ | - | ------------- |
| [`Core_to_anf`]      | : | [`Core`]     | → | [`Anf`]       |
| [`Core_to_monadic`]  | : | [`Core`]     | → | [`Monadic`]   |

[`Core_to_anf`]: ./core_to_anf.ml
[`Core_to_monadic`]: ./core_to_monadic.ml

## Todo list

Intermediate languages:

- [x] Surface language ([`Surface`])
- [x] Core language ([`Core`])
- [x] ANF language ([`Anf`])
- [x] ANF + CC language ([`Clos`])
- [x] Monadic language ([`Monadic`])
- [ ] SSA language
- [ ] TAL language

Translations:

- [x] Lexer
- [x] Parser
- [x] Elaboration to core ([`Surface`])
- [x] ANF-translation ([`Core_to_anf`])
- [x] Monadic translation ([`Core_to_monadic`])
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
            ╭─────────────┴───────────────╮
            │                             │
   Core.To_anf.translate         Core.To_monadic.translate
            │                             │
            ▼                             ▼
    ╭────────────────╮            ╭────────────────╮
    │    Anf.expr    │            │  Monadic.expr  │
    ╰────────────────╯            ╰────────────────╯
            │
   Anf.To_clos.translate
            │
            │     ╭───────────────╮
            ▼     ▼               │
    ╭────────────────╮            │
    │    Clos.expr   │   Clos.Hoist.translate
    ╰────────────────╯            │
            │     │               │
            │     ╰───────────────╯
            │
            ├─────────────────┬──── Clos.To_wasm.translate ─────────▶︎ ...
            │                 │
            │                 ├──── Clos.To_llvm.translate ─────────▶︎ ...
            │                 │
            │                 ╰──── Clos.To_cranelift.translate ────▶︎ ...
            │
   Clos.To_ssa.translate
            │
            ▼
    ╭────────────────╮
    │   Ssa.prog     │
    ╰────────────────╯
            │
   Ssa.To_tal.translate
            │
    ╭────────────────╮
    │   Tal.prog     │
    ╰────────────────╯
            │
            ╰──── Tal.To_<asm_aarget>.translate ────▶︎ ...
```
