# Plant growth DSL

This is a plant growth DSL based on tree rewriting.

```command
$ dune build lang-fractal-tree-rewriting
$ python -m http.server 8000 --bind localhost --directory _build/default/lang-fractal-tree-rewriting
```

Then navigate to <http://[::1]:8000/>.

## Todo list

Language features

- [x] Deterministic systems
- [ ] Nondeterministic systems
- [ ] Stochastic systems
- [ ] Context-sensitive systems

Language tooling

- [x] Internal DSL
- [ ] External DSL
- [ ] Web Playground
- [ ] Component library
- [ ] Render components and transitions separately
- [ ] Rule stepper
