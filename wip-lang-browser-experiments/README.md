# Browser experiments

I’d like to be able to do some interactive experiments in that browser.
This project is a playground to experiment with these ideas using
[js_of_ocaml](https://github.com/ocsigen/js_of_ocaml/).

```command
$ dune build wip-lang-browser-experiments
$ python -m http.server 8000 --bind localhost --directory _build/default/wip-lang-browser-experiments
```

Then navigate to <http://[::1]:8000/>.

## Todo list

- [x] Basic build setup
- [ ] Hot reloading

## Resources

- [Js_of_ocaml - Reference Manual](https://ocsigen.org/js_of_ocaml/latest/manual/overview)
- [JavaScript Compilation With Js_of_ocaml](https://dune.readthedocs.io/en/stable/jsoo.html)
- [JavaScript Compilation With Melange](https://dune.readthedocs.io/en/stable/melange.html)
