# MiniSAT in OCaml

[![ptal on Travis CI][travis-image]][travis]

[travis-image]: https://travis-ci.org/ptal/minisatml.png
[travis]: https://travis-ci.org/ptal/minisatml

This project is a SAT solver inspired by MiniSAT, written by Albin Coquereau in the context of its Ph.D. thesis on Alt-Ergo.
[Albin's initial project](https://github.com/OCamlPro-Coquera/minisatml) has been forked in this repository in order to be adapted for being used as an abstract domain in the [AbSolute solver](https://github.com/ptal/AbSolute).
Only a library is available, and benchmark can be performed through [kobe](https://github.com/ptal/kobe) using the [kobe-sat](https://github.com/ptal/kobe-sat) data sets.

This library can be installed via OPAM as follows:

```sh
apt-get install opam # on Debian, see opam documentation for other distributions.
opam init --comp 4.07.1+flambda # Initialize ~/.opam with a freshly compiled OCaml 4.07.1

opam repo add solvers git@github.com:ptal/solvers-opam.git
opam install minisatml
```
