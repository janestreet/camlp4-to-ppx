Camlp4 to PPX conversion tool
=============================

The aim of this project is to automatically convert OCaml source files
using camlp4 syntax extensions to their equivalent in the ppx world
while preserving comments and the layout. We used a variation of this
tool at Jane Street to convert our code base.

For instance it will translate:

```ocaml
type t =
  | A of int (* blah *)
  | B of string
  with sexp, bin_io

let x = <:sexp_of< t * int >> (A 42, 10)
```

into:

```ocaml
type t =
  | A of int (* blah *)
  | B of string
  [@@deriving sexp, bin_io]

let x = [%sexp_of: t * int ] (A 42, 10)
```

For each syntax extension to convert a plugin needs to be written.

How does it work?
-----------------

It works by writing a dummy camlp4 syntax extension that only register
substitutions using locations. After parsing the input file all the
substitutions are applied to the original file and the result is
printed.

To add a new plugin, add a file to the plugins/ directory and the
corresponding line in `bin/main.ml`. `pa_ounit.ml` should be a good
model to understand how it works.
