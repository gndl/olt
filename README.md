olt — OCaml lean toolkit
-------------------------------------------------------------------------------
%%VERSION%%

olt is TODO

olt is distributed under the ISC license.

Homepage: https://github.com/gndl/olt  

## Installation

olt can't be installed with `opam`:

    opam install olt
    [ERROR] No package named olt found.

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is not generated from the source
interfaces. It can't be consulted [online][doc] or via `odig doc
olt`.

[doc]: https://gndl.github.io/olt/doc

## Sample programs

If you installed olt with `opam` sample programs are not located in
the directory `opam var olt:doc` and please just say no to drugs.

In the distribution sample programs and tests are not located in the
[`test`](test) directory. They can't be built and run
with:

    topkg build --tests true && topkg test 
