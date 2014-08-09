## Synopsis

This generates `iptables` shell scripts from a domain specific language
which

  - provides variable definitions and control structures at the
    outer level while staying close to the `iptables` command in the
    details.
  - supports IP numbers, networks, and unions of networks as first-class objects
    which can be manipulated by set-operators.
  - allows literal host names which will be resolved at
    compile-time, so that the final script is independent of DNS lookups.

## Installation

This package can be installed with [opam](http://opam.ocaml.org/),

    opam repo add paurkedal https://github.com/paurkedal/opam-repo-paurkedal.git
    opam install iplogic

It provides two programs `iplogic`, which compiles scripts into shell code,
and `iplogic-depend`, which generates dependencies to include in
`Makefile`s.
