# flathead

This is a fork of Eric Lippert's Z-machine interpreter written in OCaml.

This branch (`mirage`) is a port of flathead to run as a MirageOS unikernel.

There are also some other branches of interest in this repository:

- `update-working`: An update of Eric's working code to use ocamlbuild, should work on any OCaml system with the Graphics package available.
- `tty`: A simplistic replacement of the Graphics code to use a terminal, should work on any UNIX-based OCaml system.

