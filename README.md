# flathead

This is a fork of Eric Lippert's Z-machine interpreter, "flathead". See also his blog series at [ericlippert.com](https://ericlippert.com/category/zmachine/).

This branch (`mirage`) is a port of flathead to run as a MirageOS unikernel.

There are also some other branches of interest in this repository:

- [`update-working`](https://github.com/mato/flathead/tree/update-working): An update of Eric's working code to use ocamlbuild, should work on any OCaml system with the Graphics package available.
- [`tty`](https://github.com/mato/flathead/tree/tty): A simplistic replacement of the Graphics code to use a terminal, should work on any UNIX-based OCaml system.

