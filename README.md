# thermischd

Very simple daemon for controlling the CPU/MB temperature by calling helpers when certain trippoints are met.

## About

This program was written to allowing different methods of action to be performed when certain trippoints are met, including executing external programs for interfaces that are not exposed directly via sysfs or when the sysfs interface is broken.

Most programs currently available are either very specific to certain devices, do not have easy methods for configuration, or do not provide the ability to call external processes if needed.

Right now all settings are done in the source code, with defaults adjusted for Dell computers supported by the i8k driver and utitilies for fan management.

## Compiling

Compilation requires `base`, `stdio` and `ocamlfind` packages.

The `Makefile` is configured to use an OCaml compiler with LTO (link-time optimization) support by default, though this can be disabled by commenting out the `OCFLAGS` variable.

For setting up the default compilation environment, command `make opam` can be used.

## Installing

`sudo sh install.sh`
