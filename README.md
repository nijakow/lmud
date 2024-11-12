# LMud

## Description

This is *LMud*, a multi-user implementation of [about half of](https://en.wikipedia.org/wiki/Greenspun%27s_tenth_rule) [Common Lisp](https://en.wikipedia.org/wiki/Common_Lisp).

It was written to act as a programming language to build [Multi User Dungeons](https://en.wikipedia.org/wiki/Multi-user_dungeon) and similar software, and acts as a replacement for the [Raven](https://github.com/nijakow/raven) driver series.

## Building and Running

To build LMud, it should be sufficient to type `make run` in the `src/lmud/` directory of this repository. You should only need a C compiler (GCC or clang), GNU Make, and a POSIX-compatible build environment.

To connect to the system, use the protocol and port indicated by the logs in the terminal.

Press `Ctrl-C` to stop the execution.
