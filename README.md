# jcc

![Demo](breakout.gif)

This is an exercise in writing a compiler for a language heavily inspired by Jonathan Blow's
[JAI](https://www.youtube.com/watch?v=uZgbKrDEzAs). While this will probably never be a production compiler
I do want to make it as good as I can, and use it to write some non-trivial programs.

As you can see from the demo, the language can actually run programs (and call C, see `raylib.jpl`).
Right now everything is running inside the IR interpreter during compilation
(I'm still making the C backend).

## Build and Run

To build the compiler and all it's dependencies run `sh build.sh`.

To run the compiler `./jcc test/breakout.jpl`
