# jcc

![Demo](demo.gif)

_A compiler for a JAI style language._

This is an exercise in writing a compiler for a language heavily inspired by Jonathan Blow's
[JAI](https://www.youtube.com/watch?v=uZgbKrDEzAs). While this will probably never be a production compiler
I do want to make it as good as I can, and use it to write some non-trivial programs.

As you can see from the demo, the language can actually run programs (and call C, see `raylib.jpl`).
Right now everything is running inside the IR interpreter during compilation while I make the assembly backend.

## Build and Run

To build just run `make`. To run the compiler `./jcc tests/<some_test>`

Only non-standard dependency you need is [raylib](https://github.com/raysan5/raylib), but this will be removed soon.
