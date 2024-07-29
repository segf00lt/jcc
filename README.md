# jcc

_A poorly named JAI style language._

This is an exercise in writing a compiler for a language heavily inspired by Jonathan Blow's
[JAI](https://www.youtube.com/watch?v=uZgbKrDEzAs). While this will probably never be a production compiler
I do want to make it as good as I can, and use it to write some non-trivial programs.

As of writing this the language is not yet fully functional, but within at least one more month it should be.
Writing a compiler for a language of this kind is quite a bit of work, especially on first try
(not counting the half finished attempt I made in 2023). Despite this, it is very fun, and I'd encourage
anyone interested to open an issue with any questions or comments you might have.

## Build and Run

To build just run `make`. To run the compiler `./jcc`. At the moment I keep it hard-coded to compile one
of the tests in depending what I'm adding so if you'd like to run a different test just go to the bottom of
`jcc.c` and change the file path.

Only non-standard dependency you need is [raylib](https://github.com/raysan5/raylib), but this will be removed soon.
