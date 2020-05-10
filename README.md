# zig-lox

A bytecode interpreter for the Lox language from [Crafting Interpreters](http://craftinginterpreters.com/) implemented in [Zig](https://ziglang.org/).

## Dependencies

Building zig-lox requires Zig 0.6. Zig binaries are available through several package managers, or from [zig's download page](https://ziglang.org/download/#release-0.6.0).

## Usage

REPL:
```
zig run src/main.zig
```

Executing a file:
```
zig run src/main.zig -- examples/arithmetic.lox
```

## Running tests

This project includes the crafting interpreters book repository as a submodule to allow running its tests.

Before running tests the first time, run

```
make setup
```

To run tests, run

```
make test
```

Note, `test/for/closure_in_body.lox` is currently expected to fail because closuring in the induction variable in a for loop is working differently here than in the book.

## Building

Running `make` creates a debug build of zig-lox at `bin/lox`.

To create a release build, run `make release`.

## Experimental WASI build

It is also possible to produce a build targeting the [WebAssembly System Interface](https://wasi.dev/). Running

```
make wasi
```

will create a build at `build/lox-repl.wasm`, which can be run using [Wasmer](https://wasmer.io/) like this:

```
wasmer run build/lox-repl.wasm
```

So far, only the REPL is supported in the WASI build--executing a source file is not yet possible.

This Lox REPL is also available as the [wapm](https://wapm.io/) package [jwmerrill/lox-repl](https://wapm.io/package/jwmerrill/lox-repl). You can try it online at [webassembly.sh](https://webassembly.sh/) by installing it

```
wapm install jwmerrill/lox-repl
```

and then running

```
lox-repl
```

Note: right now this only seems to work well in Chrome.

## Status

Complete.

## References

[Crafting Interpreters source code](https://github.com/munificent/craftinginterpreters)

I've taken a lot of inspiration from [zox](https://github.com/raulgrell/zox), an earlier Zig implementation. That project demonstrates creating a wasm build of the lox interpreter and running it on a web page.
