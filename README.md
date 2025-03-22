# zig-lox

A bytecode interpreter for the Lox language from [Crafting Interpreters](http://craftinginterpreters.com/) implemented in [Zig](https://ziglang.org/).

## Dependencies

Building zig-lox requires Zig 0.12. Zig binaries are available through several package managers, or from [zig's download page](https://ziglang.org/download).

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

To run tests, run

```
make test
```

This runs the tests from the book using a test harness written in Zig. Note that `test/for/closure_in_body.lox` is currently skipped because closuring the induction variable of a loop works differently in zig-lox than in clox.

## Building

Running `make` creates a debug build of zig-lox at `bin/lox`.

To create a release build, run `make release`.

## Web Assembly

`zig-lox` can be compiled to a freestanding Web Assembly (WASM) library to run in a web browser or in node. Running

```
make wasm
```

will create a build at `build/wasm-lib.wasm`. An example of using the WASM library through node is provided in `js/main.js`. Similarly to the `lox` binary, this provides a REPL through

```
node js/main.js
```

and can execute a specified lox source file, for example:

```
node js/main.js examples/arithmetic.lox
```

## Web REPL

A simple web REPL powered by the WASM library is available in the `www` directory. [Try it live](https://www.shapeoperator.com/toys/lox-repl/).

To run the web REPL locally, run

```
make www-server
```

(note, requires Python 3 to be available), and visit `localhost:8000`.

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

I've taken a lot of inspiration from [zox](https://github.com/raulgrell/zox), an earlier Zig implementation.
