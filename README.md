A bytecode interpreter for the Lox language from [Crafting Interpreters](http://craftinginterpreters.com/) implemented in [Zig](https://ziglang.org/).

### Usage

REPL:
```
zig run src/main.zig
```

Executing a file:
```
zig run src/main.zig -- examples/arithmetic.lox
```

### Running tests

This project includes the crafting interpreters book repository as a submodule to allow running its tests.

Before running tests the first time, run

```
make setup
```

To run tests, run

```
make test
```

### Status

Incomplete

### Ref

[Crafting Interpreters source code](https://github.com/munificent/craftinginterpreters)

I've taken a lot of inspiration from [zox](https://github.com/raulgrell/zox), an earlier Zig implementation.
