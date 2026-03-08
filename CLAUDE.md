# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands

```bash
make              # Debug build → bin/lox
make release      # ReleaseFast build → bin/lox
make test         # Run full test suite (244 tests)
make benchmark    # Run benchmarks (requires release build)
make wasm         # WASM freestanding → build/wasm-lib.wasm
make wasi         # WASI build → build/lox-repl.wasm
make www-server   # Build WASM and serve web REPL on localhost:8000
```

Requires Zig 0.15. The `ZIG` variable in the Makefile can override the zig binary path.

## Architecture

Bytecode interpreter for the Lox language (from *Crafting Interpreters*), implemented in Zig.

**Pipeline:** Source → Scanner → Compiler (recursive descent + Pratt parsing) → Bytecode → VM

Key source files in `src/`:
- **scanner.zig** — Lexical analysis, keyword detection via switch on first character
- **compiler.zig** — Parser and bytecode code generation. Handles closures/upvalues, classes, methods
- **vm.zig** — Stack-based virtual machine with 44 opcodes. Manages call frames, globals, GC roots
- **chunk.zig** — Bytecode container (opcodes + constants + line numbers)
- **value.zig** — Value representation with NaN-boxing (controlled by `debug.zig:NAN_BOXING`)
- **object.zig** — Runtime heap objects (String, Function, Closure, Upvalue, Class, Instance, BoundMethod). Uses struct-inheritance pattern with `@fieldParentPtr`
- **memory.zig** — GCAllocator wrapping a backing allocator. Mark-and-sweep GC with intrusive gray list. Threshold starts at 1MB, doubles each cycle
- **table.zig** — String-keyed hash table (not generic) for globals and object fields
- **writer.zig** — VMWriter type (`*std.Io.Writer`) and WasmWriter (custom `std.Io.Writer` VTable for WASM host I/O)
- **wasm-lib.zig** — WASM entry point exporting createVM/destroyVM/interpret/run. Imports writeOut/writeErr/now from host

## Debug Flags

Toggle in `src/debug.zig`: `PRINT_CODE`, `TRACE_EXECUTION`, `STRESS_GC`, `LOG_GC`, `TESTING_ALLOCATOR`, `NAN_BOXING`.

## Test Infrastructure

`util/test.zig` parses special comments in `.lox` test files:
- `// expect: <output>` — expected stdout line
- `// Error` or `// [line N] Error` — expected compile error
- `// expect runtime error: <msg>` — expected runtime error
- `// expect exit code: <N>` — expected exit code

Skipped tests:
- `test/for/closure_in_body.lox` — zig-lox handles loop variable closure differently than clox
- `test/benchmark/string_equality.lox` — hits constant pool limit

## Allocator Conventions

Zig allocator choice matters — see https://ziglang.org/documentation/0.15.0/#Choosing-an-Allocator for background.

**Backing allocator** (selected in `main.zig`): `DebugAllocator` in Debug/ReleaseSafe, `smp_allocator` in ReleaseFast/ReleaseSmall, `GeneralPurposeAllocator` in WASM. The VM wraps whatever backing allocator it receives in `GCAllocator` (`memory.zig`), which intercepts allocations to trigger mark-and-sweep GC.

**Key rules:**
- All runtime object allocations must go through `vm.allocator` (the GCAllocator) so the GC can track heap usage
- The VM stack uses the backing allocator directly (not GCAllocator) because upvalue `*Value` pointers prevent dynamic stack resizing
- Test/benchmark utilities (`util/test.zig`, `util/benchmark.zig`) use `ArenaAllocator` backed by `page_allocator` — appropriate since they spawn child processes and need simple bulk-free semantics, not general-purpose allocation
- When writing standalone utilities (test harnesses, benchmarks, build scripts), prefer `ArenaAllocator` over `GeneralPurposeAllocator` when all memory can be freed at once or in phases. Use `page_allocator` as the backing allocator for arenas in these contexts
- `GeneralPurposeAllocator` is for long-lived processes needing individual free/realloc (like the WASM host interface). Don't use it where an arena suffices

## Notable Design Decisions

- **NaN-boxing** enabled by default: values packed into 64-bit doubles
- **Intrusive linked list for GC gray stack** instead of growable array, preventing OOM during collection
- **Struct inheritance via `@fieldParentPtr`** for object type casting
- **Upvalues store `*Value` pointers** to either stack or heap; this prevents dynamic stack resizing
- **String interning** through global `strings` table in `table.zig`
- **No string escape sequences** (by design from the book)
