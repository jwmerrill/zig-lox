# NOTE, purposely skipping test/for/closure_in_body.lox because zig-lox
# handles closuring induction variable differently than clox
TEST_FILES=`find test -name "*.lox" \
  | grep -v test/benchmark \
	| grep -v test/scanning \
	| grep -v test/expressions \
	| grep -v test/for/closure_in_body.lox \
	`

all: lox

.PHONY: clean
clean:
	rm -rf bin/*
	rm -rf build/*
	rm -rf zig-cache
	cd craftinginterpreters && make clean

.PHONY: lox
lox:
	zig build --prefix '.'

.PHONY: release
release:
	zig build --prefix '.' -Drelease-fast=true

.PHONY: wasi
wasi:
	zig build-exe src/main.zig -target wasm32-wasi --output-dir build --name lox-repl --release-small

.PHONY: wasm
wasm:
	zig build-lib src/wasm-lib.zig -target wasm32-freestanding --output-dir build --release-small

.PHONY: www
www: wasm
	cp build/wasm-lib.wasm www/build/

.PHONY: www-server
www-server: www
	cd www && python3 -m http.server

test: lox
	zig run util/test.zig -- bin/lox $(TEST_FILES)
