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
	zig build-lib src/main_wasm_freestanding.zig -target wasm32-freestanding --output-dir build --release-small

.PHONY: www
www: wasm
	cp build/main_wasm_freestanding.wasm www/build/

.PHONY: www-server
www-server: www
	cd www && python3 -m http.server

test: lox
	mkdir -p craftinginterpreters/build
	cp bin/lox craftinginterpreters/build/cloxd
	cd craftinginterpreters && ./util/test.py clox

setup:
	git submodule update --init
	cd craftinginterpreters && make setup
