all: lox

.PHONY: clean
clean:
	rm -rf build
	rm -rf zig-cache
	cd craftinginterpreters && make clean

.PHONY: lox
lox:
	zig build --prefix ./build

.PHONY: fast
fast:
	zig build --prefix build -Drelease-fast=true

test: lox
	mkdir -p craftinginterpreters/build
	cp build/bin/lox craftinginterpreters/build/cloxd
	cd craftinginterpreters && ./util/test.py clox

setup:
	git submodule update --init
	cd craftinginterpreters && make setup