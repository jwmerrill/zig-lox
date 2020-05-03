all: lox

.PHONY: clean
clean:
	rm -rf bin/*
	rm -rf zig-cache
	cd craftinginterpreters && make clean

.PHONY: lox
lox:
	zig build --prefix '.'

.PHONY: release
release:
	zig build --prefix '.' -Drelease-fast=true

test: lox
	mkdir -p craftinginterpreters/build
	cp bin/lox craftinginterpreters/build/cloxd
	cd craftinginterpreters && ./util/test.py clox

setup:
	git submodule update --init
	cd craftinginterpreters && make setup
