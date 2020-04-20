all: lox

.PHONY: lox
lox:
	zig build-exe src/main.zig --name lox --output-dir build

test: lox
	mkdir -p craftinginterpreters/build
	cp build/lox craftinginterpreters/build/cloxd
	cd craftinginterpreters && ./util/test.py clox

setup:
	git submodule update --init
	cd craftinginterpreters && make setup