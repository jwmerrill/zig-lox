# NOTE, purposely skipping test/for/closure_in_body.lox because zig-lox
# handles closuring induction variable differently than clox
TEST_FILES=`find test -name "*.lox" \
	| grep -v test/benchmark \
	| grep -v test/scanning \
	| grep -v test/expressions \
	| grep -v test/for/closure_in_body.lox \
	`

ZIG=zig

all: lox

.PHONY: clean
clean:
	rm -rf bin/*
	rm -rf build/*
	rm -rf zig-cache
	rm -rf src/zig-cache

.PHONY: directories
	mkdir -p build

.PHONY: lox
lox:
	$(ZIG) build --prefix '.'

.PHONY: release
release:
	$(ZIG) build --prefix '.' -Doptimize=ReleaseFast

.PHONY: wasi
wasi: directories
	cd build && \
	$(ZIG) build-exe ../src/main.zig -target wasm32-wasi --name lox-repl -O ReleaseSmall

.PHONY: wasm
wasm: directories
	cd build && \
	$(ZIG) build-exe ../src/wasm-lib.zig \
		-target wasm32-freestanding \
		-fno-entry \
		-rdynamic \
		-O ReleaseSmall

.PHONY: www
www: wasm
	cp build/wasm-lib.wasm www/build/

.PHONY: www-server
www-server: www
	cd www && python3 -m http.server

test: lox
	$(ZIG) run util/test.zig -- bin/lox $(TEST_FILES)

.PHONY: version
version:
	$(ZIG) version
