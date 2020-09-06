const fs = require('fs');
const path = require('path');
const source = fs.readFileSync(
  path.join(__dirname, '..', 'build/main_wasm_freestanding.wasm')
);
const typedArray = new Uint8Array(source);

let wasm;
WebAssembly.instantiate(typedArray, {
  env: {
    writeOut: (ptr, len) => {
      process.stdout.write(new Uint8Array(wasm.memory.buffer.slice(ptr, ptr + len)));
    },
    writeErr: (ptr, len) => {
      process.stderr.write(new Uint8Array(wasm.memory.buffer.slice(ptr, ptr + len)));
    },
    now: () => Date.now()
  },
}).then((result) => {
  wasm = result.instance.exports
  main(wasm);
});

function main(wasm) {
  switch (process.argv.length) {
    case 3: {
      runSource(wasm, fs.readFileSync(process.argv[2], {encoding: 'utf-8'}));
    }
    default: {
      process.stdout.write('Usage: node main.js [path]\n');
      process.exit(1);
    }
  }
}

function runSource(wasm, source) {
  // convert source to Uint8Array
  const textEncoder = new TextEncoder();
  const sourceArray = textEncoder.encode(source);

  // get memory from wasm
  const source_len = sourceArray.length;

  const ptr = wasm.alloc(source_len);
  if (ptr === 0) {
    throw 'Cannot allocate memory';
  }

  // copy sourceArray to wasm
  var memoryu8 = new Uint8Array(wasm.memory.buffer);
  for (let i = 0; i < source_len; ++i) {
    memoryu8[ptr + i] = sourceArray[i];
  }

  // call function
  const exitCode = wasm.run(ptr, source_len);

  // dealloc function params
  wasm.dealloc(ptr, source_len);

  // throw if function returned error
  process.exit(exitCode);
}
