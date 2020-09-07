var inputElt = document.getElementById('input');
var outputElt = document.getElementById('output');

const encoder = new TextEncoder();
const decoder = new TextDecoder();

const importObject = {
  env: {
    writeOut: (ptr, len) => {
      outputElt.innerText += decoder.decode(
        new Uint8Array(wasm.memory.buffer.slice(ptr, ptr + len))
      );
    },
    writeErr: (ptr, len) => {
      outputElt.innerText += decoder.decode(
        new Uint8Array(wasm.memory.buffer.slice(ptr, ptr + len))
      );
    },
    now: () => Date.now(),
  },
};

// Note, could convert to WebAssembly.instantiateStreaming once Safari
// gets support for that
//
// Ref: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WebAssembly/instantiateStreaming
fetch('./build/wasm-lib.wasm')
  .then((response) => response.arrayBuffer())
  .then((bytes) => WebAssembly.instantiate(bytes, importObject))
  .then((result) => {
    wasm = result.instance.exports;
    main(wasm);
  });

function main(wasm) {
  // Create a lox VM
  var vm = wasm.createVM();

  // Evaluate a JS string in the lox vm
  function interpretString(str) {
    var slice = allocateString(wasm, str);
    wasm.interpret(vm, slice.ptr, slice.len);
    wasm.dealloc(slice.ptr, slice.len);
  }

  inputElt.addEventListener('keydown', (evt) => {
    if (evt.key === 'Enter') {
      evt.preventDefault();

      // Move current input to the output display
      var value = inputElt.value;
      outputElt.innerText += ['> ', value, '\n'].join('');
      inputElt.value = '';

      // Intepret input, printing any outputs to the display
      interpretString(value);
    }
  });
}

function allocateString(wasm, str) {
  // convert source to Uint8Array
  const sourceArray = encoder.encode(str);

  // get memory from wasm
  const len = sourceArray.length;

  const ptr = wasm.alloc(len);
  if (ptr === 0) throw 'Cannot allocate memory';

  // copy sourceArray to wasm
  var memoryu8 = new Uint8Array(wasm.memory.buffer);
  for (let i = 0; i < len; ++i) {
    memoryu8[ptr + i] = sourceArray[i];
  }

  return { ptr, len };
}
