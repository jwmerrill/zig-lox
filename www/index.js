var inputElt = document.getElementById('input');
var outputElt = document.getElementById('output');

const encoder = new TextEncoder();
const decoder = new TextDecoder();

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

function main(wasm) {
  var vm = wasm.createVM();
  function interpretString(str) {
    var slice = allocateString(wasm, str);
    wasm.interpret(vm, slice.ptr, slice.len);
    wasm.dealloc(slice.ptr, slice.len);
  }

  inputElt.addEventListener('keydown', (evt) => {
    if (evt.key === 'Enter') {
      var value = inputElt.value;
      evt.preventDefault();
      outputElt.innerText += ['> ', value, '\n'].join('');
      inputElt.value = '';

      interpretString(value);
    }
  });
}

function decode(arr) {
  return decoder.decode(arr);
}

WebAssembly.instantiateStreaming(fetch('./build/main_wasm_freestanding.wasm'), {
  env: {
    writeOut: (ptr, len) => {
      outputElt.innerText += decode(
        new Uint8Array(wasm.memory.buffer.slice(ptr, ptr + len))
      );
    },
    writeErr: (ptr, len) => {
      outputElt.innerText += decode(
        new Uint8Array(wasm.memory.buffer.slice(ptr, ptr + len))
      );
    },
    now: () => Date.now(),
  },
}).then((result) => {
  wasm = result.instance.exports;
  main(wasm);
});
