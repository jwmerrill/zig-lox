const builtin = @import("builtin");

pub const IS_WASM_FREESTANDING = builtin.target.isWasm() and builtin.target.os.tag == .freestanding;
