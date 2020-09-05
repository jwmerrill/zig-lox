const std = @import("std");

pub const IS_WASM_FREESTANDING = std.Target.current.isWasm() and std.Target.current.os.tag == .freestanding;
