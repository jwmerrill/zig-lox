const std = @import("std");
const builtin = @import("builtin");

pub const IS_WASM_FREESTANDING = std.Target.Cpu.Arch.isWasm(builtin.target.cpu.arch) and builtin.target.os.tag == .freestanding;
