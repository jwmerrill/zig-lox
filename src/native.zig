const std = @import("std");
const Value = @import("./value.zig").Value;
const env = @import("./env.zig");

fn clockNative(args: []const Value) Value {
    _ = args;
    return Value.fromNumber(@as(f64, @floatFromInt(std.time.milliTimestamp())) / 1000);
}

extern fn now() f64;

fn clockWasm(args: []const Value) Value {
    _ = args;
    return Value.fromNumber(now() / 1000);
}

pub const clock = if (env.IS_WASM_FREESTANDING) clockWasm else clockNative;
