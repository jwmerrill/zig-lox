const std = @import("std");
pub const Value = f64;

pub fn printValue(value: Value) void {
    std.debug.warn("{}", value);
}
