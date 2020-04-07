const std = @import("std");

pub const ValueType = enum {
    Bool,
    Nil,
    Number,
};

pub const Value = union(ValueType) {
    Bool: bool,
    Nil,
    Number: f64,
};

pub fn printValue(boxed: Value) void {
    switch (boxed) {
        .Number => |value| std.debug.warn("{}", value),
        .Bool => |value| std.debug.warn("{}", value),
        .Nil => std.debug.warn("nil"),
    }
}
