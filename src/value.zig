const std = @import("std");
const Obj = @import("./object.zig").Obj;

pub const Value = union(enum) {
    Bool: bool,
    Nil,
    Number: f64,
    Obj: *Obj,

    pub fn isFalsey(self: Value) bool {
        return switch (self) {
            .Bool => |x| !x,
            .Nil => true,
            .Number => false,
            .Obj => false,
        };
    }

    pub fn equals(aBoxed: Value, bBoxed: Value) bool {
        return switch (aBoxed) {
            .Bool => |a| {
                return switch (bBoxed) {
                    .Bool => |b| a == b,
                    else => false,
                };
            },
            .Nil => |a| {
                return switch (bBoxed) {
                    .Nil => true,
                    else => false,
                };
            },
            .Number => |a| {
                return switch (bBoxed) {
                    .Number => |b| a == b,
                    else => false,
                };
            },
            .Obj => |a| {
                return switch (bBoxed) {
                    .Obj => |b| a == b,
                    else => false,
                };
            },
        };
    }
};

pub fn printValue(boxed: Value) void {
    switch (boxed) {
        .Number => |value| std.debug.warn("{}", .{value}),
        .Bool => |value| std.debug.warn("{}", .{value}),
        .Nil => std.debug.warn("nil", .{}),
        .Obj => |obj| switch (obj.data) {
            .String => |str| std.debug.warn("\"{}\"", .{str.bytes}),
            .Function => |fun| {
                const name = if (fun.name) |str| str.bytes else "<script>";
                std.debug.warn("<fn {}>", .{name});
            },
        },
    }
}
