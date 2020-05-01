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

pub fn printValue(boxed: Value) !void {
    const stdout = std.io.getStdOut().outStream();

    switch (boxed) {
        .Number => |value| try stdout.print("{d}", .{value}),
        .Bool => |value| try stdout.print("{}", .{value}),
        .Nil => try stdout.print("nil", .{}),
        .Obj => |obj| switch (obj.objType) {
            .String => try stdout.print("{}", .{obj.asString().bytes}),
            .Function => {
                const name = if (obj.asFunction().name) |str| str.bytes else "<script>";
                try stdout.print("<fn {}>", .{name});
            },
            .NativeFunction => {
                try stdout.print("<native fn>", .{});
            },
            .Closure => {
                const name = if (obj.asClosure().function.name) |str| str.bytes else "<script>";
                try stdout.print("<fn {}>", .{name});
            },
            .Upvalue => {
                try stdout.print("upvalue", .{});
            },
            .Class => {
                try stdout.print("{}", .{obj.asClass().name.bytes});
            },
            .Instance => {
                try stdout.print("{} instance", .{obj.asInstance().class.name.bytes});
            },
        },
    }
}
