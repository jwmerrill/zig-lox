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

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: var) !void {
        switch (self) {
            .Number => |value| try out_stream.print("{d}", .{value}),
            .Bool => |value| try out_stream.print("{}", .{value}),
            .Nil => try out_stream.print("nil", .{}),
            .Obj => |obj| switch (obj.objType) {
                .String => try out_stream.print("{}", .{obj.asString().bytes}),
                .Function => {
                    const name = if (obj.asFunction().name) |str| str.bytes else "<script>";
                    try out_stream.print("<fn {}>", .{name});
                },
                .NativeFunction => {
                    try out_stream.print("<native fn>", .{});
                },
                .Closure => {
                    const name = if (obj.asClosure().function.name) |str| str.bytes else "<script>";
                    try out_stream.print("<fn {}>", .{name});
                },
                .Upvalue => {
                    try out_stream.print("upvalue", .{});
                },
                .Class => {
                    try out_stream.print("{}", .{obj.asClass().name.bytes});
                },
                .Instance => {
                    try out_stream.print("{} instance", .{obj.asInstance().class.name.bytes});
                },
                .BoundMethod => {
                    const name = if (obj.asBoundMethod().method.function.name) |str| str.bytes else "<script>";
                    try out_stream.print("<fn {}>", .{name});
                },
            },
        }
    }
};
