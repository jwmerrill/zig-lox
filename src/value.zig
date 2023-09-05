const std = @import("std");
const Obj = @import("./object.zig").Obj;
const NAN_BOXING = @import("./debug.zig").NAN_BOXING;

pub const Value = if (NAN_BOXING) NanBoxedValue else UnionValue;

pub const NanBoxedValue = packed struct {
    data: u64,

    const SIGN_BIT: u64 = 0x8000000000000000;
    const QNAN: u64 = 0x7ffc000000000000;

    const TAG_NIL = 1; // 01.
    const TAG_FALSE = 2; // 10.
    const TAG_TRUE = 3; // 11.

    const NIL_VAL = NanBoxedValue{ .data = QNAN | TAG_NIL };
    const TRUE_VAL = NanBoxedValue{ .data = QNAN | TAG_TRUE };
    const FALSE_VAL = NanBoxedValue{ .data = QNAN | TAG_FALSE };

    pub fn isBool(self: NanBoxedValue) bool {
        return (self.data & FALSE_VAL.data) == FALSE_VAL.data;
    }

    pub fn isNil(self: NanBoxedValue) bool {
        return self.data == NIL_VAL.data;
    }

    pub fn isNumber(self: NanBoxedValue) bool {
        return (self.data & QNAN) != QNAN;
    }

    pub fn isObj(self: NanBoxedValue) bool {
        return (self.data & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT);
    }

    pub fn asNumber(self: NanBoxedValue) f64 {
        std.debug.assert(self.isNumber());
        return @as(f64, @bitCast(self.data));
    }

    pub fn asBool(self: NanBoxedValue) bool {
        std.debug.assert(self.isBool());
        return self.data == TRUE_VAL.data;
    }

    pub fn asObj(self: NanBoxedValue) *Obj {
        std.debug.assert(self.isObj());
        return @as(*Obj, @ptrFromInt(@as(usize, @intCast(self.data & ~(SIGN_BIT | QNAN)))));
    }

    pub fn fromNumber(x: f64) NanBoxedValue {
        return NanBoxedValue{ .data = @as(u64, @bitCast(x)) };
    }

    pub fn fromBool(x: bool) NanBoxedValue {
        return if (x) TRUE_VAL else FALSE_VAL;
    }

    pub fn fromObj(x: *Obj) NanBoxedValue {
        return NanBoxedValue{ .data = SIGN_BIT | QNAN | @intFromPtr(x) };
    }

    pub fn nil() NanBoxedValue {
        return NIL_VAL;
    }

    pub fn isFalsey(self: NanBoxedValue) bool {
        if (self.isBool()) return !self.asBool();
        if (self.isNil()) return true;
        return false;
    }

    pub fn equals(self: NanBoxedValue, other: NanBoxedValue) bool {
        // Be careful about IEEE NaN equality semantics
        if (self.isNumber() and other.isNumber()) return self.asNumber() == other.asNumber();
        return self.data == other.data;
    }

    pub fn format(self: NanBoxedValue, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        _ = fmt;
        _ = options;
        if (self.isNumber()) {
            try out_stream.print("{d}", .{self.asNumber()});
        } else if (self.isBool()) {
            try out_stream.print("{}", .{self.asBool()});
        } else if (self.isNil()) {
            try out_stream.print("nil", .{});
        } else {
            const obj = self.asObj();
            try printObject(obj, out_stream);
        }
    }
};

pub const UnionValue = union(enum) {
    Bool: bool,
    Nil,
    Number: f64,
    Obj: *Obj,

    pub fn isBool(self: UnionValue) bool {
        return self == .Bool;
    }

    pub fn isNil(self: UnionValue) bool {
        return self == .Nil;
    }

    pub fn isNumber(self: UnionValue) bool {
        return self == .Number;
    }

    pub fn isObj(self: UnionValue) bool {
        return self == .Obj;
    }

    pub fn asBool(self: UnionValue) bool {
        std.debug.assert(self.isBool());
        return self.Bool;
    }

    pub fn asNumber(self: UnionValue) f64 {
        std.debug.assert(self.isNumber());
        return self.Number;
    }

    pub fn asObj(self: UnionValue) *Obj {
        std.debug.assert(self.isObj());
        return self.Obj;
    }

    pub fn fromBool(x: bool) UnionValue {
        return UnionValue{ .Bool = x };
    }

    pub fn nil() UnionValue {
        return .Nil;
    }

    pub fn fromNumber(x: f64) UnionValue {
        return UnionValue{ .Number = x };
    }

    pub fn fromObj(x: *Obj) UnionValue {
        return UnionValue{ .Obj = x };
    }

    pub fn isFalsey(self: UnionValue) bool {
        return switch (self) {
            .Bool => |x| !x,
            .Nil => true,
            .Number => false,
            .Obj => false,
        };
    }

    pub fn equals(aBoxed: UnionValue, bBoxed: UnionValue) bool {
        return switch (aBoxed) {
            .Bool => |a| {
                return switch (bBoxed) {
                    .Bool => |b| a == b,
                    else => false,
                };
            },
            .Nil => {
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

    pub fn format(self: UnionValue, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .Number => |value| try out_stream.print("{d}", .{value}),
            .Bool => |value| try out_stream.print("{}", .{value}),
            .Nil => try out_stream.print("nil", .{}),
            .Obj => |obj| try printObject(obj, out_stream),
        }
    }
};

// Shared between the two value representations
fn printObject(obj: *Obj, out_stream: anytype) !void {
    switch (obj.objType) {
        .String => try out_stream.print("{s}", .{obj.asString().bytes}),
        .Function => {
            const name = if (obj.asFunction().name) |str| str.bytes else "<script>";
            try out_stream.print("<fn {s}>", .{name});
        },
        .NativeFunction => {
            try out_stream.print("<native fn>", .{});
        },
        .Closure => {
            const name = if (obj.asClosure().function.name) |str| str.bytes else "<script>";
            try out_stream.print("<fn {s}>", .{name});
        },
        .Upvalue => {
            try out_stream.print("upvalue", .{});
        },
        .Class => {
            try out_stream.print("{s}", .{obj.asClass().name.bytes});
        },
        .Instance => {
            try out_stream.print("{s} instance", .{obj.asInstance().class.name.bytes});
        },
        .BoundMethod => {
            const name = if (obj.asBoundMethod().method.function.name) |str| str.bytes else "<script>";
            try out_stream.print("<fn {s}>", .{name});
        },
    }
}
