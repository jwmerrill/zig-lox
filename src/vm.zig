const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const printValue = @import("./value.zig").printValue;
const compile = @import("./compiler.zig").compile;
const verbose = @import("./debug.zig").verbose;
const Obj = @import("./object.zig").Obj;
const ObjString = @import("./object.zig").ObjString;
const Table = @import("./table.zig").Table;

fn add(x: f64, y: f64) f64 {
    return x + y;
}

fn sub(x: f64, y: f64) f64 {
    return x - y;
}

fn mul(x: f64, y: f64) f64 {
    return x * y;
}

fn div(x: f64, y: f64) f64 {
    return x / y;
}

pub const VM = struct {
    allocator: *Allocator,
    chunk: *Chunk,
    ip: usize, // NOTE, book uses a byte pointer for this
    stack: ArrayList(Value), // NOTE, book uses a fixed size stack
    objects: ?*Obj,
    strings: Table,

    pub fn init(allocator: *Allocator) VM {
        return VM{
            .allocator = allocator,
            .chunk = undefined,
            .ip = undefined,
            .stack = std.ArrayList(Value).init(allocator),
            .objects = null,
            .strings = Table.init(allocator),
        };
    }

    pub fn deinit(self: *VM) void {
        self.freeObjects();
        self.strings.deinit();
        self.stack.deinit();
    }

    pub fn freeObjects(self: *VM) void {
        var object = self.objects;
        while (object) |o| {
            const next = o.next;
            o.destroy(self);
            object = next;
        }
    }

    pub fn interpret(self: *VM, source: []const u8) !void {
        // Stack should be empty when we start and when we finish
        // running
        std.debug.assert(self.stack.items.len == 0);
        defer std.debug.assert(self.stack.items.len == 0);

        var chunk = Chunk.init(self.allocator);
        defer chunk.deinit();

        self.chunk = &chunk;
        self.ip = 0;

        try compile(self, source);
        try self.run();
    }

    fn run(self: *VM) !void {
        errdefer self.resetStack();

        while (true) {
            if (verbose) {
                // Print debugging information
                self.printStack();
                _ = self.chunk.disassembleInstruction(self.ip);
            }

            const instruction = self.readByte();
            const opCode = @intToEnum(OpCode, instruction);
            try self.runOp(opCode);
            if (opCode == .Return) break;
        }
    }

    fn runOp(self: *VM, opCode: OpCode) !void {
        switch (opCode) {
            .Return => {
                printValue(self.pop());
                std.debug.warn("\n", .{});
            },
            .Constant => {
                const constant = self.readByte();
                const value = self.chunk.constants.items[constant];
                try self.push(value);
            },
            .Nil => try self.push(Value.Nil),
            .True => try self.push(Value{ .Bool = true }),
            .False => try self.push(Value{ .Bool = false }),
            .Equal => {
                const b = self.pop();
                const a = self.pop();
                try self.push(Value{ .Bool = a.equals(b) });
            },
            .Greater => {
                const rhsBoxed = self.pop();
                const lhsBoxed = self.pop();
                switch (lhsBoxed) {
                    .Bool, .Nil, .Obj => return self.runtimeError("Operands must be numbers."),
                    .Number => |lhs| switch (rhsBoxed) {
                        .Bool, .Nil, .Obj => return self.runtimeError("Operands must be numbers."),
                        .Number => |rhs| try self.push(Value{ .Bool = lhs > rhs }),
                    },
                }
            },
            .Less => {
                const rhsBoxed = self.pop();
                const lhsBoxed = self.pop();
                switch (lhsBoxed) {
                    .Bool, .Nil, .Obj => return self.runtimeError("Operands must be numbers."),
                    .Number => |lhs| switch (rhsBoxed) {
                        .Bool, .Nil, .Obj => return self.runtimeError("Operands must be numbers."),
                        .Number => |rhs| try self.push(Value{ .Bool = lhs < rhs }),
                    },
                }
            },
            .Negate => {
                const boxed = self.pop();
                switch (boxed) {
                    .Bool, .Nil, .Obj => return self.runtimeError("Operand must be a number."),
                    .Number => |value| try self.push(Value{ .Number = -value }),
                }
            },
            .Add => {
                const rhsBoxed = self.pop();
                const lhsBoxed = self.pop();
                switch (lhsBoxed) {
                    .Bool, .Nil => return self.runtimeError("Operands must be numbers or strings."),
                    .Obj => |lhs| switch (rhsBoxed) {
                        .Bool, .Nil, .Number => return self.runtimeError("Operands must be numbers or strings."),
                        .Obj => |rhs| try self.concatenate(lhs, rhs),
                    },
                    .Number => |lhs| switch (rhsBoxed) {
                        .Bool, .Nil, .Obj => return self.runtimeError("Operands must be numbers or strings."),
                        .Number => |rhs| try self.push(Value{ .Number = lhs + rhs }),
                    },
                }
            },
            .Subtract => try self.binaryNumericOp(sub),
            .Multiply => try self.binaryNumericOp(mul),
            .Divide => try self.binaryNumericOp(div),
            .Not => try self.push(Value{ .Bool = self.pop().isFalsey() }),
        }
    }

    fn binaryNumericOp(self: *VM, comptime op: var) !void {
        const rhsBoxed = self.pop();
        const lhsBoxed = self.pop();
        switch (lhsBoxed) {
            .Bool, .Nil, .Obj => return self.runtimeError("Operands must be numbers."),
            .Number => |lhs| switch (rhsBoxed) {
                .Bool, .Nil, .Obj => return self.runtimeError("Operands must be numbers."),
                .Number => |rhs| try self.push(Value{ .Number = op(lhs, rhs) }),
            },
        }
    }

    fn concatenate(self: *VM, lhs: *Obj, rhs: *Obj) !void {
        switch (lhs.data) {
            .String => |lhsStr| switch (rhs.data) {
                .String => |rhsStr| {
                    const buffer = try self.allocator.alloc(u8, lhsStr.bytes.len + rhsStr.bytes.len);
                    std.mem.copy(u8, buffer[0..lhsStr.bytes.len], lhsStr.bytes);
                    std.mem.copy(u8, buffer[lhsStr.bytes.len..], rhsStr.bytes);
                    try self.push((try Obj.string(self, buffer)).value());
                },
            },
        }
    }

    fn readByte(self: *VM) u8 {
        const byte = self.chunk.code.items[self.ip];
        self.ip += 1;
        return byte;
    }

    fn push(self: *VM, value: Value) !void {
        try self.stack.append(value);
    }

    fn pop(self: *VM) Value {
        return self.stack.pop();
    }

    fn resetStack(self: *VM) void {
        self.stack.shrink(0);
    }

    fn printStack(self: *VM) void {
        std.debug.warn("          ", .{});
        for (self.stack.items) |value| {
            std.debug.warn("[ ", .{});
            printValue(value);
            std.debug.warn(" ]", .{});
        }
        std.debug.warn("\n", .{});
    }

    fn runtimeError(self: *VM, comptime message: []const u8) !void {
        // TODO, allow passing extra parameters here. Need varargs now,
        // but they're going away in zig 0.6.
        const line = self.chunk.code.items[self.ip];

        std.debug.warn(message, .{});
        std.debug.warn("\n[line {}] in script\n", .{line});

        return error.RuntimeError;
    }
};
