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

pub const CallFrame = struct {
    function: *Obj.Function,
    ip: usize,
    start: usize,
};

pub fn clockNative(args: []const Value) Value {
    return Value{ .Number = @intToFloat(f64, std.time.milliTimestamp()) / 1000 };
}

pub const VM = struct {
    allocator: *Allocator,
    frames: ArrayList(CallFrame), // NOTE, book uses a fixed size stack
    stack: ArrayList(Value), // NOTE, book uses a fixed size stack
    objects: ?*Obj,
    strings: Table,
    globals: Table,

    pub fn init(allocator: *Allocator) !VM {
        var vm = VM{
            .allocator = allocator,
            .frames = std.ArrayList(CallFrame).init(allocator),
            .stack = std.ArrayList(Value).init(allocator),
            .objects = null,
            .strings = Table.init(allocator),
            .globals = Table.init(allocator),
        };

        try vm.defineNative("clock", clockNative);

        return vm;
    }

    pub fn deinit(self: *VM) void {
        self.freeObjects();
        self.strings.deinit();
        self.globals.deinit();
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

        const function = try compile(self, source);
        try self.push(function.obj.value());
        try self.callValue(function.obj.value(), 0);
        try self.run();
        // Pop the call frame we put on the stack above
        _ = self.pop();
    }

    fn run(self: *VM) !void {
        errdefer self.resetStack();

        while (true) {
            if (verbose) {
                // Print debugging information
                try self.printStack();
                _ = self.currentChunk().disassembleInstruction(self.currentFrame().ip);
            }

            const instruction = self.readByte();
            const opCode = @intToEnum(OpCode, instruction);
            try self.runOp(opCode);
            if (opCode == .Return and self.frames.items.len == 0) break;
        }
    }

    fn readString(self: *VM) *Obj.String {
        const constant = self.readByte();
        const nameValue = self.currentChunk().constants.items[constant];
        return nameValue.Obj.asString();
    }

    fn runOp(self: *VM, opCode: OpCode) !void {
        switch (opCode) {
            .Return => {
                const result = self.pop();
                const frame = self.frames.pop();
                if (self.frames.items.len == 0) return;

                self.stack.shrink(frame.start);

                try self.push(result);
            },
            .Pop => {
                _ = self.pop();
            },
            .GetLocal => {
                const slot = self.readByte();
                try self.push(self.stack.items[self.currentFrame().start + slot]);
            },
            .SetLocal => {
                const slot = self.readByte();
                self.stack.items[self.currentFrame().start + slot] = self.peek(0);
            },
            .GetGlobal => {
                const name = self.readString();
                var value: Value = undefined;
                if (!self.globals.get(name, &value)) {
                    return self.runtimeError("Undefined variable '{}'.", .{name.bytes});
                }
                try self.push(value);
            },
            .DefineGlobal => {
                _ = try self.globals.set(self.readString(), self.peek(0));
                // NOTE don’t pop until value is in the hash table so
                // that we don't lose the value if the GC runs during
                // the set operation
                _ = self.pop();
            },
            .SetGlobal => {
                const name = self.readString();
                if (try self.globals.set(name, self.peek(0))) {
                    _ = self.globals.delete(name);
                    return self.runtimeError("Undefined variable '{}'.", .{name.bytes});
                }
            },
            .Print => {
                const stdout = std.io.getStdOut().outStream();
                try printValue(self.pop());
                try stdout.print("\n", .{});
            },
            .Jump => {
                const offset = self.readShort();
                self.currentFrame().ip += offset;
            },
            .JumpIfFalse => {
                const offset = self.readShort();
                if (self.peek(0).isFalsey()) self.currentFrame().ip += offset;
            },
            .Loop => {
                const offset = self.readShort();
                self.currentFrame().ip -= offset;
            },
            .Call => {
                const argCount = self.readByte();
                try self.callValue(self.peek(argCount), argCount);
            },
            .Constant => {
                const constant = self.readByte();
                const value = self.currentChunk().constants.items[constant];
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
                    .Bool, .Nil, .Obj => return self.runtimeError("Operands must be numbers.", .{}),
                    .Number => |lhs| switch (rhsBoxed) {
                        .Bool, .Nil, .Obj => return self.runtimeError("Operands must be numbers.", .{}),
                        .Number => |rhs| try self.push(Value{ .Bool = lhs > rhs }),
                    },
                }
            },
            .Less => {
                const rhsBoxed = self.pop();
                const lhsBoxed = self.pop();
                switch (lhsBoxed) {
                    .Bool, .Nil, .Obj => return self.runtimeError("Operands must be numbers.", .{}),
                    .Number => |lhs| switch (rhsBoxed) {
                        .Bool, .Nil, .Obj => return self.runtimeError("Operands must be numbers.", .{}),
                        .Number => |rhs| try self.push(Value{ .Bool = lhs < rhs }),
                    },
                }
            },
            .Negate => {
                const boxed = self.pop();
                switch (boxed) {
                    .Bool, .Nil, .Obj => return self.runtimeError("Operand must be a number.", .{}),
                    .Number => |value| try self.push(Value{ .Number = -value }),
                }
            },
            .Add => {
                const rhsBoxed = self.pop();
                const lhsBoxed = self.pop();
                switch (lhsBoxed) {
                    .Bool, .Nil => return self.runtimeError("Operands must be two numbers or two strings.", .{}),
                    .Obj => |lhs| switch (rhsBoxed) {
                        .Bool, .Nil, .Number => return self.runtimeError("Operands must be two numbers or two strings.", .{}),
                        .Obj => |rhs| try self.concatenate(lhs, rhs),
                    },
                    .Number => |lhs| switch (rhsBoxed) {
                        .Bool, .Nil, .Obj => return self.runtimeError("Operands must be two numbers or two strings.", .{}),
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
            .Bool, .Nil, .Obj => return self.runtimeError("Operands must be numbers.", .{}),
            .Number => |lhs| switch (rhsBoxed) {
                .Bool, .Nil, .Obj => return self.runtimeError("Operands must be numbers.", .{}),
                .Number => |rhs| try self.push(Value{ .Number = op(lhs, rhs) }),
            },
        }
    }

    fn concatenate(self: *VM, lhs: *Obj, rhs: *Obj) !void {
        switch (lhs.objType) {
            .Function, .NativeFunction => try self.runtimeError("Operands must be strings.", .{}),
            .String => switch (rhs.objType) {
                .Function, .NativeFunction => try self.runtimeError("Operands must be strings.", .{}),
                .String => {
                    const lhsStr = lhs.asString();
                    const rhsStr = rhs.asString();
                    const buffer = try self.allocator.alloc(u8, lhsStr.bytes.len + rhsStr.bytes.len);
                    std.mem.copy(u8, buffer[0..lhsStr.bytes.len], lhsStr.bytes);
                    std.mem.copy(u8, buffer[lhsStr.bytes.len..], rhsStr.bytes);
                    try self.push((try Obj.String.create(self, buffer)).obj.value());
                },
            },
        }
    }

    fn currentFrame(self: *VM) *CallFrame {
        return &self.frames.items[self.frames.items.len - 1];
    }

    fn currentChunk(self: *VM) *Chunk {
        return &self.currentFrame().function.chunk;
    }

    fn readByte(self: *VM) u8 {
        const frame = self.currentFrame();
        const byte = self.currentChunk().code.items[frame.ip];
        frame.ip += 1;
        return byte;
    }

    fn readShort(self: *VM) u16 {
        const frame = self.currentFrame();
        frame.ip += 2;
        const items = self.currentChunk().code.items;
        return (@intCast(u16, items[frame.ip - 2]) << 8) | items[frame.ip - 1];
    }

    fn push(self: *VM, value: Value) !void {
        try self.stack.append(value);
    }

    fn peek(self: *VM, back: usize) Value {
        return self.stack.items[self.stack.items.len - 1 - back];
    }

    fn pop(self: *VM) Value {
        return self.stack.pop();
    }

    fn call(self: *VM, function: *Obj.Function, argCount: usize) !void {
        if (argCount != function.arity) {
            return self.runtimeError("Expected {} arguments but got {}.", .{ function.arity, argCount });
        }

        // NOTE book checks stack length here and overflows if necessary

        try self.frames.append(CallFrame{
            .function = function,
            .ip = 0,
            // Stack position where this call frame begins
            //
            // Note, book uses a pointer into the stack called "slots",
            // but since our stack is resizable, use an index into the
            // stack instead.
            .start = self.stack.items.len - argCount - 1,
        });
    }

    fn callValue(self: *VM, callee: Value, argCount: usize) !void {
        switch (callee) {
            .Bool, .Nil, .Number => {
                return self.runtimeError("Can only call functions and classes.", .{});
            },
            .Obj => |obj| {
                switch (obj.objType) {
                    .String => {
                        return self.runtimeError("Can only call functions and classes.", .{});
                    },
                    .Function => try self.call(obj.asFunction(), argCount),
                    .NativeFunction => {
                        const args = self.stack.items[self.stack.items.len - 1 - argCount ..];
                        self.stack.shrink(self.stack.items.len - 1 - argCount);
                        const result = obj.asNativeFunction().function(args);
                        try self.push(result);
                    },
                }
            },
        }
    }

    fn resetStack(self: *VM) void {
        self.stack.shrink(0);
    }

    fn printStack(self: *VM) !void {
        std.debug.warn("          ", .{});
        for (self.stack.items) |value| {
            std.debug.warn("[ ", .{});
            try printValue(value);
            std.debug.warn(" ]", .{});
        }
        std.debug.warn("\n", .{});
    }

    fn runtimeError(self: *VM, comptime message: []const u8, args: var) !void {
        std.debug.warn(message, args);

        while (self.frames.items.len > 0) {
            const frame = self.frames.pop();
            const function = frame.function;
            const line = function.chunk.lines.items[frame.ip - 1];
            const name = if (function.name) |str| str.bytes else "<script>";
            std.debug.warn("\n[line {}] in {}", .{ line, name });
        }

        return error.RuntimeError;
    }

    fn defineNative(self: *VM, name: []const u8, function: Obj.NativeFunction.NativeFunctionType) !void {
        const str = try Obj.String.copy(self, name);
        // NOTE put str on the stack immediately to make sure it doesn't
        // get garbage collected when we allocate to create the native
        // function below.
        try self.push(str.obj.value());
        const functionValue = (try Obj.NativeFunction.create(self, function)).obj.value();
        try self.push(functionValue);
        _ = try self.globals.set(str, functionValue);
        _ = self.pop();
        _ = self.pop();
    }
};
