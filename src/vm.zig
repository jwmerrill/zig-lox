const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const printValue = @import("./value.zig").printValue;
const compile = @import("./compiler.zig").compile;
const verbose = @import("./debug.zig").verbose;

pub const InterpretResult = enum {
    Ok,
    CompileError,
    RuntimeError,
};

pub const VM = struct {
    allocator: *Allocator,
    chunk: *Chunk,
    ip: usize, // NOTE, book uses a byte pointer for this
    stack: ArrayList(Value), // NOTE, book uses a fixed size stack

    pub fn init(allocator: *Allocator) VM {
        return VM{
            .allocator = allocator,
            .chunk = undefined,
            .ip = undefined,
            .stack = std.ArrayList(Value).init(allocator),
        };
    }

    pub fn deinit(self: *VM) void {
        self.stack.deinit();
    }

    pub fn interpret(self: *VM, source: []const u8) !InterpretResult {
        var chunk = Chunk.init(self.allocator);
        defer chunk.deinit();

        const success = try compile(source, &chunk);
        if (!success) return .CompileError;

        self.chunk = &chunk;
        self.ip = 0;

        return try self.run();
    }

    fn run(self: *VM) !InterpretResult {
        var out = InterpretResult.RuntimeError;
        while (true) {
            if (verbose) {
                // Print debugging information
                self.printStack();
                _ = self.chunk.disassembleInstruction(self.ip);
            }

            const instruction = self.readByte();

            switch (@intToEnum(OpCode, instruction)) {
                .Return => {
                    printValue(self.pop());
                    std.debug.warn("\n");
                    out = InterpretResult.Ok;
                    break;
                },
                .Constant => {
                    const constant = self.readByte();
                    const value = self.chunk.constants.at(constant);
                    try self.push(value);
                },
                .Negate => {
                    try self.push(-self.pop());
                },
                .Add => {
                    const rhs = self.pop();
                    const lhs = self.pop();
                    try self.push(lhs + rhs);
                },
                .Subtract => {
                    const rhs = self.pop();
                    const lhs = self.pop();
                    try self.push(lhs - rhs);
                },
                .Multiply => {
                    const rhs = self.pop();
                    const lhs = self.pop();
                    try self.push(lhs * rhs);
                },
                .Divide => {
                    const rhs = self.pop();
                    const lhs = self.pop();
                    try self.push(lhs / rhs);
                },
            }
        }

        // Stack should be empty when we finish running
        std.debug.assert(self.stack.len == 0);

        return out;
    }

    fn readByte(self: *VM) u8 {
        const byte = self.chunk.code.at(self.ip);
        self.ip += 1;
        return byte;
    }

    fn push(self: *VM, value: Value) !void {
        try self.stack.append(value);
    }

    fn pop(self: *VM) Value {
        return self.stack.pop();
    }

    fn printStack(self: *VM) void {
        std.debug.warn("          ");
        for (self.stack.toSlice()) |value| {
            std.debug.warn("[ ");
            printValue(value);
            std.debug.warn(" ]");
        }
        std.debug.warn("\n");
    }
};
