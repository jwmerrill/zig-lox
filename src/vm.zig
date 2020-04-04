const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const printValue = @import("./value.zig").printValue;

const verbose = true;

pub const InterpretResult = enum {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR,
};

pub const VM = struct {
    chunk: *Chunk,
    ip: usize, // NOTE, book uses a byte pointer for this
    stack: ArrayList(Value), // NOTE, book uses a fixed size stack

    pub fn init(allocator: *Allocator) VM {
        return VM{
            .chunk = undefined,
            .ip = undefined,
            .stack = std.ArrayList(Value).init(allocator),
        };
    }

    pub fn deinit(self: *VM) void {
        self.stack.deinit();
    }

    pub fn interpret(self: *VM, chunk: *Chunk) !InterpretResult {
        self.chunk = chunk;
        self.ip = 0;
        return self.run();
    }

    fn run(self: *VM) !InterpretResult {
        var out = InterpretResult.RUNTIME_ERROR;
        blk: while (true) {
            if (verbose) {
                // Print debugging information
                self.printStack();
                _ = self.chunk.disassembleInstruction(self.ip);
            }

            const instruction = self.readByte();

            switch (@intToEnum(OpCode, instruction)) {
                .OP_RETURN => {
                    printValue(self.pop());
                    std.debug.warn("\n");
                    out = InterpretResult.OK;
                    break :blk;
                },
                .OP_CONSTANT => {
                    // TODO, abstract out reading a byte
                    const constant = self.readByte();
                    const value = self.chunk.constants.at(constant);
                    try self.push(value);
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
