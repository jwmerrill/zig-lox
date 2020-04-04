const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;
const printValue = @import("./value.zig").printValue;

pub const OpCode = enum(u8) {
    OP_RETURN,
    OP_CONSTANT,
};

pub const Chunk = struct {
    code: ArrayList(u8),
    constants: ArrayList(Value),


    pub fn init(allocator: *Allocator) Chunk {
        return Chunk{
            .code = ArrayList(u8).init(allocator),
            .constants = ArrayList(Value).init(allocator)
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
    }

    pub fn write(self: *Chunk, byte: u8) !void {
        try self.code.append(byte);
    }

    pub fn disassemble(self: *Chunk, name: []const u8) void {
        std.debug.warn("== {} ==\n", name);

        var i: usize = 0;
        while (i < self.code.len) {
            i = self.disassembleInstruction(i);
        }
    }

    fn disassembleInstruction(self: *Chunk, i: usize) usize {
        std.debug.warn("{:0>4} ", i);

        const instruction = @intToEnum(OpCode, self.code.at(i));
        switch (instruction) {
            .OP_RETURN => {
                // TODO, make a simpleInstruction command
                std.debug.warn("OP_RETURN\n");
                return i + 1;
            },
            .OP_CONSTANT => {
                // TODO, make a constantInstruction command
                const constant = self.code.at(i+1);
                std.debug.warn("OP_CONSTANT {} ", constant);
                printValue(self.constants.at(constant));
                std.debug.warn("\n");
                return i + 2;
            },
            else => {
                std.debug.warn("Unknown opcode: {}\n", instruction);
                return i + 1;
            }
        }
    }

    pub fn addConstant(self: *Chunk, value: Value) !u8 {
        // Note, this will have to change to self.constants.items.len in
        // zig 0.6
        const index = @intCast(u8, self.constants.len);
        try self.constants.append(value);
        return index;
    }
};
