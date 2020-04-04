const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;
const printValue = @import("./value.zig").printValue;

pub const OpCode = enum(u8) {
    Return,
    Constant,
};

pub const Chunk = struct {
    code: ArrayList(u8),
    constants: ArrayList(Value),
    lines: ArrayList(usize),

    pub fn init(allocator: *Allocator) Chunk {
        return Chunk{
            .code = ArrayList(u8).init(allocator),
            .constants = ArrayList(Value).init(allocator),
            .lines = ArrayList(usize).init(allocator),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn write(self: *Chunk, byte: u8, line: usize) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    pub fn disassemble(self: *Chunk, name: []const u8) void {
        std.debug.warn("== {} ==\n", name);

        var i: usize = 0;
        while (i < self.code.len) {
            i = self.disassembleInstruction(i);
        }
    }

    pub fn disassembleInstruction(self: *Chunk, offset: usize) usize {
        // Print offset
        std.debug.warn("{:0>4} ", offset);

        // Print line
        if (offset > 0 and self.lines.at(offset) == self.lines.at(offset - 1)) {
            std.debug.warn("   | ");
        } else {
            std.debug.warn("{: >4} ", self.lines.at(offset));
        }

        // Print instruction
        const instruction = @intToEnum(OpCode, self.code.at(offset));
        switch (instruction) {
            .Return => return self.simpleInstruction("OP_RETURN", offset),
            .Constant => return self.constantInstruction("OP_CONSTANT", offset),
            else => {
                std.debug.warn("Unknown opcode: {}\n", instruction);
                return offset + 1;
            },
        }
    }

    fn simpleInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        std.debug.warn("{}\n", name);
        return offset + 1;
    }

    fn constantInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        // TODO, make a constantInstruction command
        const constant = self.code.at(offset + 1);
        std.debug.warn("{} {} ", name, constant);
        printValue(self.constants.at(constant));
        std.debug.warn("\n");
        return offset + 2;
    }

    pub fn addConstant(self: *Chunk, value: Value) !u8 {
        // Note, this will have to change to self.constants.items.len in
        // zig 0.6
        const index = @intCast(u8, self.constants.len);
        try self.constants.append(value);
        return index;
    }
};
