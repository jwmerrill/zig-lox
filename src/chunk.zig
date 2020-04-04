const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const OpCode = enum {
    OP_RETURN,
    OP_WHATEVER,
};

pub const Chunk = struct {
    code: ArrayList(OpCode),

    pub fn init(allocator: *Allocator) Chunk {
        return Chunk{ .code = ArrayList(OpCode).init(allocator) };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
    }

    pub fn write(self: *Chunk, byte: OpCode) !void {
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

        switch (self.code.at(i)) {
            .OP_RETURN => {
                // TODO, make a simpleInstruction command
                std.debug.warn("OP_RETURN\n");
                return i + 1;
            },
            .OP_WHATEVER => {
                // TODO, make a simpleInstruction command
                std.debug.warn("OP_WHATEVER\n");
                return i + 1;
            }
        }
    }
};
