const std = @import("std");
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;

pub fn main() !void {
    var chunk = Chunk.init(std.debug.global_allocator);
    const constant = try chunk.addConstant(1.2);
    try chunk.write(@enumToInt(OpCode.OP_CONSTANT), 123);
    try chunk.write(constant, 123);
    try chunk.write(@enumToInt(OpCode.OP_RETURN), 123);
    chunk.disassemble("test chunk");
    chunk.deinit();
}
