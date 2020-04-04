const std = @import("std");
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const VM = @import("./vm.zig").VM;

const allocator = std.debug.global_allocator;

pub fn main() !void {
    var vm = VM.init(allocator);
    var chunk = Chunk.init(allocator);
    const constant = try chunk.addConstant(1.2);
    try chunk.write(@enumToInt(OpCode.OP_CONSTANT), 123);
    try chunk.write(constant, 123);
    try chunk.write(@enumToInt(OpCode.OP_RETURN), 123);
    chunk.disassemble("test chunk");
    std.debug.warn("\n");
    const result = vm.interpret(&chunk);
    chunk.deinit();
}
