const std = @import("std");
const Chunk = @import("./chunk.zig").Chunk;
const VM = @import("./vm.zig").VM;

const allocator = std.debug.global_allocator;

pub fn main() !void {
    var vm = VM.init(allocator);
    var chunk = Chunk.init(allocator);

    try chunk.writeOp(.Constant, 123);
    try chunk.write(try chunk.addConstant(1.2), 123);

    try chunk.writeOp(.Constant, 123);
    try chunk.write(try chunk.addConstant(3.4), 123);

    try chunk.writeOp(.Add, 123);

    try chunk.writeOp(.Constant, 123);
    try chunk.write(try chunk.addConstant(5.6), 123);

    try chunk.writeOp(.Divide, 123);
    try chunk.writeOp(.Negate, 123);
    try chunk.writeOp(.Return, 123);
    chunk.disassemble("test chunk");
    std.debug.warn("\n");
    const result = vm.interpret(&chunk);
    chunk.deinit();
}
