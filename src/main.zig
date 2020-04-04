const std = @import("std");
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;

pub fn main() !void {
  var chunk = Chunk.init(std.debug.global_allocator);
  try chunk.write(@enumToInt(OpCode.OP_RETURN));
  const constant = try chunk.addConstant(1.2);
  try chunk.write(@enumToInt(OpCode.OP_CONSTANT));
  try chunk.write(constant);
  chunk.disassemble("test chunk");
  chunk.deinit();
}
