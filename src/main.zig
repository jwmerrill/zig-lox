const std = @import("std");
const chunk = @import("./chunk.zig");

pub fn main() !void {
  var c = chunk.Chunk.init(std.debug.global_allocator);
  try c.write(@enumToInt(chunk.OpCode.OP_RETURN));
  const constant = try c.addConstant(1.2);
  try c.write(@enumToInt(chunk.OpCode.OP_CONSTANT));
  try c.write(constant);
  c.disassemble("test chunk");
  c.deinit();
}
