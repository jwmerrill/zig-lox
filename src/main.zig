const std = @import("std");
const chunk = @import("./chunk.zig");

pub fn main() !void {
  var c = chunk.Chunk.init(std.debug.global_allocator);
  try c.write(chunk.OpCode.OP_RETURN);
  c.disassemble("test chunk");
  c.deinit();
}
