const std = @import("std");
const File = std.fs.File;
const env = @import("./env.zig");

pub const ExternalWriter = struct {
    writeFn: fn (bytes: []const u8) void,

    pub fn init(writeFn: fn (bytes: []const u8) void) ExternalWriter {
        return ExternalWriter{ .writeFn = writeFn };
    }

    pub const WriteError = error{};

    pub fn write(self: ExternalWriter, bytes: []const u8) WriteError!usize {
        self.writeFn(bytes);
        return bytes.len;
    }

    pub const OutStream = std.io.OutStream(ExternalWriter, WriteError, write);
    pub fn outStream(self: ExternalWriter) OutStream {
        return .{ .context = self };
    }
};

pub const VMOutStream = comptime if (env.IS_WASM_FREESTANDING) ExternalWriter.OutStream else File.OutStream;
