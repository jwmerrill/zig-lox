const std = @import("std");
const File = std.fs.File;
const Writer = std.Io.Writer;
const env = @import("./env.zig");

/// VMWriter is a pointer to a std.Io.Writer, the unified writer type in Zig 0.15.
/// For native builds, this points to the `interface` field of a File.Writer.
/// For WASM builds, this points to a WasmWriter's `interface` field.
pub const VMWriter = *Writer;

/// WasmWriter bridges the Zig 0.15 Writer interface to external WASM host functions.
/// It is unbuffered: all writes are forwarded immediately to the host.
pub const WasmWriter = struct {
    pub const WriteFnType = *const fn (bytes: []const u8) void;

    writeFn: WriteFnType,
    interface: Writer,

    const vtable: Writer.VTable = .{
        .drain = drain,
    };

    pub fn init(writeFn: WriteFnType) WasmWriter {
        return WasmWriter{
            .writeFn = writeFn,
            .interface = .{
                .vtable = &vtable,
                .buffer = &.{}, // unbuffered
            },
        };
    }

    pub fn writer(self: *WasmWriter) VMWriter {
        return &self.interface;
    }

    fn drain(w: *Writer, data: []const []const u8, splat: usize) Writer.Error!usize {
        const self: *WasmWriter = @fieldParentPtr("interface", w);
        var total: usize = 0;
        // Write buffered content first (buffer[0..end])
        if (w.end > 0) {
            self.writeFn(w.buffer[0..w.end]);
            w.end = 0;
        }
        // Write each data chunk
        for (data) |chunk| {
            if (chunk.len > 0) {
                self.writeFn(chunk);
                total += chunk.len;
            }
        }
        // Handle splat (repeat last element)
        if (splat > 0 and data.len > 0) {
            const pattern = data[data.len - 1];
            var remaining = splat;
            while (remaining > 0) {
                self.writeFn(pattern);
                remaining -= 1;
                total += pattern.len;
            }
        }
        return total;
    }
};
