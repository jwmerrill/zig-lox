const std = @import("std");
const io = std.io;
const process = std.process;
const Allocator = std.mem.Allocator;

const Chunk = @import("./chunk.zig").Chunk;
const VM = @import("./vm.zig").VM;
const ExternalWriter = @import("./writer.zig").ExternalWriter;

const allocator = std.heap.page_allocator;

// These functions are expected to be passed in as part of the WASM environment
extern fn writeOut(ptr: usize, len: usize) void;
extern fn writeErr(ptr: usize, len: usize) void;

fn writeOutSlice(bytes: []const u8) void {
    writeOut(@ptrToInt(bytes.ptr), bytes.len);
}

fn writeErrSlice(bytes: []const u8) void {
    writeErr(@ptrToInt(bytes.ptr), bytes.len);
}

export fn run(input_ptr: [*]const u8, input_len: usize) usize {
    const outStream = ExternalWriter.init(writeOutSlice).outStream();
    const errStream = ExternalWriter.init(writeErrSlice).outStream();

    var vm = VM.create();
    vm.init(allocator, outStream, errStream) catch |err| {
        return 71;
    };
    defer vm.deinit();

    const source = input_ptr[0..input_len];
    vm.interpret(source) catch |err| switch (err) {
        error.CompileError => return 65,
        error.RuntimeError => return 70,
        else => return 71,
    };

    return 0;
}

pub export fn alloc(len: usize) u32 {
    var buf = allocator.alloc(u8, len) catch |err| return 0;
    return @intCast(u32, @ptrToInt(buf.ptr));
}

pub export fn dealloc(ptr: [*]const u8, len: usize) void {
    allocator.free(ptr[0..len]);
}
