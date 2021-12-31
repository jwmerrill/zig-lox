const std = @import("std");
const io = std.io;
const process = std.process;
const Allocator = std.mem.Allocator;

const Chunk = @import("./chunk.zig").Chunk;
const VM = @import("./vm.zig").VM;
const ExternalWriter = @import("./writer.zig").ExternalWriter;

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = general_purpose_allocator.allocator();

// These functions are expected to be passed in as part of the WASM environment
extern fn writeOut(ptr: usize, len: usize) void;
extern fn writeErr(ptr: usize, len: usize) void;

fn writeOutSlice(bytes: []const u8) void {
    writeOut(@ptrToInt(bytes.ptr), bytes.len);
}

fn writeErrSlice(bytes: []const u8) void {
    writeErr(@ptrToInt(bytes.ptr), bytes.len);
}

fn createVMPtr() !*VM {
    // Note, important that outWriter holds ExternalWriter instance by
    // value, and not by reference, since a reference to the external
    // writer would be invalidated when this function exits. That
    // mistake caught me out earlier.
    const outWriter = ExternalWriter.init(writeOutSlice).writer();
    const errWriter = ExternalWriter.init(writeErrSlice).writer();

    var vm = try allocator.create(VM);
    vm.* = VM.create();
    try vm.init(allocator, outWriter, errWriter);
    return vm;
}

export fn createVM() usize {
    var vm = createVMPtr() catch return 0;
    return @ptrToInt(vm);
}

export fn destroyVM(vm: *VM) void {
    vm.deinit();
    allocator.destroy(vm);
}

export fn interpret(vm: *VM, input_ptr: [*]const u8, input_len: usize) usize {
    const source = input_ptr[0..input_len];

    vm.interpret(source) catch |err| switch (err) {
        error.CompileError => return 65,
        error.RuntimeError => return 70,
        else => return 71,
    };

    return 0;
}

export fn run(input_ptr: [*]const u8, input_len: usize) usize {
    var vm = createVMPtr() catch return 71;
    defer destroyVM(vm);
    return interpret(vm, input_ptr, input_len);
}

pub export fn alloc(len: usize) usize {
    var buf = allocator.alloc(u8, len) catch return 0;
    return @ptrToInt(buf.ptr);
}

pub export fn dealloc(ptr: [*]const u8, len: usize) void {
    allocator.free(ptr[0..len]);
}
