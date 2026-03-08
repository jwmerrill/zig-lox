const std = @import("std");
const builtin = @import("builtin");
const process = std.process;
const Allocator = std.mem.Allocator;
const File = std.fs.File;

const VM = @import("./vm.zig").VM;
const debug = @import("./debug.zig");

pub fn main() !void {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

    const allocator = init: {
        if (comptime debug.TESTING_ALLOCATOR) {
            break :init std.testing.allocator;
        } else if (comptime std.Target.Cpu.Arch.isWasm(builtin.target.cpu.arch)) {
            break :init debug_allocator.allocator();
        } else {
            break :init switch (builtin.mode) {
                .Debug, .ReleaseSafe => debug_allocator.allocator(),
                .ReleaseFast, .ReleaseSmall => std.heap.smp_allocator,
            };
        }
    };

    if (comptime std.Target.Cpu.Arch.isWasm(builtin.target.cpu.arch)) {
        try repl(allocator);
    } else {
        const args = try process.argsAlloc(allocator);
        defer process.argsFree(allocator, args);

        switch (args.len) {
            1 => try repl(allocator),
            2 => try runFile(allocator, args[1]),
            else => {
                var buf: [4096]u8 = undefined;
                var stderr_w = File.stderr().writerStreaming(&buf);
                try stderr_w.interface.print("Usage: lox [path]\n", .{});
                try stderr_w.interface.flush();
                process.exit(64);
            },
        }
    }
}

fn repl(allocator: Allocator) !void {
    var stderr_buf: [4096]u8 = undefined;
    var stderr_w = File.stderr().writerStreaming(&stderr_buf);
    const stdin = File.stdin();

    var out_buf: [4096]u8 = undefined;
    var out_writer = File.stdout().writerStreaming(&out_buf);
    var err_buf: [4096]u8 = undefined;
    var err_writer = File.stderr().writerStreaming(&err_buf);

    var vm = VM.create();
    try vm.init(allocator, &out_writer.interface, &err_writer.interface);
    defer vm.deinit();

    var sourceBuf: [256]u8 = undefined;
    while (true) {
        try stderr_w.interface.print("> ", .{});
        try stderr_w.interface.flush();
        const amt = try stdin.read(&sourceBuf);
        if (amt == sourceBuf.len) {
            try stderr_w.interface.print("Input too long.\n", .{});
            try stderr_w.interface.flush();
            continue;
        }
        const source = sourceBuf[0..amt];
        vm.interpret(source) catch |err| switch (err) {
            error.CompileError, error.RuntimeError => {},
            else => return err,
        };
    }
}

fn runFile(allocator: Allocator, path: []const u8) !void {
    var out_buf: [4096]u8 = undefined;
    var out_writer = File.stdout().writerStreaming(&out_buf);
    var err_buf: [4096]u8 = undefined;
    var err_writer = File.stderr().writerStreaming(&err_buf);

    var vm = VM.create();
    try vm.init(allocator, &out_writer.interface, &err_writer.interface);
    defer vm.deinit();

    const source = try std.fs.cwd().readFileAlloc(allocator, path, 1_000_000);
    defer allocator.free(source);
    vm.interpret(source) catch |err| switch (err) {
        error.CompileError => process.exit(65),
        error.RuntimeError => process.exit(70),
        else => return err,
    };
}
