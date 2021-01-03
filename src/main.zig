const std = @import("std");
const io = std.io;
const process = std.process;
const Allocator = std.mem.Allocator;

const VM = @import("./vm.zig").VM;
const debug = @import("./debug.zig");

pub fn main() !void {
    const allocator = init: {
        if (comptime debug.TESTING_ALLOCATOR) {
            break :init std.testing.allocator;
        } else if (comptime std.Target.current.isWasm()) {
            // TODO, this allocator is very inefficient
            break :init std.heap.page_allocator;
        } else {
            break :init std.heap.c_allocator;
        }
    };

    if (comptime std.Target.current.isWasm()) {
        try repl(allocator);
    } else {
        const args = try process.argsAlloc(allocator);
        defer process.argsFree(allocator, args);

        switch (args.len) {
            1 => try repl(allocator),
            2 => try runFile(allocator, args[1]),
            else => {
                std.debug.warn("Usage: clox [path]\n", .{});
                process.exit(64);
            },
        }
    }
}

fn repl(allocator: *Allocator) !void {
    const stderr = io.getStdErr().outStream();
    const stdin = io.getStdIn();

    var vm = VM.create();
    try vm.init(allocator, std.io.getStdOut().outStream(), std.io.getStdErr().outStream());
    defer vm.deinit();

    var sourceBuf: [256]u8 = undefined;
    while (true) {
        try stderr.print("> ", .{});
        const amt = try stdin.read(&sourceBuf);
        if (amt == sourceBuf.len) {
            try stderr.print("Input too long.\n", .{});
            continue;
        }
        const source = sourceBuf[0..amt];
        vm.interpret(source) catch |err| switch (err) {
            error.CompileError, error.RuntimeError => {},
            else => return err,
        };
    }
}

fn runFile(allocator: *Allocator, path: []const u8) !void {
    var vm = VM.create();
    try vm.init(allocator, std.io.getStdOut().outStream(), std.io.getStdErr().outStream());
    defer vm.deinit();

    const source = try std.fs.cwd().readFileAlloc(allocator, path, 1_000_000);
    defer allocator.free(source);
    vm.interpret(source) catch |err| switch (err) {
        error.CompileError => process.exit(65),
        error.RuntimeError => process.exit(70),
        else => return err,
    };
}
