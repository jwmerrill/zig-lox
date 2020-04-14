const std = @import("std");
const io = std.io;
const process = std.process;
const Allocator = std.mem.Allocator;

const Chunk = @import("./chunk.zig").Chunk;
const VM = @import("./vm.zig").VM;

pub fn main() !void {
    const allocator = std.testing.allocator;

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

fn repl(allocator: *Allocator) !void {
    const stderr = io.getStdErr().outStream();
    const stdin = io.getStdIn();

    var vm = VM.init(allocator);
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
            error.OutOfMemory => return err,
        };
    }
}

fn runFile(allocator: *Allocator, path: []const u8) !void {
    var vm = VM.init(allocator);
    defer vm.deinit();

    const source = try std.fs.cwd().readFileAlloc(allocator, path, 1_000_000);
    defer allocator.free(source);
    vm.interpret(source) catch |err| switch (err) {
        error.CompileError => process.exit(65),
        error.RuntimeError => process.exit(70),
        error.OutOfMemory => return err,
    };
}
