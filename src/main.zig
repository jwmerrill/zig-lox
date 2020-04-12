const std = @import("std");
const process = std.process;
const Allocator = std.mem.Allocator;

const Chunk = @import("./chunk.zig").Chunk;
const VM = @import("./vm.zig").VM;

pub fn main() !void {
    const allocator = std.debug.global_allocator;

    const args = try process.argsAlloc(allocator);
    defer process.argsFree(allocator, args);

    switch (args.len) {
        1 => try repl(allocator),
        2 => try runFile(allocator, args[1]),
        else => {
            std.debug.warn("Usage: clox [path]\n");
            process.exit(64);
        },
    }
}

fn repl(allocator: *Allocator) !void {
    var vm = VM.init(allocator);
    defer vm.deinit();

    var line = [_]u8{0} ** 256;
    while (true) {
        std.debug.warn("> ");
        const source = try std.io.readLineSlice(line[0..]);
        vm.interpret(source) catch |err| switch (err) {
            error.CompileError, error.RuntimeError => {},
            error.OutOfMemory => return err,
        };
    }
}

fn runFile(allocator: *Allocator, path: []const u8) !void {
    var vm = VM.init(allocator);
    defer vm.deinit();

    const source = try std.io.readFileAlloc(allocator, path);
    defer allocator.free(source);
    vm.interpret(source) catch |err| switch (err) {
        error.CompileError => process.exit(65),
        error.RuntimeError => process.exit(70),
        error.OutOfMemory => return err,
    };
}
