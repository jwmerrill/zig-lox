const std = @import("std");
const process = std.process;

const allocator = std.heap.c_allocator;

pub fn main() !void {
    var npassed: usize = 0;
    _ = npassed;
    var nfailed: usize = 0;

    {
        const args = try process.argsAlloc(allocator);
        defer process.argsFree(allocator, args);

        const lox_path = args[1];

        for (args[2..]) |test_path| {
            try run_benchmark(lox_path, test_path);
        }
    }

    process.exit(if (nfailed > 0) 1 else 0);
}

fn run_benchmark(lox_path: []const u8, test_path: []const u8) !void {
    std.debug.print("{s}\n", .{test_path});
    const argv = [_][]const u8{ lox_path, test_path };
    var min: i64 = std.math.maxInt(i64);
    var sum: i64 = 0;
    const nSamples = 5;
    var n: i32 = 0;
    while (n < nSamples) : (n += 1) {
        const start = std.time.milliTimestamp();
        const result = try std.ChildProcess.exec(.{ .allocator = allocator, .argv = argv[0..] });
        defer allocator.free(result.stdout);
        defer allocator.free(result.stderr);
        const end = std.time.milliTimestamp();
        const delta = end - start;
        sum += delta;
        min = @min(min, delta);
    }

    std.debug.print("  min: {} mean: {}\n", .{ min, @divTrunc(sum, nSamples) });
}
