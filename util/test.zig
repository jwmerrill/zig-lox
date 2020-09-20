const std = @import("std");
const process = std.process;

const allocator = std.heap.c_allocator;

pub fn main() !void {
    var npassed: usize = 0;
    var nfailed: usize = 0;

    {
        const args = try process.argsAlloc(allocator);
        defer process.argsFree(allocator, args);

        const lox_path = args[1];

        for (args[2..]) |test_path| {
            if (try run_test(lox_path, test_path)) {
                npassed += 1;
            } else {
                nfailed += 1;
            }
        }
    }

    std.debug.warn("\nPassed: {}, Failed: {}\n", .{ npassed, nfailed });
    process.exit(if (nfailed > 0) 1 else 0);
}

fn run_test(lox_path: []const u8, test_path: []const u8) !bool {
    std.debug.warn("{}\n", .{test_path});
    const argv = [_][]const u8{ lox_path, test_path };
    const result = try std.ChildProcess.exec(.{ .allocator = allocator, .argv = argv[0..] });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    const expected = try parse_test_file(test_path);
    defer allocator.free(expected.output);
    defer allocator.free(expected.runtime_error_message);
    defer allocator.free(expected.compile_error_message);

    return (
        validate_compile_error(result.stderr, expected.compile_error_message) and
        validate_runtime_error(result.stderr, expected.runtime_error_message) and
        validate_output(result.stdout, expected.output) and
        validate_exit_code(result.term.Exited, expected.exit_code));
}

fn validate_exit_code(actual: u32, expected: u32) bool {
    if (actual == expected) return true;
    std.debug.warn("Incorrect exit code\nActual: {}\nExpected: {}\n", .{ actual, expected });
    return false;
}

fn validate_output(actual: []const u8, expected: []const u8) bool {
    if (std.mem.eql(u8, actual, expected)) return true;
    std.debug.warn("Output differs:\nActual:\n{}\n\nExpected:\n{}\n\n", .{ actual, expected });
    return false;
}

fn validate_runtime_error(actual: []const u8, expected: []const u8) bool {
    if (expected.len == 0) return true;
    if (std.mem.indexOf(u8, actual, expected) != null) return true;
    std.debug.warn("Missing expected runtime error:\nActual:\n{}\n\nExpected:\n{} ...\n\n", .{ actual, expected });
    return false;
}

fn validate_compile_error(actual: []const u8, expected: []const u8) bool {
    if (expected.len == 0) return true;
    if (std.mem.eql(u8, actual, expected)) return true;
    std.debug.warn("Missing expected compile error:\nActual:\n{}\n\nExpected:\n{}\n\n", .{ actual, expected });
    return false;
}

const Expected = struct {
    output: []const u8,
    compile_error_message: []const u8,
    runtime_error_message: []const u8,
    exit_code: u32,
};

fn matches(source: []const u8, needle: []const u8) bool {
    return std.mem.eql(u8, source[0..std.math.min(needle.len, source.len)], needle);
}

fn parse_test_file(test_path: []const u8) !Expected {
    const source = try std.fs.cwd().readFileAlloc(allocator, test_path, 1_000_000);

    var output_buffer = std.ArrayList(u8).init(allocator);
    var compile_error_buffer = std.ArrayList(u8).init(allocator);
    var runtime_error_buffer = std.ArrayList(u8).init(allocator);

    const expect_prefix = "// expect: ";
    const error_prefix = "// Error";
    const line_error_prefix = "// [line ";
    const c_line_error_prefix = "// [c line ";
    const runtime_error_prefix = "// expect runtime error: ";

    var exit_code: u32 = 0;
    var line: usize = 1;
    var i: usize = 0;
    while (i < source.len) : (i += 1) {
        if (matches(source[i..], expect_prefix)) {
            i += expect_prefix.len;
            const j = std.mem.indexOfScalarPos(u8, source, i, '\n') orelse source.len;
            try output_buffer.appendSlice(source[i..j]);
            try output_buffer.append('\n');
            i = j;
        } else if (matches(source[i..], error_prefix)) {
            exit_code = 65;
            i += error_prefix.len;
            const j = std.mem.indexOfScalarPos(u8, source, i, '\n') orelse source.len;
            try compile_error_buffer.outStream().print("[line {}] Error", .{line});
            try compile_error_buffer.appendSlice(source[i..j]);
            try compile_error_buffer.append('\n');
            i = j;
        } else if (matches(source[i..], line_error_prefix)) {
            exit_code = 65;
            i += line_error_prefix.len;
            const j = std.mem.indexOfScalarPos(u8, source, i, '\n') orelse source.len;
            try compile_error_buffer.appendSlice("[line ");
            try compile_error_buffer.appendSlice(source[i..j]);
            try compile_error_buffer.append('\n');
            i = j;
        } else if (matches(source[i..], c_line_error_prefix)) {
            exit_code = 65;
            i += c_line_error_prefix.len;
            const j = std.mem.indexOfScalarPos(u8, source, i, '\n') orelse source.len;
            try compile_error_buffer.appendSlice("[line ");
            try compile_error_buffer.appendSlice(source[i..j]);
            try compile_error_buffer.append('\n');
            i = j;
        } else if (matches(source[i..], runtime_error_prefix)) {
            exit_code = 70;
            i += runtime_error_prefix.len;
            const j = std.mem.indexOfScalarPos(u8, source, i, '\n') orelse source.len;
            try runtime_error_buffer.appendSlice(source[i..j]);
            // Append start of stack trace to runtime error message
            try runtime_error_buffer.outStream().print("\n[line {}]", .{line});
            i = j;
        }

        if (i < source.len and source[i] == '\n') line += 1;
    }

    return Expected{
        .output = output_buffer.toOwnedSlice(),
        .runtime_error_message = runtime_error_buffer.toOwnedSlice(),
        .compile_error_message = compile_error_buffer.toOwnedSlice(),
        .exit_code = exit_code,
    };
}
