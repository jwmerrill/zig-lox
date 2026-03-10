const std = @import("std");
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;

pub const OpCode = enum(u8) {
    Return,
    Pop,
    GetLocal,
    SetLocal,
    GetGlobal,
    DefineGlobal,
    SetGlobal,
    GetUpvalue,
    SetUpvalue,
    GetProperty,
    SetProperty,
    GetSuper,
    Print,
    Jump,
    JumpIfFalse,
    Loop,
    Call,
    Invoke,
    SuperInvoke,
    Closure,
    CloseUpvalue,
    Class,
    Inherit,
    Method,
    Constant,
    Nil,
    True,
    False,
    Equal,
    Greater,
    Less,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
};

/// Immutable bytecode container. Produced by ChunkBuilder.toChunk() after
/// compilation is complete. Holds owned slices that must be freed via deinit.
pub const Chunk = struct {
    code: []u8,
    constants: []Value,
    lines: []usize,

    pub const empty: Chunk = .{
        .code = &.{},
        .constants = &.{},
        .lines = &.{},
    };

    pub fn deinit(self: *Chunk, allocator: Allocator) void {
        if (self.code.len > 0) allocator.free(self.code);
        if (self.constants.len > 0) allocator.free(self.constants);
        if (self.lines.len > 0) allocator.free(self.lines);
        self.* = empty;
    }

    pub fn disassemble(self: *Chunk, name: []const u8) void {
        std.debug.print("== {s} ==\n", .{name});

        var i: usize = 0;
        while (i < self.code.len) {
            i = self.disassembleInstruction(i);
        }
    }

    pub fn disassembleInstruction(self: *Chunk, offset: usize) usize {
        // Print offset
        std.debug.print("{:0>4} ", .{offset});

        // Print line
        if (offset > 0 and self.lines[offset] == self.lines[offset - 1]) {
            std.debug.print("   | ", .{});
        } else {
            std.debug.print("{: >4} ", .{self.lines[offset]});
        }

        // Print instruction
        const instruction = @as(OpCode, @enumFromInt(self.code[offset]));
        return switch (instruction) {
            .Return => self.simpleInstruction("OP_RETURN", offset),
            .Pop => self.simpleInstruction("OP_POP", offset),
            .GetLocal => self.byteInstruction("OP_GET_LOCAl", offset),
            .SetLocal => self.byteInstruction("OP_SET_LOCAL", offset),
            .GetGlobal => self.constantInstruction("OP_GET_GLOBAL", offset),
            .DefineGlobal => self.constantInstruction("OP_DEFINE_GLOBAL", offset),
            .SetGlobal => self.constantInstruction("OP_SET_GLOBAL", offset),
            .GetUpvalue => self.byteInstruction("OP_GET_UPVALUE", offset),
            .SetUpvalue => self.byteInstruction("OP_SET_UPVALUE", offset),
            .GetProperty => self.constantInstruction("OP_GET_PROPERTY", offset),
            .SetProperty => self.constantInstruction("OP_SET_PROPERTY", offset),
            .GetSuper => self.constantInstruction("OP_GET_SUPER", offset),
            .Print => self.simpleInstruction("OP_PRINT", offset),
            .Jump => self.jumpInstruction("OP_JUMP", 1, offset),
            .JumpIfFalse => self.jumpInstruction("OP_JUMP_IF_FALSE", 1, offset),
            .Loop => self.jumpInstruction("OP_LOOP", -1, offset),
            .Call => self.byteInstruction("OP_CALL", offset),
            .Invoke => self.invokeInstruction("OP_INVOKE", offset),
            .SuperInvoke => self.invokeInstruction("OP_SUPER_INVOKE", offset),
            .Closure => self.closureInstruction("OP_CLOSURE", offset),
            .CloseUpvalue => self.simpleInstruction("OP_CLOSE_UPVALUE", offset),
            .Class => self.constantInstruction("OP_CLASS", offset),
            .Inherit => self.constantInstruction("OP_INHERIT", offset),
            .Method => self.constantInstruction("OP_METHOD", offset),
            .Constant => self.constantInstruction("OP_CONSTANT", offset),
            .Nil => self.simpleInstruction("OP_NIL", offset),
            .True => self.simpleInstruction("OP_TRUE", offset),
            .False => self.simpleInstruction("OP_FALSE", offset),
            .Equal => self.simpleInstruction("OP_EQUAL", offset),
            .Greater => self.simpleInstruction("OP_GREATER", offset),
            .Less => self.simpleInstruction("OP_LESS", offset),
            .Negate => self.simpleInstruction("OP_NEGATE", offset),
            .Add => self.simpleInstruction("OP_ADD", offset),
            .Subtract => self.simpleInstruction("OP_SUBTRACT", offset),
            .Multiply => self.simpleInstruction("OP_MULTIPLY", offset),
            .Divide => self.simpleInstruction("OP_DIVIDE", offset),
            .Not => self.simpleInstruction("OP_NOT", offset),
        };
    }

    fn simpleInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        _ = self;
        std.debug.print("{s}\n", .{name});
        return offset + 1;
    }

    fn constantInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        const constant = self.code[offset + 1];
        std.debug.print("{s} {} {f}\n", .{ name, constant, self.constants[constant] });
        std.debug.print("\n", .{});
        return offset + 2;
    }

    fn byteInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        const slot: u8 = self.code[offset + 1];
        // TODO, book makes more effort on formatting here, see Chap 22.4
        std.debug.print("{s} {}\n", .{ name, slot });
        return offset + 2;
    }

    fn jumpInstruction(self: *Chunk, name: []const u8, sign: isize, offset: usize) usize {
        var jump = @as(u16, @intCast(self.code[offset + 1])) << 8;
        jump |= self.code[offset + 2];
        const target = @as(isize, @intCast(offset)) + 3 + sign * @as(isize, @intCast(jump));
        std.debug.print("{s} {} -> {}\n", .{ name, offset, target });
        return offset + 3;
    }

    fn closureInstruction(self: *Chunk, name: []const u8, initialOffset: usize) usize {
        var offset = initialOffset + 1;
        const constant = self.code[offset];
        offset += 1;
        std.debug.print("{s} {} {f}\n", .{ name, constant, self.constants[constant] });

        // Disassemble upvalues
        const function = self.constants[constant].asObj().asFunction();
        var i: usize = 0;
        while (i < function.upvalueCount) : (i += 1) {
            const isLocal = self.code[offset] != 1;
            const valueType = if (isLocal) "local" else "upvalue";
            offset += 1;
            const index = self.code[offset];
            offset += 1;
            std.debug.print("{} | {s} {}\n", .{ offset - 2, valueType, index });
        }

        return offset;
    }

    fn invokeInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        const constant = self.code[offset + 1];
        const argCount = self.code[offset + 2];
        std.debug.print("{s} ({} args) {} '{f}'\n", .{ name, argCount, constant, self.constants[constant] });
        return offset + 3;
    }
};

/// Mutable bytecode builder used during compilation. Call toChunk() to
/// produce an immutable Chunk when compilation of a function is complete.
pub const ChunkBuilder = struct {
    allocator: Allocator,
    code: ArrayListUnmanaged(u8),
    constants: ArrayListUnmanaged(Value),
    lines: ArrayListUnmanaged(usize),

    pub fn init(allocator: Allocator) ChunkBuilder {
        return ChunkBuilder{
            .allocator = allocator,
            .code = .{},
            .constants = .{},
            .lines = .{},
        };
    }

    pub fn deinit(self: *ChunkBuilder) void {
        self.code.deinit(self.allocator);
        self.constants.deinit(self.allocator);
        self.lines.deinit(self.allocator);
    }

    /// Finalize the builder into an immutable Chunk, transferring ownership
    /// of the backing memory. The builder is reset to empty afterward.
    pub fn toChunk(self: *ChunkBuilder) !Chunk {
        const code = try self.code.toOwnedSlice(self.allocator);
        errdefer self.allocator.free(code);
        const constants = try self.constants.toOwnedSlice(self.allocator);
        errdefer self.allocator.free(constants);
        const lines = try self.lines.toOwnedSlice(self.allocator);
        return .{
            .code = code,
            .constants = constants,
            .lines = lines,
        };
    }

    pub fn write(self: *ChunkBuilder, byte: u8, line: usize) !void {
        try self.code.append(self.allocator, byte);
        try self.lines.append(self.allocator, line);
    }

    pub fn writeOp(self: *ChunkBuilder, op: OpCode, line: usize) !void {
        try self.write(@intFromEnum(op), line);
    }

    pub fn addConstant(self: *ChunkBuilder, value: Value) !u9 {
        const index = @as(u9, @intCast(self.constants.items.len));
        try self.constants.append(self.allocator, value);
        return index;
    }
};
