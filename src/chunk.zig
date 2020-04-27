const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;
const printValue = @import("./value.zig").printValue;

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
    Print,
    Jump,
    JumpIfFalse,
    Loop,
    Call,
    Closure,
    CloseUpvalue,
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

pub const Chunk = struct {
    code: ArrayList(u8),
    constants: ArrayList(Value),
    lines: ArrayList(usize),

    pub fn init(allocator: *Allocator) Chunk {
        return Chunk{
            .code = ArrayList(u8).init(allocator),
            .constants = ArrayList(Value).init(allocator),
            .lines = ArrayList(usize).init(allocator),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn write(self: *Chunk, byte: u8, line: usize) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    pub fn writeOp(self: *Chunk, op: OpCode, line: usize) !void {
        try self.write(@enumToInt(op), line);
    }

    pub fn disassemble(self: *Chunk, name: []const u8) void {
        std.debug.warn("== {} ==\n", .{name});

        var i: usize = 0;
        while (i < self.code.items.len) {
            i = self.disassembleInstruction(i);
        }
    }

    pub fn disassembleInstruction(self: *Chunk, offset: usize) usize {
        // Print offset
        std.debug.warn("{:0>4} ", .{offset});

        // Print line
        if (offset > 0 and self.lines.items[offset] == self.lines.items[offset - 1]) {
            std.debug.warn("   | ", .{});
        } else {
            std.debug.warn("{: >4} ", .{self.lines.items[offset]});
        }

        // Print instruction
        const instruction = @intToEnum(OpCode, self.code.items[offset]);
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
            .Print => self.simpleInstruction("OP_PRINT", offset),
            .Jump => self.jumpInstruction("OP_JUMP", 1, offset),
            .JumpIfFalse => self.jumpInstruction("OP_JUMP_IF_FALSE", 1, offset),
            .Loop => self.jumpInstruction("OP_LOOP", -1, offset),
            .Call => self.byteInstruction("OP_CALL", offset),
            .Closure => self.closureInstruction("OP_CLOSURE", offset),
            .CloseUpvalue => self.simpleInstruction("OP_CLOSE_UPVALUE", offset),
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
        std.debug.warn("{}\n", .{name});
        return offset + 1;
    }

    fn constantInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        const constant = self.code.items[offset + 1];
        std.debug.warn("{} {} ", .{ name, constant });
        // TODO mixes stdout and stderr in a funny way
        printValue(self.constants.items[constant]) catch unreachable;
        std.debug.warn("\n", .{});
        return offset + 2;
    }

    fn byteInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        const slot: u8 = self.code.items[offset + 1];
        // TODO, book makes more effort on formatting here, see Chap 22.4
        std.debug.warn("{} {}\n", .{ name, slot });
        return offset + 2;
    }

    fn jumpInstruction(self: *Chunk, name: []const u8, sign: isize, offset: usize) usize {
        var jump = @intCast(u16, self.code.items[offset + 1]) << 8;
        jump |= self.code.items[offset + 2];
        const target = @intCast(isize, offset) + 3 + sign * @intCast(isize, jump);
        std.debug.warn("{} {} -> {}\n", .{ name, offset, target });
        return offset + 3;
    }

    fn closureInstruction(self: *Chunk, name: []const u8, initialOffset: usize) usize {
        var offset = initialOffset + 1;
        const constant = self.code.items[offset];
        offset += 1;
        std.debug.warn("{} {} ", .{ name, constant });
        printValue(self.constants.items[constant]) catch unreachable;
        std.debug.warn("\n", .{});

        // Disassemble upvalues
        const function = self.constants.items[constant].Obj.asFunction();
        var i: usize = 0;
        while (i < function.upvalueCount) : (i += 1) {
            const isLocal = self.code.items[offset] != 1;
            const valueType = if (isLocal) "local" else "upvalue";
            offset += 1;
            const index = self.code.items[offset];
            offset += 1;
            std.debug.warn("{} | {} {}\n", .{ offset - 2, valueType, index });
        }

        return offset;
    }

    pub fn addConstant(self: *Chunk, value: Value) !u8 {
        const index = @intCast(u8, self.constants.items.len);
        try self.constants.append(value);
        return index;
    }
};
