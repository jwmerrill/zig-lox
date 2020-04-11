const std = @import("std");
const warn = std.debug.warn;
const Allocator = std.mem.Allocator;
const Scanner = @import("./scanner.zig").Scanner;
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;
const VM = @import("./vm.zig").VM;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const Obj = @import("./object.zig").Obj;
const ObjString = @import("./object.zig").ObjString;
const verbose = @import("./debug.zig").verbose;

// Note, the compiler allocates objects as part of parsing that must be
// freed by the caller.
pub fn compile(vm: *VM, source: []const u8) !bool {
    var parser = Parser.init(vm, source);
    parser.advance();
    try parser.expression();
    parser.consume(.Eof, "Expect end of expression.");
    try parser.end();

    return !parser.hadError;
}

const Precedence = enum(u8) {
    None,
    Assignment, // =
    Or, // or
    And, // and
    Equality, // == !=
    Comparison, // < > <= >=
    Term, // + -
    Factor, // * /
    Unary, // ! -
    Call, // . ()
    Primary,

    pub fn next(self: Precedence) Precedence {
        return @intToEnum(Precedence, @enumToInt(self) + 1);
    }
};

// Note, have to spell these out explicitly right now because zig has
// trouble inferring error sets for recursive functions.
//
// See https://github.com/ziglang/zig/issues/2971
const CompilerErrors = error{
// Can happen when we try to emit bytecode or constants
    OutOfMemory,
    // Can happen when we try to parse floats
    InvalidCharacter,
};

fn getPrecedence(tokenType: TokenType) Precedence {
    return switch (tokenType) {
        // Single-character tokens.
        .LeftParen => .Call,
        .RightParen, .LeftBrace, .RightBrace, .Comma => .None,
        .Dot => .Call,
        .Minus, .Plus => .Term,
        .Semicolon => .None,
        .Slash, .Star => .Factor,

        // One or two character tokens.
        .BangEqual, .EqualEqual => .Equality,
        .Greater, .GreaterEqual, .Less, .LessEqual => .Comparison,
        .Bang, .Equal => .None,

        // Literals.
        .Identifier, .String, .Number => .None,

        // Keywords.
        .And, .Class, .Else, .False, .For, .Fun, .If, .Nil, .Or => .None,
        .Print, .Return, .Super, .This, .True, .Var, .While, .Error => .None,
        .Eof => .None,
    };
}

const Parser = struct {
    vm: *VM,
    scanner: Scanner,
    current: Token,
    previous: Token,
    hadError: bool,
    panicMode: bool,

    pub fn init(vm: *VM, source: []const u8) Parser {
        return Parser{
            .vm = vm,
            .scanner = Scanner.init(source),
            .current = undefined,
            .previous = undefined,
            .hadError = false,
            .panicMode = false,
        };
    }

    pub fn currentChunk(self: *Parser) *Chunk {
        return self.vm.chunk;
    }

    pub fn advance(self: *Parser) void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();
            if (self.current.tokenType != .Error) break;
            self.errorAtCurrent(self.current.lexeme);
        }
    }

    pub fn consume(self: *Parser, tokenType: TokenType, message: []const u8) void {
        if (self.current.tokenType == tokenType) {
            self.advance();
        } else {
            self.errorAtCurrent(message);
        }
    }

    pub fn errorAtCurrent(self: *Parser, message: []const u8) void {
        self.errorAt(&self.current, message);
    }

    pub fn err(self: *Parser, message: []const u8) void {
        self.errorAt(&self.previous, message);
    }

    pub fn prefixError(self: *Parser) void {
        self.err("Expect expression.");
    }

    pub fn infixError(self: *Parser) void {
        self.err("Expect expression.");
    }

    pub fn errorAt(self: *Parser, token: *Token, message: []const u8) void {
        if (self.panicMode) return;
        self.panicMode = true;

        warn("[line {}] Error", token.line);

        switch (token.tokenType) {
            .Eof => {
                warn(" at end");
            },
            .Error => {},
            else => {
                warn(" at '{}'", token.lexeme);
            },
        }

        warn(": {}\n", message);

        self.hadError = true;
    }

    pub fn emitByte(self: *Parser, byte: u8) !void {
        try self.currentChunk().write(byte, self.previous.line);
    }

    pub fn emitOp(self: *Parser, op: OpCode) !void {
        try self.currentChunk().writeOp(op, self.previous.line);
    }

    pub fn emitUnaryOp(self: *Parser, op: OpCode, byte: u8) !void {
        try self.emitOp(op);
        try self.emitByte(byte);
    }

    pub fn emitConstant(self: *Parser, value: Value) !void {
        try self.emitUnaryOp(.Constant, try self.makeConstant(value));
    }

    pub fn emitReturn(self: *Parser) !void {
        try self.emitOp(.Return);
    }

    pub fn end(self: *Parser) !void {
        try self.emitReturn();

        if (verbose) {
            if (!self.hadError) {
                self.currentChunk().disassemble("code");
            }
        }
    }

    pub fn makeConstant(self: *Parser, value: Value) !u8 {
        const constant = try self.currentChunk().addConstant(value);
        if (constant > std.math.maxInt(u8)) {
            self.err("Too many constants in one chunk.");
            return 0;
        }

        return @intCast(u8, constant);
    }

    pub fn expression(self: *Parser) CompilerErrors!void {
        try self.parsePrecedence(.Assignment);
    }

    pub fn parsePrecedence(self: *Parser, precedence: Precedence) CompilerErrors!void {
        self.advance();
        try self.prefix(self.previous.tokenType);

        while (@enumToInt(precedence) <= @enumToInt(getPrecedence(self.current.tokenType))) {
            self.advance();
            try self.infix(self.previous.tokenType);
        }
    }

    pub fn prefix(self: *Parser, tokenType: TokenType) !void {
        switch (tokenType) {
            // Single-character tokens.
            .LeftParen => return self.grouping(),
            .RightParen, .LeftBrace, .RightBrace, .Comma, .Dot => {},
            .Minus => return self.unary(),
            .Plus, .Semicolon, .Slash, .Star => {},

            // One or two character tokens.
            .Bang => return self.unary(),
            .BangEqual,
            .EqualEqual,
            .Greater,
            .GreaterEqual,
            .Less,
            => {},
            .LessEqual, .Equal => {},

            // Literals.
            .Identifier => {},
            .String => return self.string(),
            .Number => return self.number(),

            // Keywords.
            .Nil, .True, .False => return self.literal(),
            .And, .Class, .Else, .For, .Fun, .If, .Or => {},
            .Print, .Return, .Super, .This, .Var, .While, .Error => {},
            .Eof => {},
        }

        self.prefixError();
    }

    pub fn infix(self: *Parser, tokenType: TokenType) !void {
        switch (tokenType) {
            // Single-character tokens.
            .LeftParen, .RightParen, .LeftBrace, .RightBrace, .Comma => {},
            .Dot => {},
            .Minus, .Plus => return self.binary(),
            .Semicolon => {},
            .Slash, .Star => return self.binary(),

            // One or two character tokens.
            .Bang => {},
            .BangEqual, .EqualEqual, .Greater => return self.binary(),
            .GreaterEqual, .Less, .LessEqual => return self.binary(),
            .Equal => {},

            // Literals.
            .Identifier, .String, .Number => {},

            // Keywords.
            .And, .Class, .Else, .False, .For, .Fun, .If, .Nil, .Or => {},
            .Print, .Return, .Super, .This, .True, .Var, .While, .Error => {},
            .Eof => {},
        }

        self.infixError();
    }

    pub fn number(self: *Parser) !void {
        const value = try std.fmt.parseFloat(f64, self.previous.lexeme);
        try self.emitConstant(Value{ .Number = value });
    }

    pub fn literal(self: *Parser) !void {
        switch (self.previous.tokenType) {
            .Nil => try self.emitOp(.Nil),
            .True => try self.emitOp(.True),
            .False => try self.emitOp(.False),
            else => self.err("Unexpected literal"), // unreachable
        }
    }

    pub fn string(self: *Parser) !void {
        const source = self.previous.lexeme[1..self.previous.lexeme.len-1];
        const buffer = try self.vm.allocator.alloc(u8, source.len);
        std.mem.copy(u8, buffer, source);
        try self.emitConstant((try Obj.string(self.vm, buffer)).value());
    }

    pub fn grouping(self: *Parser) !void {
        try self.expression();
        self.consume(.RightParen, "Expect ')' after expression.");
    }

    pub fn unary(self: *Parser) !void {
        const operatorType = self.previous.tokenType;

        // Compile the operand
        try self.parsePrecedence(.Unary);

        // Emit the operator instruction
        switch (operatorType) {
            .Bang => try self.emitOp(.Not),
            .Minus => try self.emitOp(.Negate),
            else => self.err("Unexpected unary operator"), // unreachable
        }
    }

    pub fn binary(self: *Parser) !void {
        const operatorType = self.previous.tokenType;

        try self.parsePrecedence(getPrecedence(operatorType).next());

        switch (operatorType) {
            .BangEqual => {
                try self.emitOp(.Equal);
                try self.emitOp(.Not);
            },
            .EqualEqual => try self.emitOp(.Equal),
            .Greater => try self.emitOp(.Greater),
            .GreaterEqual => {
                // Note, incorrect IEEE semantics for NaN, same as book
                try self.emitOp(.Less);
                try self.emitOp(.Not);
            },
            .Less => try self.emitOp(.Less),
            .LessEqual => {
                // Note, incorrect IEEE semantics for NaN, same as book
                try self.emitOp(.Greater);
                try self.emitOp(.Not);
            },
            .Plus => try self.emitOp(.Add),
            .Minus => try self.emitOp(.Subtract),
            .Star => try self.emitOp(.Multiply),
            .Slash => try self.emitOp(.Divide),
            else => self.err("Unexpected binary operator"), // unreachable
        }
    }
};
