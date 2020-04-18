const std = @import("std");
const warn = std.debug.warn;
const maxInt = std.math.maxInt;
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
pub fn compile(vm: *VM, source: []const u8) !void {
    var parser = Parser.init(vm, source);
    defer parser.deinit();
    parser.advance();
    while (!parser.match(.Eof)) {
        try parser.declaration();
    }
    parser.consume(.Eof, "Expect end of expression.");
    try parser.end();

    if (parser.hadError) return error.CompileError;
}

pub const Compiler = struct {
    locals: std.ArrayList(Local),
    scopeDepth: usize,

    pub fn init(allocator: *Allocator) Compiler {
        return Compiler{
            .locals = std.ArrayList(Local).init(allocator),
            .scopeDepth = 0,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.locals.deinit();
    }
};

pub const Local = struct {
    name: []const u8,
    depth: isize,
};

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
OutOfMemory};

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
        .And => .And,
        .Or => .Or,
        .Class, .Else, .False, .For, .Fun, .If, .Nil => .None,
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
    compiler: Compiler,

    pub fn init(vm: *VM, source: []const u8) Parser {
        return Parser{
            .vm = vm,
            .scanner = Scanner.init(source),
            .current = undefined,
            .previous = undefined,
            .hadError = false,
            .panicMode = false,
            .compiler = Compiler.init(vm.allocator),
        };
    }

    pub fn deinit(self: *Parser) void {
        self.compiler.deinit();
    }

    pub fn currentChunk(self: *Parser) *Chunk {
        return self.vm.chunk;
    }

    pub fn advance(self: *Parser) void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();
            if (!self.check(.Error)) break;
            self.errorAtCurrent(self.current.lexeme);
        }
    }

    pub fn check(self: *Parser, tokenType: TokenType) bool {
        return self.current.tokenType == tokenType;
    }

    pub fn consume(self: *Parser, tokenType: TokenType, message: []const u8) void {
        if (self.check(tokenType)) {
            self.advance();
        } else {
            self.errorAtCurrent(message);
        }
    }

    pub fn match(self: *Parser, tokenType: TokenType) bool {
        if (!self.check(tokenType)) return false;
        self.advance();
        return true;
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

        warn("[line {}] Error", .{token.line});

        switch (token.tokenType) {
            .Eof => {
                warn(" at end", .{});
            },
            .Error => {},
            else => {
                warn(" at '{}'", .{token.lexeme});
            },
        }

        warn(": {}\n", .{message});

        self.hadError = true;
    }

    pub fn emitJump(self: *Parser, op: OpCode) !usize {
        try self.emitOp(op);
        // Dummy operands that will be patched later
        try self.emitByte(0xff);
        try self.emitByte(0xff);
        return self.currentChunk().code.items.len - 2;
    }

    pub fn patchJump(self: *Parser, offset: usize) void {
        const jump = self.currentChunk().code.items.len - offset - 2;

        if (jump > maxInt(u16)) {
            self.err("Too much code to jump over.");
        }

        self.currentChunk().code.items[offset] = @intCast(u8, (jump >> 8) & 0xff);
        self.currentChunk().code.items[offset + 1] = @intCast(u8, jump & 0xff);
    }

    pub fn emitLoop(self: *Parser, loopStart: usize) !void {
        try self.emitOp(.Loop);

        const offset = self.currentChunk().code.items.len - loopStart + 2;
        if (offset > maxInt(u16)) self.err("Loop body too large.");

        try self.emitByte(@intCast(u8, (offset >> 8) & 0xff));
        try self.emitByte(@intCast(u8, offset & 0xff));
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
        if (constant > maxInt(u8)) {
            self.err("Too many constants in one chunk.");
            return 0;
        }

        return @intCast(u8, constant);
    }

    pub fn declaration(self: *Parser) !void {
        if (self.match(.Var)) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }

        if (self.panicMode) self.synchronize();
    }

    pub fn statement(self: *Parser) !void {
        if (self.match(.Print)) {
            try self.printStatement();
        } else if (self.match(.If)) {
            try self.ifStatement();
        } else if (self.match(.While)) {
            try self.whileStatement();
        } else if (self.match(.For)) {
            try self.forStatement();
        } else if (self.match(.LeftBrace)) {
            self.beginScope();
            try self.block();
            try self.endScope();
        } else {
            try self.expressionStatement();
        }
    }

    pub fn beginScope(self: *Parser) void {
        self.compiler.scopeDepth += 1;
    }

    pub fn endScope(self: *Parser) !void {
        self.compiler.scopeDepth -= 1;

        var locals = self.compiler.locals;
        while (locals.items.len > 0 and
            locals.items[locals.items.len - 1].depth > self.compiler.scopeDepth)
        {
            try self.emitOp(.Pop);
            _ = locals.pop();
        }
    }

    pub fn expression(self: *Parser) !void {
        try self.parsePrecedence(.Assignment);
    }

    pub fn block(self: *Parser) CompilerErrors!void {
        while (!self.check(.RightBrace) and !self.check(.Eof)) {
            try self.declaration();
        }

        self.consume(.RightBrace, "Expect '}' after block.");
    }

    pub fn varDeclaration(self: *Parser) !void {
        const global: u8 = try self.parseVariable("Expect variable name");

        if (self.match(.Equal)) {
            try self.expression();
        } else {
            try self.emitOp(.Nil);
        }
        self.consume(.Semicolon, "Expect ';' after variable declaration.");

        try self.defineVariable(global);
    }

    pub fn printStatement(self: *Parser) !void {
        try self.expression();
        self.consume(.Semicolon, "Expect ';' after value.");
        try self.emitOp(.Print);
    }

    pub fn ifStatement(self: *Parser) CompilerErrors!void {
        self.consume(.LeftParen, "Expect '(' after 'if'.");
        try self.expression();
        self.consume(.RightParen, "Expect ')' after condition.");

        const thenJump = try self.emitJump(.JumpIfFalse);
        try self.emitOp(.Pop);
        try self.statement();
        const elseJump = try self.emitJump(.Jump);

        self.patchJump(thenJump);
        try self.emitOp(.Pop);

        if (self.match(.Else)) try self.statement();
        self.patchJump(elseJump);
    }

    pub fn whileStatement(self: *Parser) CompilerErrors!void {
        const loopStart = self.currentChunk().code.items.len;

        self.consume(.LeftParen, "Expect '(' after 'while'.");
        try self.expression();
        self.consume(.RightParen, "Expect ')' after condition.");

        const exitJump = try self.emitJump(.JumpIfFalse);

        try self.emitOp(.Pop);
        try self.statement();

        try self.emitLoop(loopStart);

        self.patchJump(exitJump);
        try self.emitOp(.Pop);
    }

    pub fn forStatement(self: *Parser) CompilerErrors!void {
        self.beginScope();

        self.consume(.LeftParen, "Expect '(' after 'for'.");
        if (self.match(.Semicolon)) {
            // No initializer
        } else if (self.match(.Var)) {
            try self.varDeclaration();
        } else {
            try self.expressionStatement();
        }

        var loopStart = self.currentChunk().code.items.len;

        var maybeExitJump: ?usize = null;
        if (!self.match(.Semicolon)) {
            try self.expression();
            self.consume(.Semicolon, "Expect ';' after loop condition.");

            // Jump out of the loop if the condition is false
            maybeExitJump = try self.emitJump(.JumpIfFalse);
            try self.emitOp(.Pop); // Condition
        }

        if (!self.match(.RightParen)) {
            const bodyJump = try self.emitJump(.Jump);

            const incrementStart = self.currentChunk().code.items.len;
            try self.expression();
            try self.emitOp(.Pop);
            self.consume(.RightParen, "Expect ')' after for clauses.");

            try self.emitLoop(loopStart);
            loopStart = incrementStart;
            self.patchJump(bodyJump);
        }

        try self.statement();

        try self.emitLoop(loopStart);

        if (maybeExitJump) |exitJump| {
            self.patchJump(exitJump);
            try self.emitOp(.Pop);
        }

        try self.endScope();
    }

    pub fn expressionStatement(self: *Parser) !void {
        try self.expression();
        self.consume(.Semicolon, "Expect ';' after value.");
        try self.emitOp(.Pop);
    }

    pub fn synchronize(self: *Parser) void {
        self.panicMode = false;

        while (!self.check(.Eof)) {
            if (self.previous.tokenType == .Semicolon) return;

            switch (self.current.tokenType) {
                .Class, .Fun, .Var, .For, .If, .While, .Print, .Return => return,
                else => self.advance(),
            }
        }
    }

    pub fn parsePrecedence(self: *Parser, precedence: Precedence) CompilerErrors!void {
        self.advance();

        const canAssign = @enumToInt(precedence) <= @enumToInt(Precedence.Assignment);
        try self.prefix(self.previous.tokenType, canAssign);

        while (@enumToInt(precedence) <= @enumToInt(getPrecedence(self.current.tokenType))) {
            self.advance();
            try self.infix(self.previous.tokenType, canAssign);
        }

        if (canAssign and self.match(.Equal)) {
            self.err("Invalid assignment target.");
        }
    }

    pub fn parseVariable(self: *Parser, message: []const u8) !u8 {
        self.consume(.Identifier, message);

        try self.declareVariable();
        if (self.compiler.scopeDepth > 0) return 0;

        return try self.identifierConstant(self.previous.lexeme);
    }

    pub fn defineVariable(self: *Parser, global: u8) !void {
        if (self.compiler.scopeDepth > 0) {
            self.markInitialized();
            return;
        }

        try self.emitUnaryOp(.DefineGlobal, global);
    }

    pub fn and_(self: *Parser) !void {
        const endJump = try self.emitJump(.JumpIfFalse);
        try self.emitOp(.Pop);
        try self.parsePrecedence(.And);
        self.patchJump(endJump);
    }

    pub fn or_(self: *Parser) !void {
        const elseJump = try self.emitJump(.JumpIfFalse);
        const endJump = try self.emitJump(.Jump);

        self.patchJump(elseJump);
        try self.emitOp(.Pop);

        try self.parsePrecedence(.Or);
        self.patchJump(endJump);
    }

    pub fn markInitialized(self: *Parser) void {
        var locals = self.compiler.locals;
        locals.items[locals.items.len - 1].depth = @intCast(isize, self.compiler.scopeDepth);
    }

    pub fn identifierConstant(self: *Parser, name: []const u8) !u8 {
        return try self.makeConstant(try self.stringValue(name));
    }

    pub fn declareVariable(self: *Parser) !void {
        if (self.compiler.scopeDepth == 0) return;

        const name = self.previous.lexeme;

        var i: usize = 0;
        while (i < self.compiler.locals.items.len) : (i += 1) {
            const local = self.compiler.locals.items[self.compiler.locals.items.len - 1 - i];
            if (local.depth != -1 and local.depth < self.compiler.scopeDepth) break;

            if (std.mem.eql(u8, name, local.name)) {
                self.err("Variable with this name already declared in this scope.");
            }
        }

        try self.addLocal(name);
    }

    pub fn resolveLocal(self: *Parser, name: []const u8) isize {
        var i: usize = 0;
        var locals = self.compiler.locals;
        while (i < locals.items.len) : (i += 1) {
            const local = locals.items[locals.items.len - 1 - i];
            if (std.mem.eql(u8, name, local.name)) {
                if (local.depth == -1) {
                    self.err("Cannot read local variable in its own initializer.");
                }
                return @intCast(isize, locals.items.len - 1 - i);
            }
        }

        return -1;
    }

    pub fn addLocal(self: *Parser, name: []const u8) !void {
        if (self.compiler.locals.items.len > maxInt(u8)) {
            self.err("Too many local variables in function.");
            return;
        }

        const local = Local{
            .name = name,
            .depth = -1,
        };
        try self.compiler.locals.append(local);
    }

    pub fn prefix(self: *Parser, tokenType: TokenType, canAssign: bool) !void {
        switch (tokenType) {
            // Single-character tokens.
            .LeftParen => try self.grouping(),
            .Minus => try self.unary(),
            .RightParen, .LeftBrace, .RightBrace, .Comma, .Dot => self.prefixError(),
            .Plus, .Semicolon, .Slash, .Star => self.prefixError(),

            // One or two character tokens.
            .Bang => try self.unary(),
            .Equal, .BangEqual, .EqualEqual, .Greater, .GreaterEqual => self.prefixError(),
            .Less, .LessEqual => self.prefixError(),

            // Literals.
            .Identifier => try self.variable(canAssign),
            .String => try self.string(),
            .Number => try self.number(),

            // Keywords.
            .Nil, .True, .False => try self.literal(),
            .And, .Class, .Else, .For, .Fun, .If, .Or => self.prefixError(),
            .Print, .Return, .Super, .This, .Var, .While, .Error, .Eof => self.prefixError(),
        }
    }

    pub fn infix(self: *Parser, tokenType: TokenType, canAssign: bool) !void {
        switch (tokenType) {
            // Single-character tokens.
            .Minus, .Plus, .Slash, .Star => try self.binary(),
            .LeftParen, .RightParen, .LeftBrace, .RightBrace, .Comma, .Dot => self.infixError(),
            .Semicolon => self.infixError(),

            // One or two character tokens.
            .BangEqual, .EqualEqual, .Greater, .GreaterEqual => try self.binary(),
            .Less, .LessEqual => try self.binary(),

            .Bang, .Equal => self.infixError(),

            // Literals.
            .Identifier, .String, .Number => self.infixError(),

            // Keywords.
            .And => try self.and_(),
            .Or => try self.or_(),
            .Class, .Else, .False, .For, .Fun, .If, .Nil => self.infixError(),
            .Print, .Return, .Super, .This, .True, .Var, .While, .Error, .Eof => self.infixError(),
        }
    }

    pub fn number(self: *Parser) !void {
        if (std.fmt.parseFloat(f64, self.previous.lexeme)) |value| {
            try self.emitConstant(Value{ .Number = value });
        } else |e| switch (e) {
            error.InvalidCharacter => {
                self.err("Could not parse number");
                return;
            },
        }
    }

    pub fn literal(self: *Parser) !void {
        switch (self.previous.tokenType) {
            .Nil => try self.emitOp(.Nil),
            .True => try self.emitOp(.True),
            .False => try self.emitOp(.False),
            else => self.err("Unexpected literal"), // unreachable
        }
    }

    pub fn stringValue(self: *Parser, source: []const u8) !Value {
        const buffer = try self.vm.allocator.alloc(u8, source.len);
        std.mem.copy(u8, buffer, source);
        const obj = try Obj.string(self.vm, buffer);
        return obj.value();
    }

    pub fn string(self: *Parser) !void {
        const source = self.previous.lexeme[1 .. self.previous.lexeme.len - 1];
        try self.emitConstant(try self.stringValue(source));
    }

    pub fn variable(self: *Parser, canAssign: bool) !void {
        try self.namedVariable(self.previous.lexeme, canAssign);
    }

    pub fn namedVariable(self: *Parser, name: []const u8, canAssign: bool) !void {
        var getOp: OpCode = undefined;
        var setOp: OpCode = undefined;
        var arg: u8 = undefined;
        var resolvedArg = self.resolveLocal(name);

        if (resolvedArg == -1) {
            arg = try self.identifierConstant(name);
            getOp = .GetGlobal;
            setOp = .SetGlobal;
        } else {
            arg = @intCast(u8, resolvedArg);
            getOp = .GetLocal;
            setOp = .SetLocal;
        }

        if (canAssign and self.match(.Equal)) {
            try self.expression();
            try self.emitUnaryOp(setOp, arg);
        } else {
            try self.emitUnaryOp(getOp, arg);
        }
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
