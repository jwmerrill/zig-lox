const std = @import("std");
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
const debug = @import("./debug.zig");

pub fn compile(vm: *VM, source: []const u8) !*Obj.Function {
    var compiler = try Compiler.init(vm, .Script, null);
    defer compiler.deinit();

    var parser = try Parser.init(vm, &compiler, source);

    // Register the parser with the VM. Need to do this so that the
    // garbage collector can reach the parser's current compiler.
    vm.parser = &parser;
    defer vm.parser = null;

    try parser.advance();
    while (!(try parser.match(.Eof))) {
        try parser.declaration();
    }
    try parser.consume(.Eof, "Expect end of expression.");
    const fun = try parser.end();

    if (parser.hadError) return error.CompileError;
    return fun;
}

const FunctionType = enum { Function, Initializer, Method, Script };

const Upvalue = struct {
    index: u8,
    isLocal: bool,
};

pub const Compiler = struct {
    enclosing: ?*Compiler,
    function: *Obj.Function,
    functionType: FunctionType,
    locals: std.ArrayList(Local),
    upvalues: std.ArrayList(Upvalue),
    scopeDepth: usize,

    pub fn init(vm: *VM, functionType: FunctionType, enclosing: ?*Compiler) !Compiler {
        var locals = std.ArrayList(Local).init(vm.allocator);

        // First local is reserved to represent the current function
        // value on the stack. Give it a name of "" to make sure it
        // can't actually be referenced by local variables.
        try locals.append(Local{
            .depth = 0,
            .isCaptured = false,
            .name = if (functionType == .Function) "" else "this",
        });

        return Compiler{
            .enclosing = enclosing,
            // NOTE, book warns we should initialize function to null
            // and set it later for GC reasons, but that doesn't appear
            // necessary to me.
            .function = try Obj.Function.create(vm),
            .functionType = functionType,
            .locals = locals,
            .upvalues = std.ArrayList(Upvalue).init(vm.allocator),
            .scopeDepth = 0,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.locals.deinit();
        self.upvalues.deinit();
    }
};

const ClassCompiler = struct {
    enclosing: ?*ClassCompiler,
    name: []const u8,
    hasSuperclass: bool,
};

pub const Local = struct {
    name: []const u8,
    depth: isize,
    isCaptured: bool,
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

// NOTE, have to spell this out explicitly right now because zig has
// trouble inferring error sets for recursive functions.
//
// See https://github.com/ziglang/zig/issues/2971
const CompilerErrors = error{OutOfMemory} || std.os.WriteError;

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

pub const Parser = struct {
    vm: *VM,
    scanner: Scanner,
    current: Token,
    previous: Token,
    hadError: bool,
    panicMode: bool,
    compiler: *Compiler,
    currentClass: ?*ClassCompiler,

    pub fn init(vm: *VM, compiler: *Compiler, source: []const u8) !Parser {
        return Parser{
            .vm = vm,
            .scanner = Scanner.init(source),
            .current = undefined,
            .currentClass = null,
            .previous = undefined,
            .hadError = false,
            .panicMode = false,
            .compiler = compiler,
        };
    }

    pub fn currentChunk(self: *Parser) *Chunk {
        return &self.compiler.function.chunk;
    }

    fn advance(self: *Parser) !void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();
            if (!self.check(.Error)) break;
            try self.errorAtCurrent(self.current.lexeme);
        }
    }

    fn check(self: *Parser, tokenType: TokenType) bool {
        return self.current.tokenType == tokenType;
    }

    fn consume(self: *Parser, tokenType: TokenType, message: []const u8) !void {
        if (self.check(tokenType)) {
            try self.advance();
        } else {
            try self.errorAtCurrent(message);
        }
    }

    fn match(self: *Parser, tokenType: TokenType) !bool {
        if (!self.check(tokenType)) return false;
        try self.advance();
        return true;
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) !void {
        try self.errorAt(&self.current, message);
    }

    fn err(self: *Parser, message: []const u8) !void {
        try self.errorAt(&self.previous, message);
    }

    fn prefixError(self: *Parser) !void {
        try self.err("Expect expression.");
    }

    fn infixError(self: *Parser) !void {
        try self.err("Expect expression.");
    }

    fn errorAt(self: *Parser, token: *Token, message: []const u8) !void {
        if (self.panicMode) return;
        self.panicMode = true;

        try self.vm.errWriter.print("[line {}] Error", .{token.line});

        switch (token.tokenType) {
            .Eof => {
                try self.vm.errWriter.print(" at end", .{});
            },
            .Error => {},
            else => {
                try self.vm.errWriter.print(" at '{s}'", .{token.lexeme});
            },
        }

        try self.vm.errWriter.print(": {s}\n", .{message});

        self.hadError = true;
    }

    fn emitJump(self: *Parser, op: OpCode) !usize {
        try self.emitOp(op);
        // Dummy operands that will be patched later
        try self.emitByte(0xff);
        try self.emitByte(0xff);
        return self.currentChunk().code.items.len - 2;
    }

    fn patchJump(self: *Parser, offset: usize) !void {
        const jump = self.currentChunk().code.items.len - offset - 2;

        if (jump > maxInt(u16)) {
            try self.err("Too much code to jump over.");
        }

        self.currentChunk().code.items[offset] = @intCast(u8, (jump >> 8) & 0xff);
        self.currentChunk().code.items[offset + 1] = @intCast(u8, jump & 0xff);
    }

    fn emitLoop(self: *Parser, loopStart: usize) !void {
        try self.emitOp(.Loop);

        const offset = self.currentChunk().code.items.len - loopStart + 2;
        if (offset > maxInt(u16)) try self.err("Loop body too large.");

        try self.emitByte(@intCast(u8, (offset >> 8) & 0xff));
        try self.emitByte(@intCast(u8, offset & 0xff));
    }

    fn emitByte(self: *Parser, byte: u8) !void {
        try self.currentChunk().write(byte, self.previous.line);
    }

    fn emitOp(self: *Parser, op: OpCode) !void {
        try self.currentChunk().writeOp(op, self.previous.line);
    }

    fn emitUnaryOp(self: *Parser, op: OpCode, byte: u8) !void {
        try self.emitOp(op);
        try self.emitByte(byte);
    }

    fn emitConstant(self: *Parser, value: Value) !void {
        try self.emitUnaryOp(.Constant, try self.makeConstant(value));
    }

    fn emitReturn(self: *Parser) !void {
        switch (self.compiler.functionType) {
            .Initializer => try self.emitUnaryOp(.GetLocal, 0),
            .Function, .Method, .Script => try self.emitOp(.Nil),
        }

        try self.emitOp(.Return);
    }

    fn end(self: *Parser) !*Obj.Function {
        try self.emitReturn();

        if (debug.PRINT_CODE) {
            if (!self.hadError) {
                const maybeName = self.compiler.function.name;
                const name = if (maybeName) |o| o.bytes else "<script>";
                self.currentChunk().disassemble(name);
            }
        }

        const fun = self.compiler.function;
        if (self.compiler.enclosing) |compiler| {
            self.compiler = compiler;
        }

        return fun;
    }

    fn makeConstant(self: *Parser, value: Value) !u8 {
        // Make sure value is visible to the GC while addConstant
        // allocates
        self.vm.push(value);
        const constant = try self.currentChunk().addConstant(value);
        _ = self.vm.pop();

        if (constant > maxInt(u8)) {
            try self.err("Too many constants in one chunk.");
            return 0;
        }

        return @intCast(u8, constant);
    }

    fn declaration(self: *Parser) !void {
        if (try self.match(.Class)) {
            try self.classDeclaration();
        } else if (try self.match(.Fun)) {
            try self.funDeclaration();
        } else if (try self.match(.Var)) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }

        if (self.panicMode) try self.synchronize();
    }

    fn statement(self: *Parser) !void {
        if (try self.match(.Print)) {
            try self.printStatement();
        } else if (try self.match(.Return)) {
            try self.returnStatement();
        } else if (try self.match(.If)) {
            try self.ifStatement();
        } else if (try self.match(.While)) {
            try self.whileStatement();
        } else if (try self.match(.For)) {
            try self.forStatement();
        } else if (try self.match(.LeftBrace)) {
            self.beginScope();
            try self.block();
            try self.endScope();
        } else {
            try self.expressionStatement();
        }
    }

    fn beginScope(self: *Parser) void {
        self.compiler.scopeDepth += 1;
    }

    fn endScope(self: *Parser) !void {
        self.compiler.scopeDepth -= 1;

        var locals = &self.compiler.locals;
        while (locals.items.len > 0 and
            locals.items[locals.items.len - 1].depth > self.compiler.scopeDepth)
        {
            if (locals.items[locals.items.len - 1].isCaptured) {
                try self.emitOp(.CloseUpvalue);
            } else {
                try self.emitOp(.Pop);
            }
            _ = locals.pop();
        }
    }

    fn expression(self: *Parser) !void {
        try self.parsePrecedence(.Assignment);
    }

    fn block(self: *Parser) CompilerErrors!void {
        while (!self.check(.RightBrace) and !self.check(.Eof)) {
            try self.declaration();
        }

        try self.consume(.RightBrace, "Expect '}' after block.");
    }

    fn function(self: *Parser, functionType: FunctionType) !void {
        var compiler = try Compiler.init(self.vm, functionType, self.compiler);
        defer compiler.deinit();
        self.compiler = &compiler;
        self.compiler.function.name = try Obj.String.copy(self.vm, self.previous.lexeme);
        self.beginScope();

        // Compile the parameter list
        try self.consume(.LeftParen, "Expect '(' after function name.");
        if (!self.check(.RightParen)) {
            while (true) {
                if (self.compiler.function.arity == 255) {
                    try self.errorAtCurrent("Cannot have more than 255 parameters.");
                }

                self.compiler.function.arity += 1;
                const paramConstant = try self.parseVariable("Expect parameter name.");
                try self.defineVariable(paramConstant);
                if (!try self.match(.Comma)) break;
            }
        }
        try self.consume(.RightParen, "Expect ')' after parameters.");

        // The body.
        try self.consume(.LeftBrace, "Expect '{' before function body.");
        try self.block();

        const fun = try self.end();
        try self.emitUnaryOp(.Closure, try self.makeConstant(fun.obj.value()));

        for (compiler.upvalues.items) |upvalue| {
            try self.emitByte(if (upvalue.isLocal) 1 else 0);
            try self.emitByte(upvalue.index);
        }
    }

    fn method(self: *Parser) !void {
        try self.consume(.Identifier, "Expect method name.");
        const constant = try self.identifierConstant(self.previous.lexeme);

        const isInit = std.mem.eql(u8, self.previous.lexeme, "init");

        try self.function(if (isInit) .Initializer else .Method);
        try self.emitUnaryOp(.Method, constant);
    }

    fn classDeclaration(self: *Parser) !void {
        try self.consume(.Identifier, "Expect class name.");
        const className = self.previous.lexeme;
        const nameConstant = try self.identifierConstant(className);
        try self.declareVariable();

        try self.emitUnaryOp(.Class, nameConstant);
        try self.defineVariable(nameConstant);

        var classCompiler = ClassCompiler{
            .name = className,
            .enclosing = self.currentClass,
            .hasSuperclass = false,
        };
        self.currentClass = &classCompiler;
        defer self.currentClass = self.currentClass.?.enclosing;

        if (try self.match(.Less)) {
            try self.consume(.Identifier, "Expect superclass name.");
            try self.variable(false);

            if (std.mem.eql(u8, className, self.previous.lexeme)) {
                try self.err("A class cannot inherit from itself.");
            }

            self.beginScope();
            try self.addLocal("super");
            try self.defineVariable(0);

            try self.namedVariable(className, false);
            try self.emitOp(.Inherit);
            classCompiler.hasSuperclass = true;
        }

        try self.namedVariable(className, false);
        try self.consume(.LeftBrace, "Expect '{' before class body.");

        while (!self.check(.RightBrace) and !self.check(.Eof)) {
            try self.method();
        }

        try self.consume(.RightBrace, "Expect '}' after class body.");
        // Pop the class now that we're done adding methods
        try self.emitOp(.Pop);

        if (classCompiler.hasSuperclass) try self.endScope();
    }

    fn funDeclaration(self: *Parser) !void {
        const global = try self.parseVariable("Expect function name.");
        self.markInitialized();
        try self.function(.Function);
        try self.defineVariable(global);
    }

    fn varDeclaration(self: *Parser) !void {
        const global: u8 = try self.parseVariable("Expect variable name.");

        if (try self.match(.Equal)) {
            try self.expression();
        } else {
            try self.emitOp(.Nil);
        }
        try self.consume(.Semicolon, "Expect ';' after variable declaration.");

        try self.defineVariable(global);
    }

    fn printStatement(self: *Parser) !void {
        try self.expression();
        try self.consume(.Semicolon, "Expect ';' after value.");
        try self.emitOp(.Print);
    }

    fn returnStatement(self: *Parser) !void {
        if (self.compiler.functionType == .Script) {
            return try self.err("Cannot return from top-level code.");
        }

        if (try self.match(.Semicolon)) {
            try self.emitReturn();
        } else {
            if (self.compiler.functionType == .Initializer) {
                try self.err("Cannot return a value from an initializer.");
            }

            try self.expression();
            try self.consume(.Semicolon, "Expect ';' after return value.");
            try self.emitOp(.Return);
        }
    }

    fn ifStatement(self: *Parser) CompilerErrors!void {
        try self.consume(.LeftParen, "Expect '(' after 'if'.");
        try self.expression();
        try self.consume(.RightParen, "Expect ')' after condition.");

        const thenJump = try self.emitJump(.JumpIfFalse);
        try self.emitOp(.Pop);
        try self.statement();
        const elseJump = try self.emitJump(.Jump);

        try self.patchJump(thenJump);
        try self.emitOp(.Pop);

        if (try self.match(.Else)) try self.statement();
        try self.patchJump(elseJump);
    }

    fn whileStatement(self: *Parser) CompilerErrors!void {
        const loopStart = self.currentChunk().code.items.len;

        try self.consume(.LeftParen, "Expect '(' after 'while'.");
        try self.expression();
        try self.consume(.RightParen, "Expect ')' after condition.");

        const exitJump = try self.emitJump(.JumpIfFalse);

        try self.emitOp(.Pop);
        try self.statement();

        try self.emitLoop(loopStart);

        try self.patchJump(exitJump);
        try self.emitOp(.Pop);
    }

    fn forStatement(self: *Parser) CompilerErrors!void {
        self.beginScope();

        try self.consume(.LeftParen, "Expect '(' after 'for'.");
        if (try self.match(.Semicolon)) {
            // No initializer
        } else if (try self.match(.Var)) {
            try self.varDeclaration();
        } else {
            try self.expressionStatement();
        }

        var loopStart = self.currentChunk().code.items.len;

        var maybeExitJump: ?usize = null;
        if (!try self.match(.Semicolon)) {
            try self.expression();
            try self.consume(.Semicolon, "Expect ';' after loop condition.");

            // Jump out of the loop if the condition is false
            maybeExitJump = try self.emitJump(.JumpIfFalse);
            try self.emitOp(.Pop); // Condition
        }

        if (!try self.match(.RightParen)) {
            const bodyJump = try self.emitJump(.Jump);

            const incrementStart = self.currentChunk().code.items.len;
            try self.expression();
            try self.emitOp(.Pop);
            try self.consume(.RightParen, "Expect ')' after for clauses.");

            try self.emitLoop(loopStart);
            loopStart = incrementStart;
            try self.patchJump(bodyJump);
        }

        try self.statement();

        try self.emitLoop(loopStart);

        if (maybeExitJump) |exitJump| {
            try self.patchJump(exitJump);
            try self.emitOp(.Pop);
        }

        try self.endScope();
    }

    fn expressionStatement(self: *Parser) !void {
        try self.expression();
        try self.consume(.Semicolon, "Expect ';' after expression.");
        try self.emitOp(.Pop);
    }

    fn synchronize(self: *Parser) !void {
        self.panicMode = false;

        while (!self.check(.Eof)) {
            if (self.previous.tokenType == .Semicolon) return;

            switch (self.current.tokenType) {
                .Class, .Fun, .Var, .For, .If, .While, .Print, .Return => return,
                else => try self.advance(),
            }
        }
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) CompilerErrors!void {
        try self.advance();

        const canAssign = @enumToInt(precedence) <= @enumToInt(Precedence.Assignment);
        try self.prefix(self.previous.tokenType, canAssign);

        while (@enumToInt(precedence) <= @enumToInt(getPrecedence(self.current.tokenType))) {
            try self.advance();
            try self.infix(self.previous.tokenType, canAssign);
        }

        if (canAssign and try self.match(.Equal)) {
            try self.err("Invalid assignment target.");
        }
    }

    fn parseVariable(self: *Parser, message: []const u8) !u8 {
        try self.consume(.Identifier, message);

        try self.declareVariable();
        if (self.compiler.scopeDepth > 0) return 0;

        return try self.identifierConstant(self.previous.lexeme);
    }

    fn defineVariable(self: *Parser, global: u8) !void {
        if (self.compiler.scopeDepth > 0) {
            self.markInitialized();
            return;
        }

        try self.emitUnaryOp(.DefineGlobal, global);
    }

    fn and_(self: *Parser) !void {
        const endJump = try self.emitJump(.JumpIfFalse);
        try self.emitOp(.Pop);
        try self.parsePrecedence(.And);
        try self.patchJump(endJump);
    }

    fn or_(self: *Parser) !void {
        const elseJump = try self.emitJump(.JumpIfFalse);
        const endJump = try self.emitJump(.Jump);

        try self.patchJump(elseJump);
        try self.emitOp(.Pop);

        try self.parsePrecedence(.Or);
        try self.patchJump(endJump);
    }

    fn markInitialized(self: *Parser) void {
        const depth = self.compiler.scopeDepth;
        if (depth == 0) return;
        var locals = &self.compiler.locals;
        locals.items[locals.items.len - 1].depth = @intCast(isize, depth);
    }

    fn identifierConstant(self: *Parser, name: []const u8) !u8 {
        return try self.makeConstant(try self.stringValue(name));
    }

    fn declareVariable(self: *Parser) !void {
        if (self.compiler.scopeDepth == 0) return;

        const name = self.previous.lexeme;

        var i: usize = 0;
        while (i < self.compiler.locals.items.len) : (i += 1) {
            const local = self.compiler.locals.items[self.compiler.locals.items.len - 1 - i];
            if (local.depth != -1 and local.depth < self.compiler.scopeDepth) break;

            if (std.mem.eql(u8, name, local.name)) {
                try self.err("Variable with this name already declared in this scope.");
            }
        }

        try self.addLocal(name);
    }

    fn addUpvalue(self: *Parser, compiler: *Compiler, index: u8, isLocal: bool) !usize {
        for (compiler.upvalues.items) |upvalue, i| {
            if (upvalue.index == index and upvalue.isLocal == isLocal) {
                return i;
            }
        }

        if (compiler.upvalues.items.len > maxInt(u8)) {
            try self.err("Too many closure variables in function.");
            return 0;
        }

        try compiler.upvalues.append(Upvalue{
            .isLocal = isLocal,
            .index = index,
        });
        compiler.function.upvalueCount += 1;

        return compiler.upvalues.items.len - 1;
    }

    fn resolveLocal(self: *Parser, compiler: *Compiler, name: []const u8) !isize {
        var locals = &compiler.locals;

        var i: usize = 0;
        while (i < locals.items.len) : (i += 1) {
            const local = locals.items[locals.items.len - 1 - i];
            if (std.mem.eql(u8, name, local.name)) {
                if (local.depth == -1) {
                    try self.err("Cannot read local variable in its own initializer.");
                }
                return @intCast(isize, locals.items.len - 1 - i);
            }
        }

        return -1;
    }

    fn resolveUpvalue(self: *Parser, compiler: *Compiler, name: []const u8) CompilerErrors!isize {
        if (compiler.enclosing) |enclosing| {
            const local = try self.resolveLocal(enclosing, name);
            if (local != -1) {
                enclosing.locals.items[@intCast(u8, local)].isCaptured = true;
                const index = try self.addUpvalue(compiler, @intCast(u8, local), true);
                return @intCast(isize, index);
            }

            const upvalue = try self.resolveUpvalue(enclosing, name);
            if (upvalue != -1) {
                const index = try self.addUpvalue(compiler, @intCast(u8, upvalue), false);
                return @intCast(isize, index);
            }
        }

        return -1;
    }

    fn addLocal(self: *Parser, name: []const u8) !void {
        if (self.compiler.locals.items.len > maxInt(u8)) {
            try self.err("Too many local variables in function.");
            return;
        }

        const local = Local{
            .name = name,
            .depth = -1,
            .isCaptured = false,
        };
        try self.compiler.locals.append(local);
    }

    fn prefix(self: *Parser, tokenType: TokenType, canAssign: bool) !void {
        switch (tokenType) {
            // Single-character tokens.
            .LeftParen => try self.grouping(),
            .Minus => try self.unary(),
            .RightParen, .LeftBrace, .RightBrace, .Comma, .Dot => try self.prefixError(),
            .Plus, .Semicolon, .Slash, .Star => try self.prefixError(),

            // One or two character tokens.
            .Bang => try self.unary(),
            .Equal, .BangEqual, .EqualEqual, .Greater, .GreaterEqual => try self.prefixError(),
            .Less, .LessEqual => try self.prefixError(),

            // Literals.
            .Identifier => try self.variable(canAssign),
            .String => try self.string(),
            .Number => try self.number(),

            // Keywords.
            .Nil, .True, .False => try self.literal(),
            .This => try self.this(),
            .Super => try self.super(),
            .And, .Class, .Else, .For, .Fun, .If, .Or => try self.prefixError(),
            .Print, .Return, .Var, .While, .Error, .Eof => try self.prefixError(),
        }
    }

    fn infix(self: *Parser, tokenType: TokenType, canAssign: bool) !void {
        switch (tokenType) {
            // Single-character tokens.
            .Minus, .Plus, .Slash, .Star => try self.binary(),
            .LeftParen => try self.call(),
            .Dot => try self.dot(canAssign),
            .RightParen, .LeftBrace, .RightBrace, .Comma, .Semicolon => try self.infixError(),

            // One or two character tokens.
            .BangEqual, .EqualEqual, .Greater, .GreaterEqual => try self.binary(),
            .Less, .LessEqual => try self.binary(),

            .Bang, .Equal => try self.infixError(),

            // Literals.
            .Identifier, .String, .Number => try self.infixError(),

            // Keywords.
            .And => try self.and_(),
            .Or => try self.or_(),
            .Class, .Else, .False, .For, .Fun, .If, .Nil => try self.infixError(),
            .Print, .Return, .Super, .This, .True, .Var, .While, .Error, .Eof => try self.infixError(),
        }
    }

    fn number(self: *Parser) !void {
        if (std.fmt.parseFloat(f64, self.previous.lexeme)) |value| {
            try self.emitConstant(Value.fromNumber(value));
        } else |e| switch (e) {
            error.InvalidCharacter => {
                try self.err("Could not parse number");
                return;
            },
        }
    }

    fn literal(self: *Parser) !void {
        switch (self.previous.tokenType) {
            .Nil => try self.emitOp(.Nil),
            .True => try self.emitOp(.True),
            .False => try self.emitOp(.False),
            else => try self.err("Unexpected literal"), // unreachable
        }
    }

    fn stringValue(self: *Parser, source: []const u8) !Value {
        return (try Obj.String.copy(self.vm, source)).obj.value();
    }

    fn string(self: *Parser) !void {
        const source = self.previous.lexeme[1 .. self.previous.lexeme.len - 1];
        try self.emitConstant(try self.stringValue(source));
    }

    fn variable(self: *Parser, canAssign: bool) !void {
        try self.namedVariable(self.previous.lexeme, canAssign);
    }

    fn this(self: *Parser) !void {
        if (self.currentClass == null) {
            try self.err("Cannot use 'this' outside of a class.");
            return;
        }
        try self.variable(false);
    }

    fn super(self: *Parser) !void {
        if (self.currentClass) |currentClass| {
            if (!currentClass.hasSuperclass) {
                try self.err("Cannot use 'super' in a class with no superclass.");
            }
        } else {
            try self.err("Cannot use 'super' outside of a class.");
        }

        try self.consume(.Dot, "Expect '.' after 'super'.");
        try self.consume(.Identifier, "Expect superclass method name.");
        const name = try self.identifierConstant(self.previous.lexeme);

        try self.namedVariable("this", false);
        if (try self.match(.LeftParen)) {
            const argCount = try self.argumentList();
            try self.namedVariable("super", false);
            try self.emitUnaryOp(.SuperInvoke, name);
            try self.emitByte(argCount);
        } else {
            try self.namedVariable("super", false);
            try self.emitUnaryOp(.GetSuper, name);
        }
    }

    fn namedVariable(self: *Parser, name: []const u8, canAssign: bool) !void {
        var getOp: OpCode = undefined;
        var setOp: OpCode = undefined;
        var arg: u8 = undefined;
        var resolvedArg = try self.resolveLocal(self.compiler, name);

        if (resolvedArg != -1) {
            arg = @intCast(u8, resolvedArg);
            getOp = .GetLocal;
            setOp = .SetLocal;
        } else {
            const maybeArg = try self.resolveUpvalue(self.compiler, name);
            if (maybeArg != -1) {
                arg = @intCast(u8, maybeArg);
                getOp = .GetUpvalue;
                setOp = .SetUpvalue;
            } else {
                arg = try self.identifierConstant(name);
                getOp = .GetGlobal;
                setOp = .SetGlobal;
            }
        }

        if (canAssign and (try self.match(.Equal))) {
            try self.expression();
            try self.emitUnaryOp(setOp, arg);
        } else {
            try self.emitUnaryOp(getOp, arg);
        }
    }

    fn grouping(self: *Parser) !void {
        try self.expression();
        try self.consume(.RightParen, "Expect ')' after expression.");
    }

    fn unary(self: *Parser) !void {
        const operatorType = self.previous.tokenType;

        // Compile the operand
        try self.parsePrecedence(.Unary);

        // Emit the operator instruction
        switch (operatorType) {
            .Bang => try self.emitOp(.Not),
            .Minus => try self.emitOp(.Negate),
            else => try self.err("Unexpected unary operator"), // unreachable
        }
    }

    fn binary(self: *Parser) !void {
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
            else => try self.err("Unexpected binary operator"), // unreachable
        }
    }

    fn call(self: *Parser) !void {
        const argCount = try self.argumentList();
        try self.emitUnaryOp(.Call, argCount);
    }

    fn dot(self: *Parser, canAssign: bool) !void {
        try self.consume(.Identifier, "Expect property name after '.'.");
        const name = try self.identifierConstant(self.previous.lexeme);

        if (canAssign and try self.match(.Equal)) {
            try self.expression();
            try self.emitUnaryOp(.SetProperty, name);
        } else if (try self.match(.LeftParen)) {
            const argCount = try self.argumentList();
            try self.emitUnaryOp(.Invoke, name);
            try self.emitByte(argCount);
        } else {
            try self.emitUnaryOp(.GetProperty, name);
        }
    }

    fn argumentList(self: *Parser) !u8 {
        var argCount: u8 = 0;
        if (!self.check(.RightParen)) {
            while (true) {
                try self.expression();

                if (argCount == 255) {
                    try self.err("Cannot have more than 255 arguments.");
                    break;
                }
                argCount += 1;
                if (!try self.match(.Comma)) break;
            }
        }

        try self.consume(.RightParen, "Expect ')' after arguments.");
        return argCount;
    }
};
