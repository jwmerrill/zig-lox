const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const compile = @import("./compiler.zig").compile;
const Parser = @import("./compiler.zig").Parser;
const debug = @import("./debug.zig");
const Obj = @import("./object.zig").Obj;
const Table = @import("./table.zig").Table;
const FixedCapacityStack = @import("./stack.zig").FixedCapacityStack;
const GCAllocator = @import("./memory.zig").GCAllocator;
const VMWriter = @import("./writer.zig").VMWriter;
const clock = @import("./native.zig").clock;

fn add(x: f64, y: f64) f64 {
    return x + y;
}

fn sub(x: f64, y: f64) f64 {
    return x - y;
}

fn mul(x: f64, y: f64) f64 {
    return x * y;
}

fn div(x: f64, y: f64) f64 {
    return x / y;
}

const CallFrame = struct {
    closure: *Obj.Closure,
    ip: usize,
    start: usize,
};

const FRAMES_MAX = 64;
const STACK_MAX = FRAMES_MAX * (std.math.maxInt(u8) + 1);

pub const VM = struct {
    gcAllocatorInstance: GCAllocator,
    allocator: Allocator,
    frames: ArrayList(CallFrame), // NOTE, book uses a fixed size stack
    stack: FixedCapacityStack(Value),
    objects: ?*Obj,
    // Note: book uses a dynamic array for the stack of gray objects (a
    // GC bookkeeping concept), but the need to grow this stack during
    // garbage collection means that GC can fail with OOM. Use an
    // intrusive linked list instead to make it so GC can't fail
    nextGray: ?*Obj,
    openUpvalues: ?*Obj.Upvalue,
    strings: Table,
    initString: ?*Obj.String,
    globals: Table,
    parser: ?*Parser,
    outWriter: VMWriter,
    errWriter: VMWriter,

    pub fn create() VM {
        var vm = VM{
            .gcAllocatorInstance = undefined,
            .allocator = undefined,
            .frames = undefined,
            .stack = undefined,
            .objects = null,
            .openUpvalues = null,
            .strings = undefined,
            .initString = null,
            .globals = undefined,
            .parser = null,
            .nextGray = null,
            .outWriter = undefined,
            .errWriter = undefined,
        };

        return vm;
    }

    pub fn init(self: *VM, backingAllocator: Allocator, outWriter: VMWriter, errWriter: VMWriter) !void {
        self.gcAllocatorInstance = GCAllocator.init(self, backingAllocator);
        const allocator = self.gcAllocatorInstance.allocator();

        // NOTE, we can tell none of this allocates because none of
        // these operations can fail, and allocation can always fail
        // with error.OutOfMemory
        self.allocator = allocator;
        self.frames = std.ArrayList(CallFrame).init(allocator);
        self.strings = Table.init(allocator);
        self.globals = Table.init(allocator);
        self.outWriter = outWriter;
        self.errWriter = errWriter;

        // These ops all allocate
        self.stack = try FixedCapacityStack(Value).init(backingAllocator, STACK_MAX);
        self.initString = try Obj.String.copy(self, "init");
        try self.defineNative("clock", clock);
    }

    pub fn deinit(self: *VM) void {
        self.initString = null;
        self.freeObjects();
        self.strings.deinit();
        self.globals.deinit();
        self.frames.deinit();
        self.stack.deinit();
    }

    pub fn freeObjects(self: *VM) void {
        var object = self.objects;
        while (object) |o| {
            const next = o.next;
            o.destroy(self);
            object = next;
        }
    }

    pub fn interpret(self: *VM, source: []const u8) !void {
        // Stack should be empty when we start and when we finish
        // running
        std.debug.assert(self.stack.items.len == 0);
        defer std.debug.assert(self.stack.items.len == 0);

        errdefer self.resetStack() catch unreachable;
        const function = try compile(self, source);
        self.push(function.obj.value());

        // NOTE need the function on the stack when we allocate the
        // closure so that the GC can see it.
        const closure = try Obj.Closure.create(self, function);
        _ = self.pop();
        self.push(closure.obj.value());

        const currentFrame = try self.call(closure, 0);
        const code = currentFrame.closure.function.chunk.code.items;
        try self.dispatch(currentFrame, code);
        // Pop the closure we put on the stack above
        _ = self.pop();
    }

    const RuntimeErrors = error{ OutOfMemory, RuntimeError } || std.os.WriteError;

    const InstructionCallModifier = .{ .modifier = .always_tail };

    fn dispatch(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        if (debug.TRACE_EXECUTION) {
            // Print debugging information
            try self.printStack();
            _ = currentFrame.closure.function.chunk.disassembleInstruction(currentFrame.ip);
        }

        const instruction = readByte(currentFrame, code);
        const opCode = @intToEnum(OpCode, instruction);

        switch (opCode) {
            .Return => try @call(InstructionCallModifier, runReturn, .{ self, currentFrame, code }),
            .Pop => try @call(InstructionCallModifier, runPop, .{ self, currentFrame, code }),
            .GetLocal => try @call(InstructionCallModifier, runGetLocal, .{ self, currentFrame, code }),
            .SetLocal => try @call(InstructionCallModifier, runSetLocal, .{ self, currentFrame, code }),
            .GetGlobal => try @call(InstructionCallModifier, runGetGlobal, .{ self, currentFrame, code }),
            .DefineGlobal => try @call(InstructionCallModifier, runDefineGlobal, .{ self, currentFrame, code }),
            .SetGlobal => try @call(InstructionCallModifier, runSetGlobal, .{ self, currentFrame, code }),
            .GetUpvalue => try @call(InstructionCallModifier, runGetUpvalue, .{ self, currentFrame, code }),
            .SetUpvalue => try @call(InstructionCallModifier, runSetUpvalue, .{ self, currentFrame, code }),
            .GetProperty => try @call(InstructionCallModifier, runGetProperty, .{ self, currentFrame, code }),
            .SetProperty => try @call(InstructionCallModifier, runSetProperty, .{ self, currentFrame, code }),
            .GetSuper => try @call(InstructionCallModifier, runGetSuper, .{ self, currentFrame, code }),
            .CloseUpvalue => try @call(InstructionCallModifier, runCloseUpvalue, .{ self, currentFrame, code }),
            .Class => try @call(InstructionCallModifier, runClass, .{ self, currentFrame, code }),
            .Inherit => try @call(InstructionCallModifier, runInherit, .{ self, currentFrame, code }),
            .Method => try @call(InstructionCallModifier, runMethod, .{ self, currentFrame, code }),
            .Print => try @call(InstructionCallModifier, runPrint, .{ self, currentFrame, code }),
            .Jump => try @call(InstructionCallModifier, runJump, .{ self, currentFrame, code }),
            .JumpIfFalse => try @call(InstructionCallModifier, runJumpIfFalse, .{ self, currentFrame, code }),
            .Loop => try @call(InstructionCallModifier, runLoop, .{ self, currentFrame, code }),
            .Call => try @call(InstructionCallModifier, runCall, .{ self, currentFrame, code }),
            .Invoke => try @call(InstructionCallModifier, runInvoke, .{ self, currentFrame, code }),
            .SuperInvoke => try @call(InstructionCallModifier, runSuperInvoke, .{ self, currentFrame, code }),
            .Closure => try @call(InstructionCallModifier, runClosure, .{ self, currentFrame, code }),
            .Constant => try @call(InstructionCallModifier, runConstant, .{ self, currentFrame, code }),
            .Nil => try @call(InstructionCallModifier, runNil, .{ self, currentFrame, code }),
            .True => try @call(InstructionCallModifier, runTrue, .{ self, currentFrame, code }),
            .False => try @call(InstructionCallModifier, runFalse, .{ self, currentFrame, code }),
            .Equal => try @call(InstructionCallModifier, runEqual, .{ self, currentFrame, code }),
            .Greater => try @call(InstructionCallModifier, runGreater, .{ self, currentFrame, code }),
            .Less => try @call(InstructionCallModifier, runLess, .{ self, currentFrame, code }),
            .Negate => try @call(InstructionCallModifier, runNegate, .{ self, currentFrame, code }),
            .Add => try @call(InstructionCallModifier, runAdd, .{ self, currentFrame, code }),
            .Subtract => try @call(InstructionCallModifier, runSubtract, .{ self, currentFrame, code }),
            .Multiply => try @call(InstructionCallModifier, runMultiply, .{ self, currentFrame, code }),
            .Divide => try @call(InstructionCallModifier, runDivide, .{ self, currentFrame, code }),
            .Not => try @call(InstructionCallModifier, runNot, .{ self, currentFrame, code }),
        }
    }

    const DispatchCallModifier = .{ .modifier = .always_tail };

    fn runReturn(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        _ = code;
        const start = currentFrame.start;
        const result = self.pop();
        _ = self.frames.pop();

        self.closeUpvalues(&self.stack.items[start]);

        if (self.frames.items.len == 0) return;

        try self.stack.resize(start);
        self.push(result);
        const newFrame = &self.frames.items[self.frames.items.len - 1];
        const newCode = newFrame.closure.function.chunk.code.items;
        try @call(DispatchCallModifier, dispatch, .{ self, newFrame, newCode });
    }

    fn runPop(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        _ = self.pop();
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runGetLocal(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const slot = readByte(currentFrame, code);
        self.push(self.stack.items[currentFrame.start + slot]);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runSetLocal(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const slot = readByte(currentFrame, code);
        self.stack.items[currentFrame.start + slot] = self.peek(0);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runGetGlobal(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const name = readString(currentFrame, code);
        var value: Value = undefined;
        if (!self.globals.get(name, &value)) {
            return self.runtimeError("Undefined variable '{s}'.", .{name.bytes});
        }
        self.push(value);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runDefineGlobal(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        _ = try self.globals.set(readString(currentFrame, code), self.peek(0));
        // NOTE don’t pop until value is in the hash table so
        // that we don't lose the value if the GC runs during
        // the set operation
        _ = self.pop();
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runSetGlobal(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const name = readString(currentFrame, code);
        if (try self.globals.set(name, self.peek(0))) {
            _ = self.globals.delete(name);
            return self.runtimeError("Undefined variable '{s}'.", .{name.bytes});
        }
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runGetUpvalue(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const slot = readByte(currentFrame, code);
        // Upvalues are guaranteed to be filled in by the time we get here
        self.push(currentFrame.closure.upvalues[slot].?.location.*);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runSetUpvalue(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const slot = readByte(currentFrame, code);
        // Upvalues are guaranteed to be filled in by the time we get here
        currentFrame.closure.upvalues[slot].?.location.* = self.peek(0);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runGetProperty(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const maybeObj = self.peek(0);
        if (!maybeObj.isObj()) return self.runtimeError("Only instances have properties.", .{});
        const obj = maybeObj.asObj();
        switch (obj.objType) {
            .String, .Function, .NativeFunction, .Closure, .Upvalue, .Class, .BoundMethod => {
                return self.runtimeError("Only instances have properties.", .{});
            },
            .Instance => {
                const instance = obj.asInstance();
                const name = readString(currentFrame, code);

                var value: Value = undefined;
                if (instance.fields.get(name, &value)) {
                    _ = self.pop(); // Instance.
                    self.push(value);
                } else {
                    try self.bindMethod(instance.class, name);
                }
            },
        }
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runSetProperty(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const maybeObj = self.peek(1);
        if (!maybeObj.isObj()) return self.runtimeError("Only instances have fields.", .{});
        const obj = maybeObj.asObj();
        switch (obj.objType) {
            .String, .Function, .NativeFunction, .Closure, .Upvalue, .Class, .BoundMethod => {
                return self.runtimeError("Only instances have fields.", .{});
            },
            .Instance => {
                const instance = obj.asInstance();
                _ = try instance.fields.set(readString(currentFrame, code), self.peek(0));

                const value = self.pop();
                _ = self.pop();
                self.push(value);
            },
        }
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runGetSuper(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const name = readString(currentFrame, code);
        const superclass = self.pop().asObj().asClass();
        try self.bindMethod(superclass, name);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runCloseUpvalue(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        self.closeUpvalues(&self.stack.items[self.stack.items.len - 2]);
        _ = self.pop();
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runClass(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        self.push((try Obj.Class.create(self, readString(currentFrame, code))).obj.value());
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runInherit(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const maybeObj = self.peek(1);
        if (!maybeObj.isObj()) return self.runtimeError("Superclass must be a class.", .{});
        const obj = maybeObj.asObj();
        if (!obj.isClass()) {
            return self.runtimeError("Superclass must be a class.", .{});
        }
        const superclass = obj.asClass();
        const subclass = self.peek(0).asObj().asClass();

        for (superclass.methods.entries) |entry| {
            if (entry.key) |key| _ = try subclass.methods.set(key, entry.value);
        }

        _ = self.pop(); // Subclass
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runMethod(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        try self.defineMethod(readString(currentFrame, code));
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runPrint(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        try self.outWriter.print("{}\n", .{self.pop()});
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runJump(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const offset = readShort(currentFrame, code);
        currentFrame.ip += offset;
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runJumpIfFalse(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const offset = readShort(currentFrame, code);
        if (self.peek(0).isFalsey()) currentFrame.ip += offset;
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runLoop(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const offset = readShort(currentFrame, code);
        currentFrame.ip -= offset;
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runCall(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const argCount = readByte(currentFrame, code);
        const newFrame = try self.callValue(currentFrame, self.peek(argCount), argCount);
        const newCode = newFrame.closure.function.chunk.code.items;
        try @call(DispatchCallModifier, dispatch, .{ self, newFrame, newCode });
    }

    fn runInvoke(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const method = readString(currentFrame, code);
        const argCount = readByte(currentFrame, code);
        const newFrame = try self.invoke(currentFrame, method, argCount);
        const newCode = newFrame.closure.function.chunk.code.items;
        try @call(DispatchCallModifier, dispatch, .{ self, newFrame, newCode });
    }

    fn runSuperInvoke(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const method = readString(currentFrame, code);
        const argCount = readByte(currentFrame, code);
        const superclass = self.pop().asObj().asClass();
        const newFrame = try self.invokeFromClass(superclass, method, argCount);
        const newCode = newFrame.closure.function.chunk.code.items;
        try @call(DispatchCallModifier, dispatch, .{ self, newFrame, newCode });
    }

    fn runClosure(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const constant = readByte(currentFrame, code);
        const value = currentFrame.closure.function.chunk.constants.items[constant];
        const function = value.asObj().asFunction();
        const closure = try Obj.Closure.create(self, function);
        self.push(closure.obj.value());
        for (closure.upvalues) |*upvalue| {
            const isLocal = readByte(currentFrame, code) != 0;
            const index = readByte(currentFrame, code);
            if (isLocal) {
                // WARNING if the stack is ever resized, this
                // pointer into it becomes invalid. We can avoid
                // this using ensureCapacity when we create the
                // stack, and by making it an error to grow the
                // stack past that. Upvalues needing to be able
                // to point to either the stack or the heap
                // keeps us from growing the stack.
                upvalue.* = try self.captureUpvalue(&self.stack.items[currentFrame.start + index]);
            } else {
                upvalue.* = currentFrame.closure.upvalues[index];
            }
        }
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runConstant(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const constant = readByte(currentFrame, code);
        const value = currentFrame.closure.function.chunk.constants.items[constant];
        self.push(value);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runNil(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        self.push(Value.nil());
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runTrue(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        self.push(Value.fromBool(true));
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runFalse(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        self.push(Value.fromBool(false));
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runEqual(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const b = self.pop();
        const a = self.pop();
        self.push(Value.fromBool(a.equals(b)));
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runGreater(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const rhs = self.pop();
        const lhs = self.pop();
        if (!lhs.isNumber() or !rhs.isNumber()) {
            return self.runtimeError("Operands must be numbers.", .{});
        }
        self.push(Value.fromBool(lhs.asNumber() > rhs.asNumber()));
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runLess(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const rhs = self.pop();
        const lhs = self.pop();
        if (!lhs.isNumber() or !rhs.isNumber()) {
            return self.runtimeError("Operands must be numbers.", .{});
        }
        self.push(Value.fromBool(lhs.asNumber() < rhs.asNumber()));
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runNegate(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const value = self.pop();
        if (!value.isNumber()) return self.runtimeError("Operand must be a number.", .{});
        self.push(Value.fromNumber(-value.asNumber()));
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runAdd(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        const rhs = self.pop();
        const lhs = self.pop();
        if (lhs.isObj() and rhs.isObj()) {
            try self.concatenate(lhs.asObj(), rhs.asObj());
        } else if (lhs.isNumber() and rhs.isNumber()) {
            self.push(Value.fromNumber(lhs.asNumber() + rhs.asNumber()));
        } else {
            return self.runtimeError("Operands must be two numbers or two strings.", .{});
        }
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runSubtract(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        try self.binaryNumericOp(sub);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runMultiply(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        try self.binaryNumericOp(mul);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runDivide(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        try self.binaryNumericOp(div);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn runNot(self: *VM, currentFrame: *CallFrame, code: []u8) RuntimeErrors!void {
        self.push(Value.fromBool(self.pop().isFalsey()));
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code });
    }

    fn binaryNumericOp(self: *VM, comptime op: anytype) !void {
        const rhs = self.pop();
        const lhs = self.pop();
        if (!lhs.isNumber() or !rhs.isNumber()) {
            return self.runtimeError("Operands must be numbers.", .{});
        }
        self.push(Value.fromNumber(op(lhs.asNumber(), rhs.asNumber())));
    }

    fn concatenate(self: *VM, lhs: *Obj, rhs: *Obj) !void {
        switch (lhs.objType) {
            .Function, .NativeFunction, .Closure, .Upvalue, .Class, .Instance, .BoundMethod => {
                return self.runtimeError("Operands must be strings.", .{});
            },
            .String => switch (rhs.objType) {
                .Function, .NativeFunction, .Closure, .Upvalue, .Class, .Instance, .BoundMethod => {
                    return self.runtimeError("Operands must be strings.", .{});
                },
                .String => {
                    // Temporarily put the strings back on the stack so
                    // they're visible to the GC when we allocate
                    self.push(lhs.value());
                    self.push(rhs.value());
                    const lhsStr = lhs.asString();
                    const rhsStr = rhs.asString();
                    const buffer = try self.allocator.alloc(u8, lhsStr.bytes.len + rhsStr.bytes.len);
                    std.mem.copy(u8, buffer[0..lhsStr.bytes.len], lhsStr.bytes);
                    std.mem.copy(u8, buffer[lhsStr.bytes.len..], rhsStr.bytes);
                    _ = self.pop();
                    _ = self.pop();
                    self.push((try Obj.String.create(self, buffer)).obj.value());
                },
            },
        }
    }

    fn readByte(currentFrame: *CallFrame, code: []u8) u8 {
        const byte = code[currentFrame.ip];
        currentFrame.ip += 1;
        return byte;
    }

    fn readShort(currentFrame: *CallFrame, code: []u8) u16 {
        currentFrame.ip += 2;
        return (@intCast(u16, code[currentFrame.ip - 2]) << 8) | code[currentFrame.ip - 1];
    }

    fn readString(currentFrame: *CallFrame, code: []u8) *Obj.String {
        const constant = readByte(currentFrame, code);
        const nameValue = currentFrame.closure.function.chunk.constants.items[constant];
        return nameValue.asObj().asString();
    }

    pub fn push(self: *VM, value: Value) void {
        self.stack.append(value);
    }

    fn peek(self: *VM, back: usize) Value {
        return self.stack.items[self.stack.items.len - 1 - back];
    }

    pub fn pop(self: *VM) Value {
        return self.stack.pop();
    }

    fn call(self: *VM, closure: *Obj.Closure, argCount: usize) !*CallFrame {
        if (argCount != closure.function.arity) {
            const arity = closure.function.arity;
            return self.runtimeError("Expected {} arguments but got {}.", .{ arity, argCount });
        }

        if (self.frames.items.len == FRAMES_MAX) {
            return self.runtimeError("Stack overflow.", .{});
        }

        try self.frames.append(CallFrame{
            .closure = closure,
            .ip = 0,
            // Stack position where this call frame begins
            //
            // NOTE, book uses a pointer into the stack called "slots",
            // but we use an index into the stack instead. Goal was to
            // make stack resizable, but that ended up being tricky
            // because upvalues hold pointers into the stack, and
            // because pushing to the stack is a very hot operation that
            // needs to be as fast as possible.
            .start = self.stack.items.len - argCount - 1,
        });

        return &self.frames.items[self.frames.items.len - 1];
    }

    fn callValue(self: *VM, currentFrame: *CallFrame, callee: Value, argCount: usize) !*CallFrame {
        if (!callee.isObj()) return self.runtimeError("Can only call functions and classes.", .{});

        const obj = callee.asObj();
        switch (obj.objType) {
            .String, .Function, .Upvalue, .Instance => {
                return self.runtimeError("Can only call functions and classes.", .{});
            },
            .Closure => {
                return self.call(obj.asClosure(), argCount);
            },
            .NativeFunction => {
                const args = self.stack.items[self.stack.items.len - 1 - argCount ..];
                try self.stack.resize(self.stack.items.len - 1 - argCount);
                const result = obj.asNativeFunction().function(args);
                self.push(result);
                return currentFrame;
            },
            .BoundMethod => {
                const bound = obj.asBoundMethod();
                self.stack.items[self.stack.items.len - argCount - 1] = bound.receiver;
                return self.call(bound.method, argCount);
            },
            .Class => {
                const class = obj.asClass();
                const instance = (try Obj.Instance.create(self, class)).obj.value();
                self.stack.items[self.stack.items.len - argCount - 1] = instance;
                var initializer: Value = undefined;
                if (class.methods.get(self.initString.?, &initializer)) {
                    return self.call(initializer.asObj().asClosure(), argCount);
                } else if (argCount != 0) {
                    return self.runtimeError("Expected 0 arguments but got {}.", .{argCount});
                } else {
                    return currentFrame;
                }
            },
        }
    }

    fn invoke(self: *VM, currentFrame: *CallFrame, name: *Obj.String, argCount: u8) !*CallFrame {
        const receiver = self.peek(argCount).asObj();

        if (!receiver.isInstance()) {
            return self.runtimeError("Only instances have methods.", .{});
        }

        const instance = receiver.asInstance();

        var value: Value = undefined;
        if (instance.fields.get(name, &value)) {
            self.stack.items[self.stack.items.len - argCount - 1] = value;
            return self.callValue(currentFrame, value, argCount);
        }

        return self.invokeFromClass(instance.class, name, argCount);
    }

    fn invokeFromClass(self: *VM, class: *Obj.Class, name: *Obj.String, argCount: u8) !*CallFrame {
        var method: Value = undefined;
        if (!class.methods.get(name, &method)) {
            return self.runtimeError("Undefined property '{s}'.", .{name.bytes});
        }

        return self.call(method.asObj().asClosure(), argCount);
    }

    fn bindMethod(self: *VM, class: *Obj.Class, name: *Obj.String) !void {
        var method: Value = undefined;
        if (!class.methods.get(name, &method)) {
            return self.runtimeError("Undefined property '{s}'.", .{name.bytes});
        }

        const bound = try Obj.BoundMethod.create(self, self.peek(0), method.asObj().asClosure());
        _ = self.pop();
        self.push(bound.obj.value());
    }

    fn captureUpvalue(self: *VM, local: *Value) !*Obj.Upvalue {
        var prevUpvalue: ?*Obj.Upvalue = null;
        var maybeUpvalue = self.openUpvalues;

        while (maybeUpvalue) |upvalue| {
            if (@ptrToInt(upvalue.location) <= @ptrToInt(local)) break;
            prevUpvalue = upvalue;
            maybeUpvalue = upvalue.next;
        }

        if (maybeUpvalue) |upvalue| {
            if (upvalue.location == local) return upvalue;
        }

        const createdUpvalue = try Obj.Upvalue.create(self, local);
        createdUpvalue.next = maybeUpvalue;

        if (prevUpvalue) |p| {
            p.next = createdUpvalue;
        } else {
            self.openUpvalues = createdUpvalue;
        }

        return createdUpvalue;
    }

    fn closeUpvalues(self: *VM, last: *Value) void {
        while (self.openUpvalues) |openUpvalues| {
            if (@ptrToInt(openUpvalues.location) < @ptrToInt(last)) break;
            const upvalue = openUpvalues;
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed;
            self.openUpvalues = upvalue.next;
        }
    }

    fn defineMethod(self: *VM, name: *Obj.String) !void {
        const method = self.peek(0);
        const class = self.peek(1).asObj().asClass();
        _ = try class.methods.set(name, method);
        _ = self.pop();
    }

    fn resetStack(self: *VM) !void {
        try self.stack.resize(0);
    }

    fn printStack(self: *VM) !void {
        std.debug.print("          ", .{});
        for (self.stack.items) |value| {
            std.debug.print("[ {} ]", .{value});
        }
        std.debug.print("\n", .{});
    }

    fn runtimeError(self: *VM, comptime message: []const u8, args: anytype) RuntimeErrors {
        @setCold(true);

        try self.errWriter.print(message, args);
        try self.errWriter.print("\n", .{});

        while (self.frames.items.len > 0) {
            const frame = self.frames.pop();
            const function = frame.closure.function;
            const line = function.chunk.lines.items[frame.ip - 1];
            const name = if (function.name) |str| str.bytes else "<script>";
            try self.errWriter.print("[line {}] in {s}\n", .{ line, name });
        }

        return error.RuntimeError;
    }

    fn defineNative(self: *VM, name: []const u8, function: Obj.NativeFunction.NativeFunctionType) !void {
        const str = try Obj.String.copy(self, name);
        // NOTE put str on the stack immediately to make sure it doesn't
        // get garbage collected when we allocate to create the native
        // function below.
        self.push(str.obj.value());
        const functionValue = (try Obj.NativeFunction.create(self, function)).obj.value();
        self.push(functionValue);
        _ = try self.globals.set(str, functionValue);
        _ = self.pop();
        _ = self.pop();
    }
};
