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

        const currentFrame = try self.call(closure, 0, 0);
        const code = currentFrame.closure.function.chunk.code.items;
        try self.dispatch(currentFrame, code, currentFrame.ip);
        // Pop the closure we put on the stack above
        _ = self.pop();
    }

    const RuntimeErrors = error{ OutOfMemory, RuntimeError } || std.os.WriteError;

    const InstructionCallModifier = .{ .modifier = .always_tail };

    // Invariant: when we get to dispatch, we need to be at the "tag" of an instruction
    //
    // I think it'll be cleaner to avoid having read functions mutate
    // the instruction pointer
    //
    // Instead, we should decode any data associated with an instruction
    // at the start of the instruction and simultaneously compute the
    // location of the next instruction
    fn dispatch(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        if (debug.TRACE_EXECUTION) {
            // Print debugging information
            try self.printStack();
            _ = currentFrame.closure.function.chunk.disassembleInstruction(ip);
        }

        currentFrame.ip = ip;
        const instruction = readByte(code, ip);
        const opCode = @intToEnum(OpCode, instruction);

        const args = .{ self, currentFrame, code, ip };

        switch (opCode) {
            .Return => try @call(InstructionCallModifier, runReturn, args),
            .Pop => try @call(InstructionCallModifier, runPop, args),
            .GetLocal => try @call(InstructionCallModifier, runGetLocal, args),
            .SetLocal => try @call(InstructionCallModifier, runSetLocal, args),
            .GetGlobal => try @call(InstructionCallModifier, runGetGlobal, args),
            .DefineGlobal => try @call(InstructionCallModifier, runDefineGlobal, args),
            .SetGlobal => try @call(InstructionCallModifier, runSetGlobal, args),
            .GetUpvalue => try @call(InstructionCallModifier, runGetUpvalue, args),
            .SetUpvalue => try @call(InstructionCallModifier, runSetUpvalue, args),
            .GetProperty => try @call(InstructionCallModifier, runGetProperty, args),
            .SetProperty => try @call(InstructionCallModifier, runSetProperty, args),
            .GetSuper => try @call(InstructionCallModifier, runGetSuper, args),
            .CloseUpvalue => try @call(InstructionCallModifier, runCloseUpvalue, args),
            .Class => try @call(InstructionCallModifier, runClass, args),
            .Inherit => try @call(InstructionCallModifier, runInherit, args),
            .Method => try @call(InstructionCallModifier, runMethod, args),
            .Print => try @call(InstructionCallModifier, runPrint, args),
            .Jump => try @call(InstructionCallModifier, runJump, args),
            .JumpIfFalse => try @call(InstructionCallModifier, runJumpIfFalse, args),
            .Loop => try @call(InstructionCallModifier, runLoop, args),
            .Call => try @call(InstructionCallModifier, runCall, args),
            .Invoke => try @call(InstructionCallModifier, runInvoke, args),
            .SuperInvoke => try @call(InstructionCallModifier, runSuperInvoke, args),
            .Closure => try @call(InstructionCallModifier, runClosure, args),
            .Constant => try @call(InstructionCallModifier, runConstant, args),
            .Nil => try @call(InstructionCallModifier, runNil, args),
            .True => try @call(InstructionCallModifier, runTrue, args),
            .False => try @call(InstructionCallModifier, runFalse, args),
            .Equal => try @call(InstructionCallModifier, runEqual, args),
            .Greater => try @call(InstructionCallModifier, runGreater, args),
            .Less => try @call(InstructionCallModifier, runLess, args),
            .Negate => try @call(InstructionCallModifier, runNegate, args),
            .Add => try @call(InstructionCallModifier, runAdd, args),
            .Subtract => try @call(InstructionCallModifier, runSubtract, args),
            .Multiply => try @call(InstructionCallModifier, runMultiply, args),
            .Divide => try @call(InstructionCallModifier, runDivide, args),
            .Not => try @call(InstructionCallModifier, runNot, args),
        }
    }

    const DispatchCallModifier = .{ .modifier = .always_tail };

    fn runReturn(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        _ = ip;
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
        try @call(DispatchCallModifier, dispatch, .{ self, newFrame, newCode, newFrame.ip });
    }

    fn runPop(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 1;
        _ = self.pop();
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runGetLocal(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 2;
        const slot = readByte(code, ip + 1);
        self.push(self.stack.items[currentFrame.start + slot]);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runSetLocal(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 2;
        const slot = readByte(code, ip + 1);
        self.stack.items[currentFrame.start + slot] = self.peek(0);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runGetGlobal(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 2;
        const name = readStringFrame(currentFrame, code, ip + 1);
        var value: Value = undefined;
        if (!self.globals.get(name, &value)) {
            return self.runtimeError("Undefined variable '{s}'.", .{name.bytes}, ip);
        }
        self.push(value);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runDefineGlobal(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 2;
        _ = try self.globals.set(readStringFrame(currentFrame, code, ip + 1), self.peek(0));
        // NOTE don’t pop until value is in the hash table so
        // that we don't lose the value if the GC runs during
        // the set operation
        _ = self.pop();
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runSetGlobal(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 2;
        const name = readStringFrame(currentFrame, code, ip + 1);
        if (try self.globals.set(name, self.peek(0))) {
            _ = self.globals.delete(name);
            return self.runtimeError("Undefined variable '{s}'.", .{name.bytes}, ip);
        }
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runGetUpvalue(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 2;
        const slot = readByte(code, ip + 1);
        // Upvalues are guaranteed to be filled in by the time we get here
        self.push(currentFrame.closure.upvalues[slot].?.location.*);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runSetUpvalue(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 2;
        const slot = readByte(code, ip + 1);
        // Upvalues are guaranteed to be filled in by the time we get here
        currentFrame.closure.upvalues[slot].?.location.* = self.peek(0);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runGetProperty(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 2;
        const maybeObj = self.peek(0);
        if (!maybeObj.isObj()) return self.runtimeError("Only instances have properties.", .{}, ip);
        const obj = maybeObj.asObj();
        switch (obj.objType) {
            .String, .Function, .NativeFunction, .Closure, .Upvalue, .Class, .BoundMethod => {
                return self.runtimeError("Only instances have properties.", .{}, ip);
            },
            .Instance => {
                const instance = obj.asInstance();
                const name = readStringFrame(currentFrame, code, ip + 1);

                var value: Value = undefined;
                if (instance.fields.get(name, &value)) {
                    _ = self.pop(); // Instance.
                    self.push(value);
                } else {
                    try self.bindMethod(instance.class, name, ip);
                }
            },
        }
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runSetProperty(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 2;
        const maybeObj = self.peek(1);
        if (!maybeObj.isObj()) return self.runtimeError("Only instances have fields.", .{}, ip);
        const obj = maybeObj.asObj();
        switch (obj.objType) {
            .String, .Function, .NativeFunction, .Closure, .Upvalue, .Class, .BoundMethod => {
                return self.runtimeError("Only instances have fields.", .{}, ip);
            },
            .Instance => {
                const instance = obj.asInstance();
                _ = try instance.fields.set(readStringFrame(currentFrame, code, ip + 1), self.peek(0));

                const value = self.pop();
                _ = self.pop();
                self.push(value);
            },
        }
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runGetSuper(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 2;
        const name = readStringFrame(currentFrame, code, ip + 1);
        const superclass = self.pop().asObj().asClass();
        try self.bindMethod(superclass, name, ip);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runCloseUpvalue(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 1;
        self.closeUpvalues(&self.stack.items[self.stack.items.len - 2]);
        _ = self.pop();
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runClass(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 2;
        self.push((try Obj.Class.create(self, readStringFrame(currentFrame, code, ip + 1))).obj.value());
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runInherit(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 1;
        const maybeObj = self.peek(1);
        if (!maybeObj.isObj()) return self.runtimeError("Superclass must be a class.", .{}, ip);
        const obj = maybeObj.asObj();
        if (!obj.isClass()) {
            return self.runtimeError("Superclass must be a class.", .{}, ip);
        }
        const superclass = obj.asClass();
        const subclass = self.peek(0).asObj().asClass();

        for (superclass.methods.entries) |entry| {
            if (entry.key) |key| _ = try subclass.methods.set(key, entry.value);
        }

        _ = self.pop(); // Subclass
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runMethod(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 2;
        try self.defineMethod(readStringFrame(currentFrame, code, ip + 1));
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runPrint(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 1;
        try self.outWriter.print("{}\n", .{self.pop()});
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runJump(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 3;
        const offset = readShort(code, ip + 1);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes + offset });
    }

    fn runJumpIfFalse(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 3;
        const offset = if (self.peek(0).isFalsey()) readShort(code, ip + 1) else 0;
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes + offset });
    }

    fn runLoop(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 3;
        const offset = readShort(code, ip + 1);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes - offset });
    }

    fn runCall(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 2;
        const argCount = readByte(code, ip + 1);
        const newFrame = try self.callValue(currentFrame, self.peek(argCount), argCount, ip + instructionBytes);
        const newCode = newFrame.closure.function.chunk.code.items;
        try @call(DispatchCallModifier, dispatch, .{ self, newFrame, newCode, newFrame.ip });
    }

    fn runInvoke(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 3;
        const method = readStringFrame(currentFrame, code, ip + 1);
        const argCount = readByte(code, ip + 2);
        const newFrame = try self.invoke(currentFrame, method, argCount, ip + instructionBytes);
        const newCode = newFrame.closure.function.chunk.code.items;
        try @call(DispatchCallModifier, dispatch, .{ self, newFrame, newCode, newFrame.ip });
    }

    fn runSuperInvoke(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 3;
        const method = readStringFrame(currentFrame, code, ip + 1);
        const argCount = readByte(code, ip + 2);
        const superclass = self.pop().asObj().asClass();
        const newFrame = try self.invokeFromClass(superclass, method, argCount, ip + instructionBytes);
        const newCode = newFrame.closure.function.chunk.code.items;
        try @call(DispatchCallModifier, dispatch, .{ self, newFrame, newCode, newFrame.ip });
    }

    fn runClosure(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        var instructionBytes: usize = 2; // note, modified in loop below
        const constant = readByte(code, ip + 1);
        const value = currentFrame.closure.function.chunk.constants.items[constant];
        const function = value.asObj().asFunction();
        const closure = try Obj.Closure.create(self, function);
        self.push(closure.obj.value());
        for (closure.upvalues) |*upvalue| {
            const isLocal = readByte(code, ip + instructionBytes) != 0;
            instructionBytes += 1;
            const index = readByte(code, ip + instructionBytes);
            instructionBytes += 1;
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
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runConstant(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 2;
        const constant = readByte(code, ip + 1);
        const value = currentFrame.closure.function.chunk.constants.items[constant];
        self.push(value);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runNil(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 1;
        self.push(Value.nil());
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runTrue(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 1;
        self.push(Value.fromBool(true));
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runFalse(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 1;
        self.push(Value.fromBool(false));
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runEqual(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 1;
        const b = self.pop();
        const a = self.pop();
        self.push(Value.fromBool(a.equals(b)));
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runGreater(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 1;
        const rhs = self.pop();
        const lhs = self.pop();
        if (!lhs.isNumber() or !rhs.isNumber()) {
            return self.runtimeError("Operands must be numbers.", .{}, ip);
        }
        self.push(Value.fromBool(lhs.asNumber() > rhs.asNumber()));
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runLess(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 1;
        const rhs = self.pop();
        const lhs = self.pop();
        if (!lhs.isNumber() or !rhs.isNumber()) {
            return self.runtimeError("Operands must be numbers.", .{}, ip);
        }
        self.push(Value.fromBool(lhs.asNumber() < rhs.asNumber()));
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runNegate(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 1;
        const value = self.pop();
        if (!value.isNumber()) return self.runtimeError("Operand must be a number.", .{}, ip);
        self.push(Value.fromNumber(-value.asNumber()));
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runAdd(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 1;
        const rhs = self.pop();
        const lhs = self.pop();
        if (lhs.isObj() and rhs.isObj()) {
            try self.concatenate(lhs.asObj(), rhs.asObj(), ip);
        } else if (lhs.isNumber() and rhs.isNumber()) {
            self.push(Value.fromNumber(lhs.asNumber() + rhs.asNumber()));
        } else {
            return self.runtimeError("Operands must be two numbers or two strings.", .{}, ip);
        }
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runSubtract(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 1;
        try self.binaryNumericOp(sub, ip);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runMultiply(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 1;
        try self.binaryNumericOp(mul, ip);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runDivide(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 1;
        try self.binaryNumericOp(div, ip);
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn runNot(self: *VM, currentFrame: *CallFrame, code: []u8, ip: usize) RuntimeErrors!void {
        const instructionBytes = 1;
        self.push(Value.fromBool(self.pop().isFalsey()));
        try @call(DispatchCallModifier, dispatch, .{ self, currentFrame, code, ip + instructionBytes });
    }

    fn binaryNumericOp(self: *VM, comptime op: anytype, ip: usize) !void {
        const rhs = self.pop();
        const lhs = self.pop();
        if (!lhs.isNumber() or !rhs.isNumber()) {
            return self.runtimeError("Operands must be numbers.", .{}, ip);
        }
        self.push(Value.fromNumber(op(lhs.asNumber(), rhs.asNumber())));
    }

    fn concatenate(self: *VM, lhs: *Obj, rhs: *Obj, ip: usize) !void {
        switch (lhs.objType) {
            .Function, .NativeFunction, .Closure, .Upvalue, .Class, .Instance, .BoundMethod => {
                return self.runtimeError("Operands must be strings.", .{}, ip);
            },
            .String => switch (rhs.objType) {
                .Function, .NativeFunction, .Closure, .Upvalue, .Class, .Instance, .BoundMethod => {
                    return self.runtimeError("Operands must be strings.", .{}, ip);
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

    // NOTE book version of readByte, readShort, and readString mutate
    // ip, but in this implementation, managing ip is handled inline in
    // the run* Instruction handlers.
    fn readByte(code: []u8, ip: usize) u8 {
        return code[ip];
    }

    fn readShort(code: []u8, ip: usize) u16 {
        return (@intCast(u16, code[ip]) << 8) | code[ip + 1];
    }

    fn readString(code: []u8, constants: []Value, ip: usize) *Obj.String {
        const constant = readByte(code, ip);
        const nameValue = constants[constant];
        return nameValue.asObj().asString();
    }

    fn readStringFrame(currentFrame: *CallFrame, code: []u8, ip: usize) *Obj.String {
        const constants = currentFrame.closure.function.chunk.constants.items;
        return readString(code, constants, ip);
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

    fn call(self: *VM, closure: *Obj.Closure, argCount: usize, ip: usize) !*CallFrame {
        if (argCount != closure.function.arity) {
            const arity = closure.function.arity;
            return self.runtimeError("Expected {} arguments but got {}.", .{ arity, argCount }, ip);
        }

        if (self.frames.items.len == FRAMES_MAX) {
            return self.runtimeError("Stack overflow.", .{}, ip);
        }

        if (self.frames.items.len > 0) {
            self.frames.items[self.frames.items.len - 1].ip = ip;
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

    fn callValue(self: *VM, currentFrame: *CallFrame, callee: Value, argCount: usize, ip: usize) !*CallFrame {
        if (!callee.isObj()) return self.runtimeError("Can only call functions and classes.", .{}, ip);

        const obj = callee.asObj();
        switch (obj.objType) {
            .String, .Function, .Upvalue, .Instance => {
                return self.runtimeError("Can only call functions and classes.", .{}, ip);
            },
            .Closure => {
                return self.call(obj.asClosure(), argCount, ip);
            },
            .NativeFunction => {
                const args = self.stack.items[self.stack.items.len - 1 - argCount ..];
                try self.stack.resize(self.stack.items.len - 1 - argCount);
                const result = obj.asNativeFunction().function(args);
                self.push(result);
                currentFrame.ip = ip;
                return currentFrame;
            },
            .BoundMethod => {
                const bound = obj.asBoundMethod();
                self.stack.items[self.stack.items.len - argCount - 1] = bound.receiver;
                return self.call(bound.method, argCount, ip);
            },
            .Class => {
                const class = obj.asClass();
                const instance = (try Obj.Instance.create(self, class)).obj.value();
                self.stack.items[self.stack.items.len - argCount - 1] = instance;
                var initializer: Value = undefined;
                if (class.methods.get(self.initString.?, &initializer)) {
                    return self.call(initializer.asObj().asClosure(), argCount, ip);
                } else if (argCount != 0) {
                    return self.runtimeError("Expected 0 arguments but got {}.", .{argCount}, ip);
                } else {
                    currentFrame.ip = ip;
                    return currentFrame;
                }
            },
        }
    }

    fn invoke(self: *VM, currentFrame: *CallFrame, name: *Obj.String, argCount: u8, ip: usize) !*CallFrame {
        const receiver = self.peek(argCount).asObj();

        if (!receiver.isInstance()) {
            return self.runtimeError("Only instances have methods.", .{}, ip);
        }

        const instance = receiver.asInstance();

        var value: Value = undefined;
        if (instance.fields.get(name, &value)) {
            self.stack.items[self.stack.items.len - argCount - 1] = value;
            return self.callValue(currentFrame, value, argCount, ip);
        }

        return self.invokeFromClass(instance.class, name, argCount, ip);
    }

    fn invokeFromClass(self: *VM, class: *Obj.Class, name: *Obj.String, argCount: u8, ip: usize) !*CallFrame {
        var method: Value = undefined;
        if (!class.methods.get(name, &method)) {
            return self.runtimeError("Undefined property '{s}'.", .{name.bytes}, ip);
        }

        return self.call(method.asObj().asClosure(), argCount, ip);
    }

    fn bindMethod(self: *VM, class: *Obj.Class, name: *Obj.String, ip: usize) !void {
        var method: Value = undefined;
        if (!class.methods.get(name, &method)) {
            return self.runtimeError("Undefined property '{s}'.", .{name.bytes}, ip);
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

    fn runtimeError(self: *VM, comptime message: []const u8, args: anytype, ip: usize) RuntimeErrors {
        @setCold(true);

        // Synchronize ip of current frame
        if (self.frames.items.len > 0) {
            self.frames.items[self.frames.items.len - 1].ip = ip;
        }

        try self.errWriter.print(message, args);
        try self.errWriter.print("\n", .{});

        while (self.frames.items.len > 0) {
            const frame = self.frames.pop();
            const function = frame.closure.function;
            const line = function.chunk.lines.items[frame.ip];
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
