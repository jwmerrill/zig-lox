const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const printValue = @import("./value.zig").printValue;
const compile = @import("./compiler.zig").compile;
const Parser = @import("./compiler.zig").Parser;
const debug = @import("./debug.zig");
const Obj = @import("./object.zig").Obj;
const Table = @import("./table.zig").Table;
const GCAllocator = @import("./memory.zig").GCAllocator;

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

pub const CallFrame = struct {
    closure: *Obj.Closure,
    ip: usize,
    start: usize,
};

pub fn clockNative(args: []const Value) Value {
    return Value{ .Number = @intToFloat(f64, std.time.milliTimestamp()) / 1000 };
}

const STACK_MAX = 2048;
const FRAMES_MAX = 64;

pub const VM = struct {
    gcAllocatorInstance: GCAllocator,
    allocator: *Allocator,
    frames: ArrayList(CallFrame), // NOTE, book uses a fixed size stack
    stack: ArrayList(Value), // NOTE, book uses a fixed size stack
    objects: ?*Obj,
    openUpvalues: ?*Obj.Upvalue,
    strings: Table,
    initString: ?*Obj.String,
    globals: Table,
    parser: ?*Parser,
    grayStack: ArrayList(*Obj),

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
            .grayStack = undefined,
        };

        return vm;
    }

    pub fn init(self: *VM, backingAllocator: *Allocator) !void {
        self.gcAllocatorInstance = GCAllocator.init(self, backingAllocator);
        const allocator = &self.gcAllocatorInstance.allocator;

        // Note, we can tell none of this allocates because none of
        // these operations can fail, and allocation can always fail
        // with error.OutOfMemory
        self.allocator = allocator;
        self.frames = std.ArrayList(CallFrame).init(allocator);
        self.stack = std.ArrayList(Value).init(allocator);
        self.strings = Table.init(allocator);
        self.initString = try Obj.String.copy(self, "init");
        self.globals = Table.init(allocator);
        // Note, purposely uses the backing allocator to avoid having
        // growing the grayStack during GC kick off more GC.
        self.grayStack = std.ArrayList(*Obj).init(backingAllocator);

        // We need to make sure the stack doesn't actually grow
        // dynamically so that upvalue pointers into the stack
        // do not get invalidated
        try self.stack.ensureCapacity(STACK_MAX);

        try self.defineNative("clock", clockNative);
    }

    pub fn deinit(self: *VM) void {
        self.initString = null;
        self.freeObjects();
        self.strings.deinit();
        self.globals.deinit();
        self.stack.deinit();
        self.grayStack.deinit();
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

        errdefer self.resetStack();
        const function = try compile(self, source);
        try self.push(function.obj.value());

        // NOTE need the function on the stack when we allocate the
        // closure so that the GC can see it.
        const closure = try Obj.Closure.create(self, function);
        _ = self.pop();
        try self.push(closure.obj.value());

        try self.callValue(closure.obj.value(), 0);
        try self.run();
        // Pop the closure we put on the stack above
        _ = self.pop();
    }

    fn run(self: *VM) !void {
        while (true) {
            if (debug.TRACE_EXECUTION) {
                // Print debugging information
                try self.printStack();
                _ = self.currentChunk().disassembleInstruction(self.currentFrame().ip);
            }

            const instruction = self.readByte();
            const opCode = @intToEnum(OpCode, instruction);
            try self.runOp(opCode);
            if (opCode == .Return and self.frames.items.len == 0) break;
        }
    }

    fn readString(self: *VM) *Obj.String {
        const constant = self.readByte();
        const nameValue = self.currentChunk().constants.items[constant];
        return nameValue.Obj.asString();
    }

    fn runOp(self: *VM, opCode: OpCode) !void {
        switch (opCode) {
            .Return => {
                const result = self.pop();
                const frame = self.frames.pop();

                self.closeUpvalues(&self.stack.items[frame.start]);

                if (self.frames.items.len == 0) return;

                try self.stack.resize(frame.start);
                try self.push(result);
            },
            .Pop => {
                _ = self.pop();
            },
            .GetLocal => {
                const slot = self.readByte();
                try self.push(self.stack.items[self.currentFrame().start + slot]);
            },
            .SetLocal => {
                const slot = self.readByte();
                self.stack.items[self.currentFrame().start + slot] = self.peek(0);
            },
            .GetGlobal => {
                const name = self.readString();
                var value: Value = undefined;
                if (!self.globals.get(name, &value)) {
                    return self.runtimeError("Undefined variable '{}'.", .{name.bytes});
                }
                try self.push(value);
            },
            .DefineGlobal => {
                _ = try self.globals.set(self.readString(), self.peek(0));
                // NOTE don’t pop until value is in the hash table so
                // that we don't lose the value if the GC runs during
                // the set operation
                _ = self.pop();
            },
            .SetGlobal => {
                const name = self.readString();
                if (try self.globals.set(name, self.peek(0))) {
                    _ = self.globals.delete(name);
                    return self.runtimeError("Undefined variable '{}'.", .{name.bytes});
                }
            },
            .GetUpvalue => {
                const slot = self.readByte();
                // Upvalues are guaranteed to be filled in by the time we get here
                try self.push(self.currentFrame().closure.upvalues[slot].?.location.*);
            },
            .SetUpvalue => {
                const slot = self.readByte();
                // Upvalues are guaranteed to be filled in by the time we get here
                self.currentFrame().closure.upvalues[slot].?.location.* = self.peek(0);
            },
            .GetProperty => {
                const maybeObj = self.peek(0);

                switch (maybeObj) {
                    .Number, .Bool, .Nil => {
                        return self.runtimeError("Only instances have properties.", .{});
                    },
                    .Obj => |obj| {
                        switch (obj.objType) {
                            .String, .Function, .NativeFunction, .Closure, .Upvalue, .Class, .BoundMethod => {
                                return self.runtimeError("Only instances have properties.", .{});
                            },
                            .Instance => {
                                const instance = obj.asInstance();
                                const name = self.readString();

                                var value: Value = undefined;
                                if (instance.fields.get(name, &value)) {
                                    _ = self.pop(); // Instance.
                                    try self.push(value);
                                } else {
                                    try self.bindMethod(instance.class, name);
                                }
                            },
                        }
                    },
                }
            },
            .SetProperty => {
                const maybeObj = self.peek(1);

                switch (maybeObj) {
                    .Number, .Bool, .Nil => {
                        return self.runtimeError("Only instances have fields.", .{});
                    },
                    .Obj => |obj| {
                        switch (obj.objType) {
                            .String, .Function, .NativeFunction, .Closure, .Upvalue, .Class, .BoundMethod => {
                                return self.runtimeError("Only instances have fields.", .{});
                            },
                            .Instance => {
                                const instance = obj.asInstance();
                                _ = try instance.fields.set(self.readString(), self.peek(0));

                                const value = self.pop();
                                _ = self.pop();
                                try self.push(value);
                            },
                        }
                    },
                }
            },
            .GetSuper => {
                const name = self.readString();
                const superclass = self.pop().Obj.asClass();
                try self.bindMethod(superclass, name);
            },
            .CloseUpvalue => {
                self.closeUpvalues(&self.stack.items[self.stack.items.len - 2]);
                _ = self.pop();
            },
            .Class => {
                try self.push((try Obj.Class.create(self, self.readString())).obj.value());
            },
            .Inherit => {
                const value = self.peek(1);
                switch (value) {
                    .Number, .Bool, .Nil => return self.runtimeError("Superclass must be a class.", .{}),
                    .Obj => |obj| {
                        if (!obj.isClass()) {
                            return self.runtimeError("Superclass must be a class.", .{});
                        }
                        const superclass = obj.asClass();
                        const subclass = self.peek(0).Obj.asClass();

                        for (superclass.methods.entries) |entry| {
                            if (entry.key) |key| _ = try subclass.methods.set(key, entry.value);
                        }

                        _ = self.pop(); // Subclass
                    },
                }
            },
            .Method => {
                try self.defineMethod(self.readString());
            },
            .Print => {
                const stdout = std.io.getStdOut().outStream();
                try printValue(self.pop());
                try stdout.print("\n", .{});
            },
            .Jump => {
                const offset = self.readShort();
                self.currentFrame().ip += offset;
            },
            .JumpIfFalse => {
                const offset = self.readShort();
                if (self.peek(0).isFalsey()) self.currentFrame().ip += offset;
            },
            .Loop => {
                const offset = self.readShort();
                self.currentFrame().ip -= offset;
            },
            .Call => {
                const argCount = self.readByte();
                try self.callValue(self.peek(argCount), argCount);
            },
            .Invoke => {
                const method = self.readString();
                const argCount = self.readByte();
                try self.invoke(method, argCount);
            },
            .SuperInvoke => {
                const method = self.readString();
                const argCount = self.readByte();
                const superclass = self.pop().Obj.asClass();
                try self.invokeFromClass(superclass, method, argCount);
            },
            .Closure => {
                const constant = self.readByte();
                const value = self.currentChunk().constants.items[constant];
                const function = value.Obj.asFunction();
                const closure = try Obj.Closure.create(self, function);
                try self.push(closure.obj.value());
                for (closure.upvalues) |*upvalue| {
                    const isLocal = self.readByte() != 0;
                    const index = self.readByte();
                    if (isLocal) {
                        // WARNING if the stack is ever resized, this
                        // pointer into it becomes invalid. We can avoid
                        // this using ensureCapacity when we create the
                        // stack, and by making it an error to grow the
                        // stack past that. Upvalues needing to be able
                        // to point to either the stack or the heap
                        // keeps us from growing the stack.
                        upvalue.* = try self.captureUpvalue(&self.stack.items[self.currentFrame().start + index]);
                    } else {
                        upvalue.* = self.currentFrame().closure.upvalues[index];
                    }
                }
            },
            .Constant => {
                const constant = self.readByte();
                const value = self.currentChunk().constants.items[constant];
                try self.push(value);
            },
            .Nil => try self.push(Value.Nil),
            .True => try self.push(Value{ .Bool = true }),
            .False => try self.push(Value{ .Bool = false }),
            .Equal => {
                const b = self.pop();
                const a = self.pop();
                try self.push(Value{ .Bool = a.equals(b) });
            },
            .Greater => {
                const rhsBoxed = self.pop();
                const lhsBoxed = self.pop();
                switch (lhsBoxed) {
                    .Bool, .Nil, .Obj => return self.runtimeError("Operands must be numbers.", .{}),
                    .Number => |lhs| switch (rhsBoxed) {
                        .Bool, .Nil, .Obj => return self.runtimeError("Operands must be numbers.", .{}),
                        .Number => |rhs| try self.push(Value{ .Bool = lhs > rhs }),
                    },
                }
            },
            .Less => {
                const rhsBoxed = self.pop();
                const lhsBoxed = self.pop();
                switch (lhsBoxed) {
                    .Bool, .Nil, .Obj => return self.runtimeError("Operands must be numbers.", .{}),
                    .Number => |lhs| switch (rhsBoxed) {
                        .Bool, .Nil, .Obj => return self.runtimeError("Operands must be numbers.", .{}),
                        .Number => |rhs| try self.push(Value{ .Bool = lhs < rhs }),
                    },
                }
            },
            .Negate => {
                const boxed = self.pop();
                switch (boxed) {
                    .Bool, .Nil, .Obj => return self.runtimeError("Operand must be a number.", .{}),
                    .Number => |value| try self.push(Value{ .Number = -value }),
                }
            },
            .Add => {
                const rhsBoxed = self.pop();
                const lhsBoxed = self.pop();
                switch (lhsBoxed) {
                    .Bool, .Nil => return self.runtimeError("Operands must be two numbers or two strings.", .{}),
                    .Obj => |lhs| switch (rhsBoxed) {
                        .Bool, .Nil, .Number => return self.runtimeError("Operands must be two numbers or two strings.", .{}),
                        .Obj => |rhs| try self.concatenate(lhs, rhs),
                    },
                    .Number => |lhs| switch (rhsBoxed) {
                        .Bool, .Nil, .Obj => return self.runtimeError("Operands must be two numbers or two strings.", .{}),
                        .Number => |rhs| try self.push(Value{ .Number = lhs + rhs }),
                    },
                }
            },
            .Subtract => try self.binaryNumericOp(sub),
            .Multiply => try self.binaryNumericOp(mul),
            .Divide => try self.binaryNumericOp(div),
            .Not => try self.push(Value{ .Bool = self.pop().isFalsey() }),
        }
    }

    fn binaryNumericOp(self: *VM, comptime op: var) !void {
        const rhsBoxed = self.pop();
        const lhsBoxed = self.pop();
        switch (lhsBoxed) {
            .Bool, .Nil, .Obj => return self.runtimeError("Operands must be numbers.", .{}),
            .Number => |lhs| switch (rhsBoxed) {
                .Bool, .Nil, .Obj => return self.runtimeError("Operands must be numbers.", .{}),
                .Number => |rhs| try self.push(Value{ .Number = op(lhs, rhs) }),
            },
        }
    }

    fn concatenate(self: *VM, lhs: *Obj, rhs: *Obj) !void {
        switch (lhs.objType) {
            .Function, .NativeFunction, .Closure, .Upvalue, .Class, .Instance, .BoundMethod => {
                try self.runtimeError("Operands must be strings.", .{});
            },
            .String => switch (rhs.objType) {
                .Function, .NativeFunction, .Closure, .Upvalue, .Class, .Instance, .BoundMethod => {
                    try self.runtimeError("Operands must be strings.", .{});
                },
                .String => {
                    // Temporarily put the strings back on the stack so
                    // they're visible to the GC when we allocate
                    try self.push(lhs.value());
                    try self.push(rhs.value());
                    const lhsStr = lhs.asString();
                    const rhsStr = rhs.asString();
                    const buffer = try self.allocator.alloc(u8, lhsStr.bytes.len + rhsStr.bytes.len);
                    std.mem.copy(u8, buffer[0..lhsStr.bytes.len], lhsStr.bytes);
                    std.mem.copy(u8, buffer[lhsStr.bytes.len..], rhsStr.bytes);
                    _ = self.pop();
                    _ = self.pop();
                    try self.push((try Obj.String.create(self, buffer)).obj.value());
                },
            },
        }
    }

    fn currentFrame(self: *VM) *CallFrame {
        return &self.frames.items[self.frames.items.len - 1];
    }

    fn currentChunk(self: *VM) *Chunk {
        return &self.currentFrame().closure.function.chunk;
    }

    fn readByte(self: *VM) u8 {
        const frame = self.currentFrame();
        const byte = self.currentChunk().code.items[frame.ip];
        frame.ip += 1;
        return byte;
    }

    fn readShort(self: *VM) u16 {
        const frame = self.currentFrame();
        frame.ip += 2;
        const items = self.currentChunk().code.items;
        return (@intCast(u16, items[frame.ip - 2]) << 8) | items[frame.ip - 1];
    }

    fn push(self: *VM, value: Value) !void {
        if (self.stack.items.len >= STACK_MAX) {
            // Cast this to an OutOfMemory error instead of a
            // runtime error because the compiler pushes things onto the
            // stack for GC visibility but doesn't expect to deal with
            // RuntimeError.
            return self.runtimeError("Stack overflow.", .{}) catch error.OutOfMemory;
        }
        try self.stack.append(value);
    }

    fn peek(self: *VM, back: usize) Value {
        return self.stack.items[self.stack.items.len - 1 - back];
    }

    fn pop(self: *VM) Value {
        return self.stack.pop();
    }

    fn call(self: *VM, closure: *Obj.Closure, argCount: usize) !void {
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
            // Note, book uses a pointer into the stack called "slots",
            // but since our stack is resizable, use an index into the
            // stack instead.
            .start = self.stack.items.len - argCount - 1,
        });
    }

    fn callValue(self: *VM, callee: Value, argCount: usize) !void {
        switch (callee) {
            .Bool, .Nil, .Number => {
                return self.runtimeError("Can only call functions and classes.", .{});
            },
            .Obj => |obj| {
                switch (obj.objType) {
                    .String, .Function, .Upvalue, .Instance => {
                        return self.runtimeError("Can only call functions and classes.", .{});
                    },
                    .Closure => try self.call(obj.asClosure(), argCount),
                    .NativeFunction => {
                        const args = self.stack.items[self.stack.items.len - 1 - argCount ..];
                        try self.stack.resize(self.stack.items.len - 1 - argCount);
                        const result = obj.asNativeFunction().function(args);
                        try self.push(result);
                    },
                    .BoundMethod => {
                        const bound = obj.asBoundMethod();
                        self.stack.items[self.stack.items.len - argCount - 1] = bound.receiver;
                        try self.call(bound.method, argCount);
                    },
                    .Class => {
                        const class = obj.asClass();
                        const instance = (try Obj.Instance.create(self, class)).obj.value();
                        self.stack.items[self.stack.items.len - argCount - 1] = instance;
                        var initializer: Value = undefined;
                        if (class.methods.get(self.initString.?, &initializer)) {
                            try self.call(initializer.Obj.asClosure(), argCount);
                        } else if (argCount != 0) {
                            return self.runtimeError("Expected 0 arguments but got {}.", .{argCount});
                        }
                    },
                }
            },
        }
    }

    fn invoke(self: *VM, name: *Obj.String, argCount: u8) !void {
        const receiver = self.peek(argCount).Obj;

        if (!receiver.isInstance()) {
            return self.runtimeError("Only instances have methods.", .{});
        }

        const instance = receiver.asInstance();

        var value: Value = undefined;
        if (instance.fields.get(name, &value)) {
            self.stack.items[self.stack.items.len - argCount - 1] = value;
            return self.callValue(value, argCount);
        }

        return self.invokeFromClass(instance.class, name, argCount);
    }

    fn invokeFromClass(self: *VM, class: *Obj.Class, name: *Obj.String, argCount: u8) !void {
        var method: Value = undefined;
        if (!class.methods.get(name, &method)) {
            return self.runtimeError("Undefined property '{}'.", .{name.bytes});
        }

        return self.call(method.Obj.asClosure(), argCount);
    }

    fn bindMethod(self: *VM, class: *Obj.Class, name: *Obj.String) !void {
        var method: Value = undefined;
        if (!class.methods.get(name, &method)) {
            return self.runtimeError("Undefined property '{}'.", .{name.bytes});
        }

        const bound = try Obj.BoundMethod.create(self, self.peek(0), method.Obj.asClosure());
        _ = self.pop();
        try self.push(bound.obj.value());
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
        const class = self.peek(1).Obj.asClass();
        _ = try class.methods.set(name, method);
        _ = self.pop();
    }

    fn resetStack(self: *VM) void {
        self.stack.shrink(0);
    }

    fn printStack(self: *VM) !void {
        std.debug.warn("          ", .{});
        for (self.stack.items) |value| {
            std.debug.warn("[ ", .{});
            try printValue(value);
            std.debug.warn(" ]", .{});
        }
        std.debug.warn("\n", .{});
    }

    fn runtimeError(self: *VM, comptime message: []const u8, args: var) !void {
        @setCold(true);

        std.debug.warn(message, args);
        std.debug.warn("\n", .{});

        while (self.frames.items.len > 0) {
            const frame = self.frames.pop();
            const function = frame.closure.function;
            const line = function.chunk.lines.items[frame.ip - 1];
            const name = if (function.name) |str| str.bytes else "<script>";
            std.debug.warn("[line {}] in {}\n", .{ line, name });
        }

        return error.RuntimeError;
    }

    fn defineNative(self: *VM, name: []const u8, function: Obj.NativeFunction.NativeFunctionType) !void {
        const str = try Obj.String.copy(self, name);
        // NOTE put str on the stack immediately to make sure it doesn't
        // get garbage collected when we allocate to create the native
        // function below.
        try self.push(str.obj.value());
        const functionValue = (try Obj.NativeFunction.create(self, function)).obj.value();
        try self.push(functionValue);
        _ = try self.globals.set(str, functionValue);
        _ = self.pop();
        _ = self.pop();
    }
};
