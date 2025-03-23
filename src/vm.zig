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
        return VM{
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
            const opCode = @as(OpCode, @enumFromInt(instruction));
            try self.runOp(opCode);
            if (opCode == .Return and self.frames.items.len == 0) break;
        }
    }

    fn readString(self: *VM) *Obj.String {
        const constant = self.readByte();
        const nameValue = self.currentChunk().constants.items[constant];
        return nameValue.asObj().asString();
    }

    fn runOp(self: *VM, opCode: OpCode) !void {
        switch (opCode) {
            .Return => {
                const result = self.pop();
                const frame = self.frames.pop() orelse unreachable;

                self.closeUpvalues(&self.stack.items[frame.start]);

                if (self.frames.items.len == 0) return;

                try self.stack.resize(frame.start);
                self.push(result);
            },
            .Pop => {
                _ = self.pop();
            },
            .GetLocal => {
                const slot = self.readByte();
                self.push(self.stack.items[self.currentFrame().start + slot]);
            },
            .SetLocal => {
                const slot = self.readByte();
                self.stack.items[self.currentFrame().start + slot] = self.peek(0);
            },
            .GetGlobal => {
                const name = self.readString();
                var value: Value = undefined;
                if (!self.globals.get(name, &value)) {
                    return self.runtimeError("Undefined variable '{s}'.", .{name.bytes});
                }
                self.push(value);
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
                    return self.runtimeError("Undefined variable '{s}'.", .{name.bytes});
                }
            },
            .GetUpvalue => {
                const slot = self.readByte();
                // Upvalues are guaranteed to be filled in by the time we get here
                self.push(self.currentFrame().closure.upvalues[slot].?.location.*);
            },
            .SetUpvalue => {
                const slot = self.readByte();
                // Upvalues are guaranteed to be filled in by the time we get here
                self.currentFrame().closure.upvalues[slot].?.location.* = self.peek(0);
            },
            .GetProperty => {
                const maybeObj = self.peek(0);
                if (!maybeObj.isObj()) return self.runtimeError("Only instances have properties.", .{});
                const obj = maybeObj.asObj();
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
                            self.push(value);
                        } else {
                            try self.bindMethod(instance.class, name);
                        }
                    },
                }
            },
            .SetProperty => {
                const maybeObj = self.peek(1);
                if (!maybeObj.isObj()) return self.runtimeError("Only instances have fields.", .{});
                const obj = maybeObj.asObj();
                switch (obj.objType) {
                    .String, .Function, .NativeFunction, .Closure, .Upvalue, .Class, .BoundMethod => {
                        return self.runtimeError("Only instances have fields.", .{});
                    },
                    .Instance => {
                        const instance = obj.asInstance();
                        _ = try instance.fields.set(self.readString(), self.peek(0));

                        const value = self.pop();
                        _ = self.pop();
                        self.push(value);
                    },
                }
            },
            .GetSuper => {
                const name = self.readString();
                const superclass = self.pop().asObj().asClass();
                try self.bindMethod(superclass, name);
            },
            .CloseUpvalue => {
                self.closeUpvalues(&self.stack.items[self.stack.items.len - 2]);
                _ = self.pop();
            },
            .Class => {
                self.push((try Obj.Class.create(self, self.readString())).obj.value());
            },
            .Inherit => {
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
            },
            .Method => {
                try self.defineMethod(self.readString());
            },
            .Print => {
                try self.outWriter.print("{}\n", .{self.pop()});
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
                const superclass = self.pop().asObj().asClass();
                try self.invokeFromClass(superclass, method, argCount);
            },
            .Closure => {
                const constant = self.readByte();
                const value = self.currentChunk().constants.items[constant];
                const function = value.asObj().asFunction();
                const closure = try Obj.Closure.create(self, function);
                self.push(closure.obj.value());
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
                self.push(value);
            },
            .Nil => self.push(Value.nil()),
            .True => self.push(Value.fromBool(true)),
            .False => self.push(Value.fromBool(false)),
            .Equal => {
                const b = self.pop();
                const a = self.pop();
                self.push(Value.fromBool(a.equals(b)));
            },
            .Greater => {
                const rhs = self.pop();
                const lhs = self.pop();
                if (!lhs.isNumber() or !rhs.isNumber()) {
                    return self.runtimeError("Operands must be numbers.", .{});
                }
                self.push(Value.fromBool(lhs.asNumber() > rhs.asNumber()));
            },
            .Less => {
                const rhs = self.pop();
                const lhs = self.pop();
                if (!lhs.isNumber() or !rhs.isNumber()) {
                    return self.runtimeError("Operands must be numbers.", .{});
                }
                self.push(Value.fromBool(lhs.asNumber() < rhs.asNumber()));
            },
            .Negate => {
                const value = self.pop();
                if (!value.isNumber()) return self.runtimeError("Operand must be a number.", .{});
                self.push(Value.fromNumber(-value.asNumber()));
            },
            .Add => {
                const rhs = self.pop();
                const lhs = self.pop();
                if (lhs.isObj() and rhs.isObj()) {
                    try self.concatenate(lhs.asObj(), rhs.asObj());
                } else if (lhs.isNumber() and rhs.isNumber()) {
                    self.push(Value.fromNumber(lhs.asNumber() + rhs.asNumber()));
                } else {
                    return self.runtimeError("Operands must be two numbers or two strings.", .{});
                }
            },
            .Subtract => try self.binaryNumericOp(sub),
            .Multiply => try self.binaryNumericOp(mul),
            .Divide => try self.binaryNumericOp(div),
            .Not => self.push(Value.fromBool(self.pop().isFalsey())),
        }
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
                try self.runtimeError("Operands must be strings.", .{});
            },
            .String => switch (rhs.objType) {
                .Function, .NativeFunction, .Closure, .Upvalue, .Class, .Instance, .BoundMethod => {
                    try self.runtimeError("Operands must be strings.", .{});
                },
                .String => {
                    // Temporarily put the strings back on the stack so
                    // they're visible to the GC when we allocate
                    self.push(lhs.value());
                    self.push(rhs.value());
                    const lhsStr = lhs.asString();
                    const rhsStr = rhs.asString();
                    const buffer = try self.allocator.alloc(u8, lhsStr.bytes.len + rhsStr.bytes.len);
                    @memcpy(buffer[0..lhsStr.bytes.len], lhsStr.bytes);
                    @memcpy(buffer[lhsStr.bytes.len..], rhsStr.bytes);
                    _ = self.pop();
                    _ = self.pop();
                    self.push((try Obj.String.create(self, buffer)).obj.value());
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
        return (@as(u16, @intCast(items[frame.ip - 2])) << 8) | items[frame.ip - 1];
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
            // NOTE, book uses a pointer into the stack called "slots",
            // but we use an index into the stack instead. Goal was to
            // make stack resizable, but that ended up being tricky
            // because upvalues hold pointers into the stack, and
            // because pushing to the stack is a very hot operation that
            // needs to be as fast as possible.
            .start = self.stack.items.len - argCount - 1,
        });
    }

    fn callValue(self: *VM, callee: Value, argCount: usize) !void {
        if (!callee.isObj()) return self.runtimeError("Can only call functions and classes.", .{});

        const obj = callee.asObj();
        switch (obj.objType) {
            .String, .Function, .Upvalue, .Instance => {
                return self.runtimeError("Can only call functions and classes.", .{});
            },
            .Closure => try self.call(obj.asClosure(), argCount),
            .NativeFunction => {
                const args = self.stack.items[self.stack.items.len - 1 - argCount ..];
                try self.stack.resize(self.stack.items.len - 1 - argCount);
                const result = obj.asNativeFunction().function(args);
                self.push(result);
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
                    try self.call(initializer.asObj().asClosure(), argCount);
                } else if (argCount != 0) {
                    return self.runtimeError("Expected 0 arguments but got {}.", .{argCount});
                }
            },
        }
    }

    fn invoke(self: *VM, name: *Obj.String, argCount: u8) !void {
        const receiver = self.peek(argCount).asObj();

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
            if (@intFromPtr(upvalue.location) <= @intFromPtr(local)) break;
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
            if (@intFromPtr(openUpvalues.location) < @intFromPtr(last)) break;
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

    fn runtimeError(self: *VM, comptime message: []const u8, args: anytype) !void {
        @branchHint(.cold);

        try self.errWriter.print(message, args);
        try self.errWriter.print("\n", .{});

        while (self.frames.pop()) |frame| {
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
