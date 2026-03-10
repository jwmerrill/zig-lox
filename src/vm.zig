const std = @import("std");
const ArrayListUnmanaged = std.ArrayListUnmanaged;
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
const Writer = std.Io.Writer;
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

// Inline bytecode reading helpers that operate on local ip/code variables.
// Using inline fn with pointer parameters lets the compiler keep ip in a
// register while keeping the encoding logic in one place.

inline fn readByte(ip: *usize, code_: []u8) u8 {
    const byte = code_[ip.*];
    ip.* += 1;
    return byte;
}

inline fn readShort(ip: *usize, code_: []u8) u16 {
    const hi: u16 = @intCast(code_[ip.*]);
    const lo: u16 = code_[ip.* + 1];
    ip.* += 2;
    return (hi << 8) | lo;
}

inline fn readConstant(ip: *usize, code_: []u8, constants: []const Value) Value {
    return constants[readByte(ip, code_)];
}

inline fn readString(ip: *usize, code_: []u8, constants: []const Value) *Obj.String {
    return readConstant(ip, code_, constants).asObj().asString();
}

inline fn nextOp(ip: *usize, code_: []u8, self: *VM) OpCode {
    if (debug.TRACE_EXECUTION) {
        self.printStack() catch {};
        _ = self.currentChunk().disassembleInstruction(ip.*);
    }
    return @enumFromInt(readByte(ip, code_));
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
    frames: ArrayListUnmanaged(CallFrame), // NOTE, book uses a fixed size stack
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
        self.frames = .{};
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
        self.frames.deinit(self.allocator);
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
        // Cache ip and code as locals to encourage register allocation.
        // These are the two hottest accesses in the dispatch loop.
        var ip = self.currentFrame().ip;
        var code = self.currentChunk().code.items;

        // Helpers to save/reload locals at frame boundaries.
        const saveAndLoad = struct {
            inline fn saveIp(vm: *VM, ip_val: usize) void {
                vm.currentFrame().ip = ip_val;
            }
            inline fn load(vm: *VM) struct { usize, []u8 } {
                const frame = vm.currentFrame();
                return .{ frame.ip, frame.closure.function.chunk.code.items };
            }
        };

        // Use labeled switch for efficient dispatch. Each prong jumps
        // directly to the next instruction's handler, giving the CPU's
        // branch predictor separate branch sites per opcode transition.
        const initial: OpCode = nextOp(&ip, code, self);
        dispatch: switch (initial) {
            .Return => {
                const result = self.pop();
                const frame = self.frames.pop() orelse unreachable;

                self.closeUpvalues(&self.stack.items[frame.start]);

                if (self.frames.items.len == 0) return;

                try self.stack.resize(frame.start);
                self.push(result);
                ip, code = saveAndLoad.load(self);
                continue :dispatch nextOp(&ip, code, self);
            },
            .Pop => {
                _ = self.pop();
                continue :dispatch nextOp(&ip, code, self);
            },
            .GetLocal => {
                const slot = readByte(&ip, code);
                self.push(self.stack.items[self.currentFrame().start + slot]);
                continue :dispatch nextOp(&ip, code, self);
            },
            .SetLocal => {
                const slot = readByte(&ip, code);
                self.stack.items[self.currentFrame().start + slot] = self.peek(0);
                continue :dispatch nextOp(&ip, code, self);
            },
            .GetGlobal => {
                const name = readString(&ip, code, self.currentFrame().closure.function.chunk.constants.items);
                var value: Value = undefined;
                if (!self.globals.get(name, &value)) {
                    saveAndLoad.saveIp(self, ip);
                    return self.runtimeError("Undefined variable '{s}'.", .{name.bytes});
                }
                self.push(value);
                continue :dispatch nextOp(&ip, code, self);
            },
            .DefineGlobal => {
                _ = try self.globals.set(readString(&ip, code, self.currentFrame().closure.function.chunk.constants.items), self.peek(0));
                // NOTE don't pop until value is in the hash table so
                // that we don't lose the value if the GC runs during
                // the set operation
                _ = self.pop();
                continue :dispatch nextOp(&ip, code, self);
            },
            .SetGlobal => {
                const name = readString(&ip, code, self.currentFrame().closure.function.chunk.constants.items);
                if (try self.globals.set(name, self.peek(0))) {
                    _ = self.globals.delete(name);
                    saveAndLoad.saveIp(self, ip);
                    return self.runtimeError("Undefined variable '{s}'.", .{name.bytes});
                }
                continue :dispatch nextOp(&ip, code, self);
            },
            .GetUpvalue => {
                const slot = readByte(&ip, code);
                // Upvalues are guaranteed to be filled in by the time we get here
                self.push(self.currentFrame().closure.upvalues[slot].?.location.*);
                continue :dispatch nextOp(&ip, code, self);
            },
            .SetUpvalue => {
                const slot = readByte(&ip, code);
                // Upvalues are guaranteed to be filled in by the time we get here
                self.currentFrame().closure.upvalues[slot].?.location.* = self.peek(0);
                continue :dispatch nextOp(&ip, code, self);
            },
            .GetProperty => {
                const maybeObj = self.peek(0);
                if (!maybeObj.isObj()) {
                    saveAndLoad.saveIp(self, ip);
                    return self.runtimeError("Only instances have properties.", .{});
                }
                const obj = maybeObj.asObj();
                switch (obj.objType) {
                    .String, .Function, .NativeFunction, .Closure, .Upvalue, .Class, .BoundMethod => {
                        saveAndLoad.saveIp(self, ip);
                        return self.runtimeError("Only instances have properties.", .{});
                    },
                    .Instance => {
                        const instance = obj.asInstance();
                        const name = readString(&ip, code, self.currentFrame().closure.function.chunk.constants.items);

                        var value: Value = undefined;
                        if (instance.fields.get(name, &value)) {
                            _ = self.pop(); // Instance.
                            self.push(value);
                        } else {
                            saveAndLoad.saveIp(self, ip);
                            try self.bindMethod(instance.class, name);
                        }
                    },
                }
                continue :dispatch nextOp(&ip, code, self);
            },
            .SetProperty => {
                const maybeObj = self.peek(1);
                if (!maybeObj.isObj()) {
                    saveAndLoad.saveIp(self, ip);
                    return self.runtimeError("Only instances have fields.", .{});
                }
                const obj = maybeObj.asObj();
                switch (obj.objType) {
                    .String, .Function, .NativeFunction, .Closure, .Upvalue, .Class, .BoundMethod => {
                        saveAndLoad.saveIp(self, ip);
                        return self.runtimeError("Only instances have fields.", .{});
                    },
                    .Instance => {
                        const instance = obj.asInstance();
                        _ = try instance.fields.set(readString(&ip, code, self.currentFrame().closure.function.chunk.constants.items), self.peek(0));

                        const value = self.pop();
                        _ = self.pop();
                        self.push(value);
                    },
                }
                continue :dispatch nextOp(&ip, code, self);
            },
            .GetSuper => {
                const name = readString(&ip, code, self.currentFrame().closure.function.chunk.constants.items);
                const superclass = self.pop().asObj().asClass();
                saveAndLoad.saveIp(self, ip);
                try self.bindMethod(superclass, name);
                continue :dispatch nextOp(&ip, code, self);
            },
            .CloseUpvalue => {
                self.closeUpvalues(&self.stack.items[self.stack.items.len - 2]);
                _ = self.pop();
                continue :dispatch nextOp(&ip, code, self);
            },
            .Class => {
                self.push((try Obj.Class.create(self, readString(&ip, code, self.currentFrame().closure.function.chunk.constants.items))).obj.value());
                continue :dispatch nextOp(&ip, code, self);
            },
            .Inherit => {
                const maybeObj = self.peek(1);
                if (!maybeObj.isObj()) {
                    saveAndLoad.saveIp(self, ip);
                    return self.runtimeError("Superclass must be a class.", .{});
                }
                const obj = maybeObj.asObj();
                if (!obj.isClass()) {
                    saveAndLoad.saveIp(self, ip);
                    return self.runtimeError("Superclass must be a class.", .{});
                }
                const superclass = obj.asClass();
                const subclass = self.peek(0).asObj().asClass();

                for (superclass.methods.entries) |entry| {
                    if (entry.key) |key| _ = try subclass.methods.set(key, entry.value);
                }

                _ = self.pop(); // Subclass
                continue :dispatch nextOp(&ip, code, self);
            },
            .Method => {
                try self.defineMethod(readString(&ip, code, self.currentFrame().closure.function.chunk.constants.items));
                continue :dispatch nextOp(&ip, code, self);
            },
            .Print => {
                try self.outWriter.print("{f}\n", .{self.pop()});
                try self.outWriter.flush();
                continue :dispatch nextOp(&ip, code, self);
            },
            .Jump => {
                const offset = readShort(&ip, code);
                ip += offset;
                continue :dispatch nextOp(&ip, code, self);
            },
            .JumpIfFalse => {
                const offset = readShort(&ip, code);
                if (self.peek(0).isFalsey()) ip += offset;
                continue :dispatch nextOp(&ip, code, self);
            },
            .Loop => {
                const offset = readShort(&ip, code);
                ip -= offset;
                continue :dispatch nextOp(&ip, code, self);
            },
            .Call => {
                const argCount = readByte(&ip, code);
                saveAndLoad.saveIp(self, ip);
                try self.callValue(self.peek(argCount), argCount);
                ip, code = saveAndLoad.load(self);
                continue :dispatch nextOp(&ip, code, self);
            },
            .Invoke => {
                const method = readString(&ip, code, self.currentFrame().closure.function.chunk.constants.items);
                const argCount = readByte(&ip, code);
                saveAndLoad.saveIp(self, ip);
                try self.invoke(method, argCount);
                ip, code = saveAndLoad.load(self);
                continue :dispatch nextOp(&ip, code, self);
            },
            .SuperInvoke => {
                const method = readString(&ip, code, self.currentFrame().closure.function.chunk.constants.items);
                const argCount = readByte(&ip, code);
                const superclass = self.pop().asObj().asClass();
                saveAndLoad.saveIp(self, ip);
                try self.invokeFromClass(superclass, method, argCount);
                ip, code = saveAndLoad.load(self);
                continue :dispatch nextOp(&ip, code, self);
            },
            .Closure => {
                const constant = readByte(&ip, code);
                const value = self.currentFrame().closure.function.chunk.constants.items[constant];
                const function = value.asObj().asFunction();
                const closure = try Obj.Closure.create(self, function);
                self.push(closure.obj.value());
                for (closure.upvalues) |*upvalue| {
                    const isLocal = readByte(&ip, code) != 0;
                    const index = readByte(&ip, code);
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
                continue :dispatch nextOp(&ip, code, self);
            },
            .Constant => {
                const constant = readByte(&ip, code);
                const value = self.currentFrame().closure.function.chunk.constants.items[constant];
                self.push(value);
                continue :dispatch nextOp(&ip, code, self);
            },
            .Nil => {
                self.push(Value.nil());
                continue :dispatch nextOp(&ip, code, self);
            },
            .True => {
                self.push(Value.fromBool(true));
                continue :dispatch nextOp(&ip, code, self);
            },
            .False => {
                self.push(Value.fromBool(false));
                continue :dispatch nextOp(&ip, code, self);
            },
            .Equal => {
                const b = self.pop();
                const a = self.pop();
                self.push(Value.fromBool(a.equals(b)));
                continue :dispatch nextOp(&ip, code, self);
            },
            .Greater => {
                const rhs = self.pop();
                const lhs = self.pop();
                if (!lhs.isNumber() or !rhs.isNumber()) {
                    saveAndLoad.saveIp(self, ip);
                    return self.runtimeError("Operands must be numbers.", .{});
                }
                self.push(Value.fromBool(lhs.asNumber() > rhs.asNumber()));
                continue :dispatch nextOp(&ip, code, self);
            },
            .Less => {
                const rhs = self.pop();
                const lhs = self.pop();
                if (!lhs.isNumber() or !rhs.isNumber()) {
                    saveAndLoad.saveIp(self, ip);
                    return self.runtimeError("Operands must be numbers.", .{});
                }
                self.push(Value.fromBool(lhs.asNumber() < rhs.asNumber()));
                continue :dispatch nextOp(&ip, code, self);
            },
            .Negate => {
                const value = self.pop();
                if (!value.isNumber()) {
                    saveAndLoad.saveIp(self, ip);
                    return self.runtimeError("Operand must be a number.", .{});
                }
                self.push(Value.fromNumber(-value.asNumber()));
                continue :dispatch nextOp(&ip, code, self);
            },
            .Add => {
                const rhs = self.pop();
                const lhs = self.pop();
                if (lhs.isObj() and rhs.isObj()) {
                    saveAndLoad.saveIp(self, ip);
                    try self.concatenate(lhs.asObj(), rhs.asObj());
                } else if (lhs.isNumber() and rhs.isNumber()) {
                    self.push(Value.fromNumber(lhs.asNumber() + rhs.asNumber()));
                } else {
                    saveAndLoad.saveIp(self, ip);
                    return self.runtimeError("Operands must be two numbers or two strings.", .{});
                }
                continue :dispatch nextOp(&ip, code, self);
            },
            .Subtract => {
                saveAndLoad.saveIp(self, ip);
                try self.binaryNumericOp(sub);
                continue :dispatch nextOp(&ip, code, self);
            },
            .Multiply => {
                saveAndLoad.saveIp(self, ip);
                try self.binaryNumericOp(mul);
                continue :dispatch nextOp(&ip, code, self);
            },
            .Divide => {
                saveAndLoad.saveIp(self, ip);
                try self.binaryNumericOp(div);
                continue :dispatch nextOp(&ip, code, self);
            },
            .Not => {
                self.push(Value.fromBool(self.pop().isFalsey()));
                continue :dispatch nextOp(&ip, code, self);
            },
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

        try self.frames.append(self.allocator, CallFrame{
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
            std.debug.print("[ {f} ]", .{value});
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
            try self.errWriter.print("[line {d}] in {s}\n", .{ line, name });
        }

        try self.errWriter.flush();
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
