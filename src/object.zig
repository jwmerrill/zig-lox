const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;
const VM = @import("./vm.zig").VM;
const Chunk = @import("./chunk.zig").Chunk;
const debug = @import("./debug.zig");
const Table = @import("./table.zig").Table;

pub const Obj = struct {
    next: ?*Obj,
    objType: Type,
    isMarked: bool,

    pub const Type = enum { String, Function, NativeFunction, Closure, Upvalue, Class, Instance, BoundMethod };

    pub fn allocate(vm: *VM, comptime T: type, objType: Type) !*Obj {
        const ptr = try vm.allocator.create(T);

        ptr.obj = Obj{
            .next = vm.objects,
            .objType = objType,
            .isMarked = false,
        };

        vm.objects = &ptr.obj;

        if (debug.LOG_GC) {
            std.debug.warn("{} allocate {} for {}\n", .{ @ptrToInt(&ptr.obj), @sizeOf(T), @typeName(T) });
        }

        return &ptr.obj;
    }

    pub fn destroy(self: *Obj, vm: *VM) void {
        if (debug.LOG_GC) {
            std.debug.warn("{} free {} {}\n", .{ @ptrToInt(self), self.objType, self.value() });
        }

        switch (self.objType) {
            .String => self.asString().destroy(vm),
            .Function => self.asFunction().destroy(vm),
            .NativeFunction => self.asNativeFunction().destroy(vm),
            .Closure => self.asClosure().destroy(vm),
            .Upvalue => self.asUpvalue().destroy(vm),
            .Class => self.asClass().destroy(vm),
            .Instance => self.asInstance().destroy(vm),
            .BoundMethod => self.asBoundMethod().destroy(vm),
        }
    }

    pub fn isString(self: *Obj) bool {
        return self.objType == .String;
    }

    pub fn isFunction(self: *Obj) bool {
        return self.objType == .Function;
    }

    pub fn isNativeFunction(self: *Obj) bool {
        return self.objType == .NativeFunction;
    }

    pub fn isClosure(self: *Obj) bool {
        return self.objType == .Closure;
    }

    pub fn isUpvalue(self: *Obj) bool {
        return self.objType == .Upvalue;
    }

    pub fn isClass(self: *Obj) bool {
        return self.objType == .Class;
    }

    pub fn isInstance(self: *Obj) bool {
        return self.objType == .Instance;
    }

    pub fn isBoundMethod(self: *Obj) bool {
        return self.objType == .BoundMethod;
    }

    pub fn asString(self: *Obj) *String {
        return @fieldParentPtr(String, "obj", self);
    }

    pub fn asFunction(self: *Obj) *Function {
        return @fieldParentPtr(Function, "obj", self);
    }

    pub fn asNativeFunction(self: *Obj) *NativeFunction {
        return @fieldParentPtr(NativeFunction, "obj", self);
    }

    pub fn asClosure(self: *Obj) *Closure {
        return @fieldParentPtr(Closure, "obj", self);
    }

    pub fn asUpvalue(self: *Obj) *Upvalue {
        return @fieldParentPtr(Upvalue, "obj", self);
    }

    pub fn asClass(self: *Obj) *Class {
        return @fieldParentPtr(Class, "obj", self);
    }

    pub fn asInstance(self: *Obj) *Instance {
        return @fieldParentPtr(Instance, "obj", self);
    }

    pub fn asBoundMethod(self: *Obj) *BoundMethod {
        return @fieldParentPtr(BoundMethod, "obj", self);
    }

    pub fn value(self: *Obj) Value {
        return Value.fromObj(self);
    }

    pub const String = struct {
        obj: Obj,
        hash: u32,
        bytes: []const u8,

        pub fn create(vm: *VM, bytes: []const u8) !*String {
            const hash = hashFn(bytes);

            if (vm.strings.findString(bytes, hash)) |interned| {
                vm.allocator.free(bytes);
                return interned;
            } else {
                const obj = try Obj.allocate(vm, String, .String);
                const out = obj.asString();
                out.* = String{
                    .obj = obj.*,
                    .hash = hash,
                    .bytes = bytes,
                };
                // Make sure string is visible to the GC, since adding
                // to the table may allocate
                vm.push(out.obj.value());
                _ = try vm.strings.set(out, Value.fromBool(true));
                _ = vm.pop();
                return out;
            }
        }

        pub fn copy(vm: *VM, source: []const u8) !*String {
            const buffer = try vm.allocator.alloc(u8, source.len);
            std.mem.copy(u8, buffer, source);
            return String.create(vm, buffer);
        }

        pub fn destroy(self: *String, vm: *VM) void {
            vm.allocator.free(self.bytes);
            vm.allocator.destroy(self);
        }

        fn hashFn(bytes: []const u8) u32 {
            // NOTE zig standard library has its own implementation of this
            // FNV-1a hash function already, in std.hash.fnv
            var hash: u32 = 2166136261;

            for (bytes) |byte| {
                hash ^= byte;
                // NOTE Zig makes you use a special operator when you want
                // wraparound on overflow.
                hash *%= 16777619;
            }

            return hash;
        }
    };

    pub const Function = struct {
        obj: Obj,
        arity: u9,
        upvalueCount: u9,
        chunk: Chunk,
        name: ?*String,

        pub fn create(vm: *VM) !*Function {
            const obj = try Obj.allocate(vm, Function, .Function);
            const out = obj.asFunction();

            out.* = Function{
                .obj = obj.*,
                .arity = 0,
                .upvalueCount = 0,
                .name = null,
                .chunk = Chunk.init(vm.allocator),
            };

            return out;
        }

        pub fn destroy(self: *Function, vm: *VM) void {
            self.chunk.deinit();
            vm.allocator.destroy(self);
        }
    };

    pub const NativeFunction = struct {
        obj: Obj,
        function: NativeFunctionType,

        pub const NativeFunctionType = fn (args: []Value) Value;

        pub fn create(vm: *VM, function: NativeFunctionType) !*NativeFunction {
            const obj = try Obj.allocate(vm, NativeFunction, .NativeFunction);

            const out = obj.asNativeFunction();

            out.* = NativeFunction{
                .obj = obj.*,
                .function = function,
            };

            return out;
        }

        pub fn destroy(self: *NativeFunction, vm: *VM) void {
            vm.allocator.destroy(self);
        }
    };

    pub const Closure = struct {
        obj: Obj,
        function: *Function,
        upvalues: []?*Upvalue,

        pub fn create(vm: *VM, function: *Function) !*Closure {
            const upvalues = try vm.allocator.alloc(?*Upvalue, function.upvalueCount);
            // Need to null this out rather than leaving it
            // uninitialized becaue the GC might try to look at it
            // before it gets filled in with values
            for (upvalues) |*upvalue| upvalue.* = null;

            const obj = try Obj.allocate(vm, Closure, .Closure);
            const out = obj.asClosure();

            out.* = Closure{
                .obj = obj.*,
                .function = function,
                .upvalues = upvalues,
            };

            return out;
        }

        pub fn destroy(self: *Closure, vm: *VM) void {
            vm.allocator.free(self.upvalues);
            vm.allocator.destroy(self);
        }
    };

    pub const Upvalue = struct {
        obj: Obj,
        location: *Value,
        closed: Value,
        next: ?*Upvalue,

        pub fn create(vm: *VM, location: *Value) !*Upvalue {
            const obj = try Obj.allocate(vm, Upvalue, .Upvalue);
            const out = obj.asUpvalue();

            out.* = Upvalue{
                .obj = obj.*,
                .location = location,
                .closed = Value.nil(),
                .next = null,
            };

            return out;
        }

        pub fn destroy(self: *Upvalue, vm: *VM) void {
            vm.allocator.destroy(self);
        }
    };

    pub const Class = struct {
        obj: Obj,
        name: *String,
        methods: Table,

        pub fn create(vm: *VM, name: *String) !*Class {
            const obj = try Obj.allocate(vm, Class, .Class);
            const out = obj.asClass();

            out.* = Class{
                .obj = obj.*,
                .name = name,
                .methods = Table.init(vm.allocator),
            };

            return out;
        }

        pub fn destroy(self: *Class, vm: *VM) void {
            self.methods.deinit();
            vm.allocator.destroy(self);
        }
    };

    pub const Instance = struct {
        obj: Obj,
        class: *Class,
        fields: Table,

        pub fn create(vm: *VM, class: *Class) !*Instance {
            const obj = try Obj.allocate(vm, Instance, .Instance);
            const out = obj.asInstance();

            out.* = Instance{
                .obj = obj.*,
                .class = class,
                .fields = Table.init(vm.allocator),
            };

            return out;
        }

        pub fn destroy(self: *Instance, vm: *VM) void {
            self.fields.deinit();
            vm.allocator.destroy(self);
        }
    };

    pub const BoundMethod = struct {
        obj: Obj,
        receiver: Value,
        method: *Closure,

        pub fn create(vm: *VM, receiver: Value, method: *Closure) !*BoundMethod {
            const obj = try Obj.allocate(vm, BoundMethod, .BoundMethod);
            const out = obj.asBoundMethod();

            out.* = BoundMethod{
                .obj = obj.*,
                .receiver = receiver,
                .method = method,
            };

            return out;
        }

        pub fn destroy(self: *BoundMethod, vm: *VM) void {
            vm.allocator.destroy(self);
        }
    };
};
