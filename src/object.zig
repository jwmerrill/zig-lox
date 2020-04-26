const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;
const VM = @import("./vm.zig").VM;
const Chunk = @import("./chunk.zig").Chunk;

pub const Obj = struct {
    next: ?*Obj,
    objType: Type,

    pub const Type = enum {
        String, Function, NativeFunction, Closure, Upvalue
    };

    pub fn create(vm: *VM, objType: Type) Obj {
        return Obj{
            .next = vm.objects,
            .objType = objType,
        };
    }

    pub fn destroy(self: *Obj, vm: *VM) void {
        switch (self.objType) {
            .String => self.asString().destroy(vm),
            .Function => self.asFunction().destroy(vm),
            .NativeFunction => self.asFunction().destroy(vm),
            .Closure => self.asClosure().destroy(vm),
            .Upvalue => self.asUpvalue().destroy(vm),
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

    pub fn value(self: *Obj) Value {
        return Value{ .Obj = self };
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
                var string = try vm.allocator.create(String);
                string.* = String{
                    .obj = Obj.create(vm, .String),
                    .hash = hash,
                    .bytes = bytes,
                };
                _ = try vm.strings.set(string, Value{ .Bool = true });
                return string;
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
        arity: u8,
        upvalueCount: u8,
        chunk: Chunk,
        name: ?*String,

        pub fn create(vm: *VM) !*Function {
            var function = try vm.allocator.create(Function);

            function.* = Function{
                .obj = Obj.create(vm, .Function),
                .arity = 0,
                .upvalueCount = 0,
                .name = null,
                .chunk = Chunk.init(vm.allocator),
            };

            return function;
        }

        pub fn destroy(self: *Function, vm: *VM) void {
            self.chunk.deinit();
            vm.allocator.destroy(self);

            // TODO, do we need to free name also, or will the GC take care
            // of that for us?
        }
    };

    pub const NativeFunction = struct {
        obj: Obj,
        function: NativeFunctionType,

        pub const NativeFunctionType = fn (args: []Value) Value;

        pub fn create(vm: *VM, function: NativeFunctionType) !*NativeFunction {
            const out = try vm.allocator.create(NativeFunction);

            out.* = NativeFunction{
                .obj = Obj.create(vm, .NativeFunction),
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
        upvalues: []*Upvalue,

        pub fn create(vm: *VM, function: *Function) !*Closure {
            const out = try vm.allocator.create(Closure);
            // NOTE book takes care to set each upvalue to null, for "GC reasons".
            // Can we skip this, since we're saying these pointers can't be null?
            const upvalues = try vm.allocator.alloc(*Upvalue, function.upvalueCount);

            out.* = Closure{
                .obj = Obj.create(vm, .Closure),
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
            const out = try vm.allocator.create(Upvalue);

            out.* = Upvalue{
                .obj = Obj.create(vm, .Upvalue),
                .location = location,
                .closed = Value.Nil,
                .next = null,
            };

            return out;
        }

        pub fn destroy(self: *Upvalue, vm: *VM) void {
            vm.allocator.destroy(self);
        }
    };
};
