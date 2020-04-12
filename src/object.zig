const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;
const VM = @import("./vm.zig").VM;

// NOTE book uses "struct inheritance" pattern to lay this out in a
// a way that might be more memory efficient. Is it more memory
// efficient? Can that approach be replicated in Zig? Seems to involve
// some casts that Zig wouldn't consider safe
pub const Obj = struct {
    next: ?*Obj,
    data: Data,

    pub const Data = union(enum) {
        String: ObjString,
    };

    pub fn create(vm: *VM) !*Obj {
        var object = try vm.allocator.create(Obj);
        object.* = Obj{
            .next = vm.objects,
            .data = undefined,
        };
        vm.objects = object;
        return object;
    }

    pub fn destroy(self: *Obj, vm: *VM) void {
        switch (self.data) {
            .String => |*str| str.destroy(vm.allocator),
        }

        vm.allocator.destroy(self);
    }

    // NOTE takes ownership of bytes. The GC will take care of
    // deallocating them.
    pub fn string(vm: *VM, bytes: []const u8) !*Obj {
        var objString = ObjString.create(bytes);
        var maybeInterned = vm.strings.findString(objString);
        if (maybeInterned) |interned| {
            objString.destroy(vm.allocator);
            return interned.Obj;
        } else {
            var object = try Obj.create(vm);
            object.data = Obj.Data{
                .String = objString,
            };
            _ = try vm.strings.set(&object.data.String, object.value());
            return object;
        }
    }

    pub fn value(self: *Obj) Value {
        return Value{ .Obj = self };
    }
};

pub const ObjString = struct {
    hash: u32,
    bytes: []const u8,

    pub fn create(bytes: []const u8) ObjString {
        return ObjString{
            .hash = hashFn(bytes),
            .bytes = bytes,
        };
    }

    pub fn destroy(self: *ObjString, allocator: *Allocator) void {
        allocator.free(self.bytes);
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
