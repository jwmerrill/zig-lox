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
            .String => |str| vm.allocator.free(str.bytes),
        }

        vm.allocator.destroy(self);
    }

    // NOTE takes ownership of bytes. The GC will take care of
    // deallocating them.
    pub fn string(vm: *VM, bytes: []const u8) !*Obj {
        var object = try Obj.create(vm);
        object.data = Obj.Data{
            .String = ObjString{ .bytes = bytes },
        };
        return object;
    }

    pub fn value(self: *Obj) Value {
        return Value{ .Obj = self };
    }
};

pub const ObjString = struct {
    bytes: []const u8,
};
