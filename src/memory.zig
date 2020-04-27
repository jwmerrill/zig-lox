const std = @import("std");
const VM = @import("./vm.zig").VM;
const Allocator = std.mem.Allocator;

pub const GCAllocator = struct {
    vm: *VM,
    backing_allocator: *Allocator,
    allocator: Allocator,

    pub fn init(vm: *VM, backing_allocator: *Allocator) GCAllocator {
        return GCAllocator{
            .vm = vm,
            .allocator = Allocator{
                .reallocFn = realloc,
                .shrinkFn = shrink,
            },
            .backing_allocator = backing_allocator
        };
    }

    fn realloc(allocator: *Allocator, old_mem: []u8, old_align: u29, new_size: usize, new_align: u29) ![]u8 {
        const self = @fieldParentPtr(GCAllocator, "allocator", allocator);
        return try self.backing_allocator.reallocFn(self.backing_allocator, old_mem, old_align, new_size, new_align);
    }

    fn shrink(allocator: *Allocator, old_mem: []u8, old_align: u29, new_size: usize, new_align: u29) []u8 {
        const self = @fieldParentPtr(GCAllocator, "allocator", allocator);
        return self.backing_allocator.shrinkFn(self.backing_allocator, old_mem, old_align, new_size, new_align);
    }
};
