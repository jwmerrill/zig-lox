const std = @import("std");
const VM = @import("./vm.zig").VM;
const Compiler = @import("./compiler.zig").Compiler;
const debug = @import("./debug.zig");
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;
const Obj = @import("./object.zig").Obj;
const Table = @import("./table.zig").Table;

pub const GCAllocator = struct {
    vm: *VM,
    backing_allocator: *Allocator,
    allocator: Allocator,
    bytesAllocated: usize,
    nextGC: usize,

    const HEAP_GROW_FACTOR = 2;

    pub fn init(vm: *VM, backing_allocator: *Allocator) GCAllocator {
        return GCAllocator{
            .vm = vm,
            .allocator = Allocator{
                .allocFn = alloc,
                .resizeFn = resize,
            },
            .backing_allocator = backing_allocator,
            .bytesAllocated = 0,
            .nextGC = 1024 * 1024,
        };
    }

    fn alloc(allocator: *Allocator, len: usize, ptr_align: u29, len_align: u29, ret_addr: usize) std.mem.Allocator.Error![]u8 {
        const self = @fieldParentPtr(GCAllocator, "allocator", allocator);

        if ((self.bytesAllocated + len > self.nextGC) or debug.STRESS_GC) {
            try self.collectGarbage();
        }

        var out = try self.backing_allocator.allocFn(self.backing_allocator, len, ptr_align, len_align, ret_addr);

        self.bytesAllocated += out.len;

        return out;
    }

    fn resize(allocator: *Allocator, buf: []u8, buf_align: u29, new_len: usize, len_align: u29, ret_addr: usize) std.mem.Allocator.Error!usize {
        const self = @fieldParentPtr(GCAllocator, "allocator", allocator);

        if (new_len > buf.len) {
            if ((self.bytesAllocated + (new_len - buf.len) > self.nextGC) or debug.STRESS_GC) {
                try self.collectGarbage();
            }
        }

        const out = try self.backing_allocator.resizeFn(self.backing_allocator, buf, buf_align, new_len, len_align, ret_addr);

        if (out > buf.len) {
            self.bytesAllocated += out - buf.len;
        } else {
            self.bytesAllocated -= buf.len - out;
        }

        return out;
    }

    fn collectGarbage(self: *GCAllocator) !void {
        if (debug.LOG_GC) {
            std.debug.warn("-- gc begin\n", .{});
        }

        try self.markRoots();
        try self.traceReferences();
        self.removeUnreferencedStrings();
        self.sweep();

        self.nextGC = self.bytesAllocated * HEAP_GROW_FACTOR;

        if (debug.LOG_GC) {
            std.debug.warn("-- gc end\n", .{});
        }
    }

    fn markRoots(self: *GCAllocator) !void {
        for (self.vm.stack.items) |value| {
            try self.markValue(value);
        }

        for (self.vm.frames.items) |frame| {
            try self.markObject(&frame.closure.obj);
        }

        var maybeUpvalue = self.vm.openUpvalues;
        while (maybeUpvalue) |upvalue| {
            try self.markObject(&upvalue.obj);
            maybeUpvalue = upvalue.next;
        }

        try self.markTable(&self.vm.globals);
        try self.markCompilerRoots();
        if (self.vm.initString) |initString| try self.markObject(&initString.obj);
    }

    fn traceReferences(self: *GCAllocator) !void {
        while (self.vm.grayStack.items.len > 0) {
            const obj = self.vm.grayStack.pop();
            try self.blackenObject(obj);
        }
    }

    fn removeUnreferencedStrings(self: *GCAllocator) void {
        for (self.vm.strings.entries) |*entry| {
            if (entry.key) |key| {
                if (!key.obj.isMarked) {
                    _ = self.vm.strings.delete(key);
                }
            }
        }
    }

    fn sweep(self: *GCAllocator) void {
        var previous: ?*Obj = null;
        var maybeObject = self.vm.objects;
        while (maybeObject) |object| {
            if (object.isMarked) {
                object.isMarked = false;
                previous = object;
                maybeObject = object.next;
            } else {
                const unreached = object;
                maybeObject = object.next;
                if (previous) |p| {
                    p.next = maybeObject;
                } else {
                    self.vm.objects = maybeObject;
                }

                unreached.destroy(self.vm);
            }
        }
    }

    fn blackenObject(self: *GCAllocator, obj: *Obj) !void {
        if (debug.LOG_GC) {
            std.debug.warn("{} blacken {}\n", .{ @ptrToInt(obj), obj.value() });
        }

        switch (obj.objType) {
            .Upvalue => try self.markValue(obj.asUpvalue().closed),
            .Function => {
                const function = obj.asFunction();
                if (function.name) |name| try self.markObject(&name.obj);
                try self.markArray(function.chunk.constants.items);
            },
            .Closure => {
                const closure = obj.asClosure();
                try self.markObject(&closure.function.obj);
                for (closure.upvalues) |maybeUpvalue| {
                    if (maybeUpvalue) |upvalue| {
                        try self.markObject(&upvalue.obj);
                    }
                }
            },
            .Class => {
                const class = obj.asClass();
                try self.markObject(&class.name.obj);
                try self.markTable(&class.methods);
            },
            .Instance => {
                const instance = obj.asInstance();
                try self.markObject(&instance.class.obj);
                try self.markTable(&instance.fields);
            },
            .BoundMethod => {
                const bound = obj.asBoundMethod();
                try self.markValue(bound.receiver);
                try self.markObject(&bound.method.obj);
            },
            .NativeFunction, .String => {},
        }
    }

    fn markArray(self: *GCAllocator, values: []Value) !void {
        for (values) |value| try self.markValue(value);
    }

    fn markValue(self: *GCAllocator, value: Value) !void {
        if (value.isObj()) try self.markObject(value.asObj());
    }

    fn markObject(self: *GCAllocator, obj: *Obj) !void {
        if (obj.isMarked) return;

        if (debug.LOG_GC) {
            std.debug.warn("{} mark {}\n", .{ @ptrToInt(obj), obj.value() });
        }

        obj.isMarked = true;

        try self.vm.grayStack.append(obj);
    }

    fn markTable(self: *GCAllocator, table: *Table) !void {
        for (table.entries) |entry| {
            if (entry.key) |key| try self.markObject(&key.obj);
            try self.markValue(entry.value);
        }
    }

    fn markCompilerRoots(self: *GCAllocator) !void {
        if (self.vm.parser) |parser| {
            var maybeCompiler: ?*Compiler = parser.compiler;

            while (maybeCompiler) |compiler| {
                try self.markObject(&compiler.function.obj);
                maybeCompiler = compiler.enclosing;
            }
        }
    }
};
