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
    parent_allocator: Allocator,
    bytesAllocated: usize,
    nextGC: usize,

    const Self = @This();
    const HEAP_GROW_FACTOR = 2;

    pub fn init(vm: *VM, parent_allocator: Allocator) GCAllocator {
        return .{
            .vm = vm,
            .parent_allocator = parent_allocator,
            .bytesAllocated = 0,
            .nextGC = 1024 * 1024,
        };
    }

    pub fn allocator(self: *Self) Allocator {
        return Allocator.init(self, alloc, resize, free);
    }

    fn alloc(self: *Self, len: usize, ptr_align: u29, len_align: u29, ret_addr: usize) error{OutOfMemory}![]u8 {
        if ((self.bytesAllocated + len > self.nextGC) or debug.STRESS_GC) {
            self.collectGarbage();
        }

        var out = try self.parent_allocator.rawAlloc(len, ptr_align, len_align, ret_addr);

        self.bytesAllocated += out.len;

        return out;
    }

    fn resize(self: *Self, buf: []u8, buf_align: u29, new_len: usize, len_align: u29, ret_addr: usize) ?usize {
        if (new_len > buf.len) {
            if ((self.bytesAllocated + (new_len - buf.len) > self.nextGC) or debug.STRESS_GC) {
                self.collectGarbage();
            }
        }

        if (self.parent_allocator.rawResize(buf, buf_align, new_len, len_align, ret_addr)) |resized_len| {
            if (resized_len > buf.len) {
                self.bytesAllocated += resized_len - buf.len;
            } else {
                self.bytesAllocated -= buf.len - resized_len;
            }
            return resized_len;
        } else {
            return null;
        }
    }

    fn free(
        self: *Self,
        buf: []u8,
        buf_align: u29,
        ret_addr: usize,
    ) void {
        self.parent_allocator.rawFree(buf, buf_align, ret_addr);
        self.bytesAllocated -= buf.len;
    }

    fn collectGarbage(self: *Self) void {
        if (debug.LOG_GC) {
            std.debug.print("-- gc begin\n", .{});
        }

        self.markRoots();
        self.traceReferences();
        self.removeUnreferencedStrings();
        self.sweep();

        self.nextGC = self.bytesAllocated * HEAP_GROW_FACTOR;

        if (debug.LOG_GC) {
            std.debug.print("-- gc end\n", .{});
        }
    }

    fn markRoots(self: *Self) void {
        for (self.vm.stack.items) |value| {
            self.markValue(value);
        }

        for (self.vm.frames.items) |frame| {
            self.markObject(&frame.closure.obj);
        }

        var maybeUpvalue = self.vm.openUpvalues;
        while (maybeUpvalue) |upvalue| {
            self.markObject(&upvalue.obj);
            maybeUpvalue = upvalue.next;
        }

        self.markTable(&self.vm.globals);
        self.markCompilerRoots();
        if (self.vm.initString) |initString| self.markObject(&initString.obj);
    }

    fn traceReferences(self: *Self) void {
        while (self.vm.nextGray) |obj| {
            self.vm.nextGray = obj.nextGray;
            obj.nextGray = null;
            self.blackenObject(obj);
        }
    }

    fn removeUnreferencedStrings(self: *Self) void {
        for (self.vm.strings.entries) |*entry| {
            if (entry.key) |key| {
                if (!key.obj.isMarked) {
                    _ = self.vm.strings.delete(key);
                }
            }
        }
    }

    fn sweep(self: *Self) void {
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

    fn blackenObject(self: *Self, obj: *Obj) void {
        if (debug.LOG_GC) {
            std.debug.print("{} blacken {}\n", .{ @intFromPtr(obj), obj.value() });
        }

        switch (obj.objType) {
            .Upvalue => self.markValue(obj.asUpvalue().closed),
            .Function => {
                const function = obj.asFunction();
                if (function.name) |name| self.markObject(&name.obj);
                self.markArray(function.chunk.constants.items);
            },
            .Closure => {
                const closure = obj.asClosure();
                self.markObject(&closure.function.obj);
                for (closure.upvalues) |maybeUpvalue| {
                    if (maybeUpvalue) |upvalue| {
                        self.markObject(&upvalue.obj);
                    }
                }
            },
            .Class => {
                const class = obj.asClass();
                self.markObject(&class.name.obj);
                self.markTable(&class.methods);
            },
            .Instance => {
                const instance = obj.asInstance();
                self.markObject(&instance.class.obj);
                self.markTable(&instance.fields);
            },
            .BoundMethod => {
                const bound = obj.asBoundMethod();
                self.markValue(bound.receiver);
                self.markObject(&bound.method.obj);
            },
            .NativeFunction, .String => {},
        }
    }

    fn markArray(self: *Self, values: []Value) void {
        for (values) |value| self.markValue(value);
    }

    fn markValue(self: *Self, value: Value) void {
        if (value.isObj()) self.markObject(value.asObj());
    }

    fn markObject(self: *Self, obj: *Obj) void {
        if (obj.isMarked) return;

        if (debug.LOG_GC) {
            std.debug.print("{} mark {}\n", .{ @intFromPtr(obj), obj.value() });
        }

        obj.isMarked = true;

        obj.nextGray = self.vm.nextGray;
        self.vm.nextGray = obj;
    }

    fn markTable(self: *Self, table: *Table) void {
        for (table.entries) |entry| {
            if (entry.key) |key| self.markObject(&key.obj);
            self.markValue(entry.value);
        }
    }

    fn markCompilerRoots(self: *Self) void {
        if (self.vm.parser) |parser| {
            var maybeCompiler: ?*Compiler = parser.compiler;

            while (maybeCompiler) |compiler| {
                self.markObject(&compiler.function.obj);
                maybeCompiler = compiler.enclosing;
            }
        }
    }
};
