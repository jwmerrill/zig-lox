const std = @import("std");
const Allocator = std.mem.Allocator;
const Obj = @import("./object.zig").Obj;
const Value = @import("./value.zig").Value;

pub const Table = struct {
    allocator: *Allocator,
    entries: []Entry,
    count: usize,

    pub fn init(allocator: *Allocator) Table {
        return Table{
            .allocator = allocator,
            .entries = &[_]Entry{},
            .count = 0,
        };
    }

    pub fn deinit(self: *Table) void {
        self.allocator.free(self.entries);
    }

    pub fn set(self: *Table, key: *Obj.String, value: Value) !bool {
        // Encodes a 75% capacity
        if (4 * (self.count + 1) > 3 * self.entries.len) {
            try self.increaseCapacity();
        }

        const entry = findEntry(self.entries, key);

        const isNewKey = entry.key == null;

        if (isNewKey) {
            if (!entry.isTombstone()) self.count += 1;
        }

        entry.key = key;
        entry.value = value;

        return isNewKey;
    }

    pub fn get(self: *Table, key: *Obj.String, value: *Value) bool {
        if (self.entries.len == 0) return false;

        const entry = findEntry(self.entries, key);

        if (entry.key == null) return false;

        value.* = entry.value;
        return true;
    }

    pub fn delete(self: *Table, key: *Obj.String) bool {
        if (self.entries.len == 0) return false;

        var entry = findEntry(self.entries, key);
        if (entry.key == null) return false;

        // Add "tombstone" represented by empty key and true value
        entry.key = null;
        entry.value = Value{ .Bool = true };
        return true;
    }

    pub fn increaseCapacity(self: *Table) !void {
        const newCapacity = if (self.entries.len < 8) 8 else self.entries.len * 2;
        var entries = try self.allocator.alloc(Entry, newCapacity);

        // Initialize new entries to empty values
        for (entries) |*entry| {
            entry.key = null;
            entry.value = Value.Nil;
        }

        // Copy old entries to new entries
        self.count = 0;
        for (self.entries) |entry| {
            if (entry.key) |key| {
                var dest = findEntry(entries, key);
                dest.key = key;
                dest.value = entry.value;
                self.count += 1;
            }
        }

        // Free old entries
        self.allocator.free(self.entries);

        // Set entries reference to new entries
        self.entries = entries;
    }

    pub fn copyTo(self: *Table, dest: *Table) !void {
        for (self.entries) |entry| {
            if (entry.key) |key| {
                dest.set(key, entry.value);
            }
        }
    }

    pub fn findEntry(entries: []Entry, key: *Obj.String) *Entry {
        var index = key.hash % entries.len;
        var maybeTombstone: ?*Entry = null;

        // Linear probing on entries
        while (true) {
            var entry = &entries[index];

            if (entry.key) |entryKey| {
                if (entryKey == key) return entry;
            } else {
                if (entry.isTombstone()) {
                    if (maybeTombstone == null) maybeTombstone = entry;
                } else {
                    return if (maybeTombstone) |tombstone| tombstone else entry;
                }
            }

            index = (index + 1) % entries.len;
        }
    }

    pub fn findString(self: *Table, bytes: []const u8, hash: u32) ?*Obj.String {
        const entries = self.entries;
        if (entries.len == 0) return null;

        var index = hash % entries.len;

        // Linear probing on entries
        while (true) {
            var entry = entries[index];

            if (entry.key) |entryKey| {
                if (entryKey.hash == hash and
                    std.mem.eql(u8, entryKey.bytes, bytes))
                {
                    return entryKey;
                }
            } else if (!entry.isTombstone()) {
                // Stop if we find an empty non-tombstone entry.
                return null;
            }

            index = (index + 1) % entries.len;
        }
    }
};

pub const Entry = struct {
    key: ?*Obj.String,
    value: Value,

    pub fn isTombstone(self: *Entry) bool {
        if (self.key != null) {
            return false;
        } else {
            return switch (self.value) {
                .Nil => false,
                else => true,
            };
        }
    }
};
