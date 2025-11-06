const std = @import("std");

const Obj = @import("object.zig").Obj;
const Value = @import("value.zig").Value;

const Entry = struct {
    key: ?*Obj.String,
    value: Value,
};

pub const Table = struct {
    allocator: std.mem.Allocator,
    count: usize,
    entries: []Entry,

    pub fn init(allocator: std.mem.Allocator) Table {
        return Table{
            .allocator = allocator,
            .count = 0,
            .entries = &.{},
        };
    }

    pub fn deinit(self: *Table) void {
        self.allocator.free(self.entries);
    }

    pub fn get(self: *Table, key: *Obj.String) ?Value {
        if (self.count == 0) return null;

        const entry = findEntry(self.entries, key);
        if (entry.key == null) return null;

        return entry.value;
    }

    /// returns true if the key is new
    pub fn set(self: *Table, key: *Obj.String, value: Value) !bool {
        // self.count + 1 > self.entries.len * MAX_LOAD
        // MAX_LOAD = 0.75 = 3/4
        if (4 * (self.count + 1) > 3 * self.entries.len) {
            try self.grow();
        }

        var entry = findEntry(self.entries, key);
        const isNewKey = entry.key == null;
        if (isNewKey and entry.value.isNil()) self.count += 1;

        entry.key = key;
        entry.value = value;
        return isNewKey;
    }

    /// returns true if the key is deleted
    pub fn delete(self: *Table, key: *Obj.String) bool {
        if (self.count == 0) return false;

        const entry = findEntry(self.entries, key);
        if (entry.key == null) return false;

        // a tombstone is <null, true>!!
        entry.key = null;
        entry.value = Value.fromBool(true);
        return true;
    }

    pub fn addAll(self: *Table, from: *Table) !void {
        for (from.entries) |entry| {
            if (entry.key) |key| {
                try self.set(key, entry.value);
            }
        }
    }

    pub fn findString(self: *Table, bytes: []const u8, hash: u32) ?*Obj.String {
        if (self.count == 0) return null;

        var index = hash % self.entries.len;
        while (true) {
            const entry = &self.entries[index];
            if (entry.key) |key| {
                if (std.mem.eql(u8, key.buffer, bytes)) {
                    return key;
                }
            } else {
                // non-tombstone entry means that we're done
                if (entry.value.isNil()) {
                    return null;
                }
            }

            index = (index + 1) % self.entries.len;
        }
    }

    fn grow(self: *Table) !void {
        const capacity = if (self.entries.len < 8) 8 else self.entries.len * 2;

        const entries = try self.allocator.alloc(Entry, capacity);
        for (entries) |*entry| {
            entry.key = null;
            entry.value = Value.nil();
        }

        var count: usize = 0;
        for (self.entries) |entry| {
            if (entry.key) |key| {
                var dest = findEntry(entries, key);
                dest.key = key;
                dest.value = entry.value;
                count += 1;
            }
        }

        self.allocator.free(self.entries);
        self.entries = entries;
        self.count = count;
    }
};

fn findEntry(entries: []Entry, key: *Obj.String) *Entry {
    var index = key.hash % entries.len;
    var tombstone: ?*Entry = null;

    while (true) {
        const entry = &entries[index];
        if (entry.key == null) {
            if (entry.value.isNil()) {
                // it's really empty
                return if (tombstone) |value|
                    value
                else
                    entry;
            } else {
                // we found a tombstone
                if (tombstone == null)
                    tombstone = entry;
            }
        } else if (entry.key == key) {
            return entry;
        }
        index = (index + 1) % entries.len;
    }
}
