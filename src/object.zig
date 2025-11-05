const std = @import("std");

const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

pub const Obj = struct {
    type: Type,
    next: ?*Obj,

    pub const Type = enum { String };

    pub fn init(vm: *VM, comptime T: type, objectType: Type) !*T {
        var object = try vm.allocator.create(T);

        object.obj.type = objectType;
        object.obj.next = vm.objects;
        vm.objects = &object.obj;

        return object;
    }

    pub fn deinit(self: *Obj, allocator: std.mem.Allocator) void {
        switch (self.type) {
            .String => {
                const string = self.asString();
                allocator.free(string.buffer);
                allocator.destroy(string);
            },
        }
    }

    pub fn toValue(self: *Obj) Value {
        return Value.fromObj(self);
    }

    pub fn isString(self: *Obj) bool {
        return self.type == .String;
    }

    pub fn asString(self: *Obj) *String {
        return @fieldParentPtr("obj", self);
    }

    pub const String = struct {
        obj: Obj,
        buffer: []const u8,

        pub fn init(vm: *VM, buffer: []const u8) !*String {
            var string = try Obj.init(vm, String, .String);
            string.buffer = buffer;
            return string;
        }

        pub fn copy(vm: *VM, source: []const u8) !*String {
            const buffer = try vm.allocator.alloc(u8, source.len);
            @memcpy(buffer, source);
            return String.init(vm, buffer);
        }
    };
};
