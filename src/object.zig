const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

pub const Obj = struct {
    type: Type,
    next: ?*Obj,

    pub const Type = enum { Function, String };

    pub fn init(vm: *VM, comptime T: type, objectType: Type) !*T {
        var object = try vm.allocator.create(T);

        object.obj.type = objectType;
        object.obj.next = vm.objects;
        vm.objects = &object.obj;

        return object;
    }

    pub fn deinit(self: *Obj, allocator: std.mem.Allocator) void {
        switch (self.type) {
            .Function => {
                const function = self.asFunction();
                function.chunk.deinit(allocator);
                allocator.destroy(function);
            },
            .String => {
                const string = self.asString();
                allocator.free(string.buffer);
                allocator.destroy(string);
            },
        }
    }

    pub fn format(self: *Obj, writer: anytype) !void {
        return switch (self.type) {
            .Function => if (self.asFunction().name) |name|
                writer.print("<fn {s}>", .{name.buffer})
            else
                writer.print("<script>", .{}),
            .String => writer.print("{s}", .{self.asString().buffer}),
        };
    }

    pub fn toValue(self: *Obj) Value {
        return Value.fromObj(self);
    }

    pub fn isFunction(self: *Obj) bool {
        return self.type == .Function;
    }

    pub fn isString(self: *Obj) bool {
        return self.type == .String;
    }

    pub fn asFunction(self: *Obj) *Function {
        return @fieldParentPtr("obj", self);
    }

    pub fn asString(self: *Obj) *String {
        return @fieldParentPtr("obj", self);
    }

    pub const Function = struct {
        obj: Obj,
        arity: usize,
        chunk: Chunk,
        name: ?*String,

        pub fn init(vm: *VM) !*Function {
            var function = try Obj.init(vm, Function, .Function);
            function.arity = 0;
            function.name = null;
            function.chunk = try Chunk.init(vm.allocator);
            return function;
        }
    };

    pub const String = struct {
        obj: Obj,
        buffer: []const u8,
        hash: u32,

        fn allocate(vm: *VM, buffer: []const u8, hash: u32) !*String {
            var string = try Obj.init(vm, String, .String);
            string.buffer = buffer;
            string.hash = hash;
            _ = try vm.strings.set(string, Value.nil());
            return string;
        }

        pub fn take(vm: *VM, buffer: []const u8) !*String {
            const hash = hashString(buffer);
            if (vm.strings.findString(buffer, hash)) |string| {
                vm.allocator.free(buffer);
                return string;
            }

            return try allocate(vm, buffer, hash);
        }

        pub fn copy(vm: *VM, source: []const u8) !*String {
            const hash = hashString(source);
            if (vm.strings.findString(source, hash)) |string| {
                return string;
            }

            const buffer = try vm.allocator.alloc(u8, source.len);
            @memcpy(buffer, source);
            return allocate(vm, buffer, hash);
        }

        fn hashString(bytes: []const u8) u32 {
            var hash: u32 = 2166136261;
            for (bytes) |b| {
                hash ^= b;
                hash *%= 16777619;
            }
            return hash;
        }
    };
};
