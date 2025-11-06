const std = @import("std");

const Obj = @import("object.zig").Obj;

pub const Value = UnionValue;

const UnionValue = union(enum) {
    Nil,
    Bool: bool,
    Number: f64,
    Obj: *Obj,

    pub fn equals(self: UnionValue, other: UnionValue) bool {
        if (std.meta.activeTag(self) != std.meta.activeTag(other)) {
            return false;
        }

        return switch (self) {
            .Nil => true,
            .Bool => |a| a == other.asBool(),
            .Number => |a| a == other.asNumber(),
            .Obj => |a| a == other.asObj(),
        };
    }

    pub fn format(self: UnionValue, writer: anytype) !void {
        return switch (self) {
            .Nil => writer.print("nil", .{}),
            .Bool => |val| writer.print("{}", .{val}),
            .Number => |val| writer.print("{d}", .{val}),
            .Obj => |val| val.format(writer),
        };
    }

    pub fn nil() UnionValue {
        return .{ .Nil = {} };
    }

    pub fn fromBool(value: bool) UnionValue {
        return .{ .Bool = value };
    }

    pub fn fromNumber(value: f64) UnionValue {
        return .{ .Number = value };
    }

    pub fn fromObj(value: *Obj) UnionValue {
        return .{ .Obj = value };
    }

    pub fn isNil(self: UnionValue) bool {
        return self == .Nil;
    }

    pub fn isBool(self: UnionValue) bool {
        return self == .Bool;
    }

    pub fn isFalsey(self: UnionValue) bool {
        return self.isNil() or self.isBool() and !self.asBool();
    }

    pub fn isNumber(self: UnionValue) bool {
        return self == .Number;
    }

    pub fn isObj(self: UnionValue) bool {
        return self == .Obj;
    }

    pub fn asBool(self: UnionValue) bool {
        return self.Bool;
    }

    pub fn asNumber(self: UnionValue) f64 {
        return self.Number;
    }

    pub fn asObj(self: UnionValue) *Obj {
        return self.Obj;
    }
};
