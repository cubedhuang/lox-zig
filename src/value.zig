const std = @import("std");

pub const Value = UnionValue;

const UnionValue = union(enum) {
    Nil,
    Bool: bool,
    Number: f64,

    pub fn equals(self: UnionValue, other: UnionValue) bool {
        if (std.meta.activeTag(self) != std.meta.activeTag(other)) {
            return false;
        }

        return switch (self) {
            .Nil => true,
            .Bool => |a| a == other.Bool,
            .Number => |a| a == other.Number,
        };
    }

    pub fn format(self: UnionValue, writer: anytype) !void {
        return switch (self) {
            .Nil => writer.print("nil", .{}),
            .Bool => |val| writer.print("{}", .{val}),
            .Number => |val| writer.print("{d}", .{val}),
        };
    }

    pub fn nil() UnionValue {
        return UnionValue{ .Nil = {} };
    }

    pub fn fromBool(value: bool) UnionValue {
        return UnionValue{ .Bool = value };
    }

    pub fn fromNumber(value: f64) UnionValue {
        return UnionValue{ .Number = value };
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

    pub fn asBool(self: UnionValue) bool {
        return self.Bool;
    }

    pub fn asNumber(self: UnionValue) f64 {
        return self.Number;
    }
};
