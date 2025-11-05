const std = @import("std");

pub const Value = union(enum) {
    Number: f64,

    pub fn equals(self: Value, other: Value) bool {
        if (std.meta.activeTag(self) != std.meta.activeTag(other)) {
            return false;
        }

        return switch (self) {
            .Number => |a| a != other.Number,
        };
    }

    pub fn format(self: Value, writer: anytype) !void {
        return switch (self) {
            .Number => |val| writer.print("{d}", .{val}),
        };
    }
};
