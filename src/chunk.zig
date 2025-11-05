const std = @import("std");
const ArrayList = std.ArrayList;

const Value = @import("value.zig").Value;

pub const OpCode = enum(u8) {
    Constant,
    ConstantLong,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Return,
};

const LineCount = struct {
    line: usize,
    count: usize,
};

pub const Chunk = struct {
    code: ArrayList(u8),
    lines: ArrayList(LineCount),
    constants: ArrayList(Value),

    pub fn init(allocator: std.mem.Allocator) !Chunk {
        return Chunk{
            .code = try ArrayList(u8).initCapacity(allocator, 0),
            .lines = try ArrayList(LineCount).initCapacity(allocator, 0),
            .constants = try ArrayList(Value).initCapacity(allocator, 0),
        };
    }

    pub fn deinit(self: *Chunk, allocator: std.mem.Allocator) void {
        self.code.deinit(allocator);
        self.lines.deinit(allocator);
        self.constants.deinit(allocator);
    }

    pub fn write(self: *Chunk, allocator: std.mem.Allocator, byte: u8, line: usize) !void {
        try self.code.append(allocator, byte);

        if (self.lines.items.len == 0 or self.lines.getLast().line != line) {
            try self.lines.append(
                allocator,
                .{ .line = line, .count = 1 },
            );
        } else {
            self.lines.items[self.lines.items.len - 1].count += 1;
        }
    }

    pub fn writeOp(self: *Chunk, allocator: std.mem.Allocator, op: OpCode, line: usize) !void {
        try self.write(allocator, @intFromEnum(op), line);
    }

    pub fn writeConstant(self: *Chunk, allocator: std.mem.Allocator, value: Value, line: usize) !void {
        const constant = try self.addConstant(allocator, value);

        if (constant < 256) {
            try self.writeOp(allocator, .Constant, line);
            try self.write(allocator, @intCast(constant), line);
        } else if (constant < 1 << 24) {
            try self.writeOp(allocator, .ConstantLong, line);
            try self.write(allocator, @intCast(constant >> 16), line);
            try self.write(allocator, @intCast((constant >> 8) & 0xFF), line);
            try self.write(allocator, @intCast(constant & 0xFF), line);
        } else {
            @panic("Reached limit of 2^24 constants per chunk");
        }
    }

    fn addConstant(self: *Chunk, allocator: std.mem.Allocator, value: Value) !usize {
        for (self.constants.items, 0..) |constant, i| {
            if (constant.equals(value)) {
                return i;
            }
        }

        try self.constants.append(allocator, value);
        return self.constants.items.len - 1;
    }

    pub fn disassemble(self: *const Chunk, name: []const u8) void {
        std.debug.print("== {s} ==\n", .{name});

        var offset: usize = 0;

        while (offset < self.code.items.len) {
            offset = self.disassembleInstruction(offset);
        }
    }

    pub fn disassembleInstruction(self: *const Chunk, offset: usize) usize {
        std.debug.print("{d:04} ", .{offset});

        const line = self.getLine(offset);
        if (offset > 0 and line == self.getLine(offset - 1)) {
            std.debug.print("   | ", .{});
        } else {
            std.debug.print("{d:4} ", .{line});
        }

        const instruction = @as(OpCode, @enumFromInt(self.code.items[offset]));
        return switch (instruction) {
            .Constant => self.constantInstruction("Constant", offset),
            .ConstantLong => self.constantLongInstruction("ConstantLong", offset),
            .Add => simpleInstruction("Add", offset),
            .Subtract => simpleInstruction("Subtract", offset),
            .Multiply => simpleInstruction("Multiply", offset),
            .Divide => simpleInstruction("Divide", offset),
            .Negate => simpleInstruction("Negate", offset),
            .Return => simpleInstruction("Return", offset),
        };
    }

    fn getLine(self: *const Chunk, offset: usize) usize {
        var current = offset;

        for (self.lines.items) |line_count| {
            if (current < line_count.count) {
                return line_count.line;
            }
            current -= line_count.count;
        }

        @panic("Line out of bounds");
    }

    fn simpleInstruction(name: []const u8, offset: usize) usize {
        std.debug.print("{s}\n", .{name});
        return offset + 1;
    }

    fn constantInstruction(self: *const Chunk, name: []const u8, offset: usize) usize {
        const constant = self.code.items[offset + 1];
        const value = self.constants.items[constant];

        std.debug.print("{s} {d:12} '{f}'\n", .{ name, constant, value });

        return offset + 2;
    }

    fn constantLongInstruction(self: *const Chunk, name: []const u8, offset: usize) usize {
        const constant = (@as(usize, self.code.items[offset + 1]) << 16) +
            (@as(usize, self.code.items[offset + 2]) << 8) +
            self.code.items[offset + 3];
        const value = self.constants.items[constant];

        std.debug.print("{s} {d:8} '{f}'\n", .{ name, constant, value });

        return offset + 4;
    }
};
