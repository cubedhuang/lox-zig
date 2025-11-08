const std = @import("std");
const ArrayList = std.ArrayList;

const Value = @import("value.zig").Value;

pub const OpCode = enum(u8) {
    Constant,
    ConstantLong,
    Nil,
    True,
    False,
    Pop,

    GetLocal,
    GetLocalLong,
    SetLocal,
    SetLocalLong,
    GetGlobal,
    GetGlobalLong,
    DefineGlobal,
    DefineGlobalLong,
    SetGlobal,
    SetGlobalLong,
    GetUpvalue,
    GetUpvalueLong,
    SetUpvalue,
    SetUpvalueLong,

    Equal,
    Greater,
    Less,

    Add,
    Subtract,
    Multiply,
    Divide,

    Not,
    Negate,
    Print,

    Jump,
    JumpIfFalse,
    Loop,
    Call,
    Closure,
    ClosureLong,
    CloseUpvalue,
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

    /// writes an op that relies on either a short or long constant with both op types
    pub fn writeOpWithConstant(
        self: *Chunk,
        allocator: std.mem.Allocator,
        op: OpCode,
        opLong: OpCode,
        constant: usize,
        line: usize,
    ) !void {
        if (constant < 256) {
            try self.writeOp(allocator, op, line);
            try self.write(allocator, @intCast(constant), line);
        } else if (constant < 1 << 24) {
            try self.writeOp(allocator, opLong, line);
            try self.write(allocator, @intCast(constant >> 16), line);
            try self.write(allocator, @intCast((constant >> 8) & 0xFF), line);
            try self.write(allocator, @intCast(constant & 0xFF), line);
        } else {
            @panic("Reached limit of 2^24 constants per chunk");
        }
    }

    /// writes an op that relies on either a short or long constant with both op types
    pub fn writeOpWithConstantValue(
        self: *Chunk,
        allocator: std.mem.Allocator,
        op: OpCode,
        opLong: OpCode,
        value: Value,
        line: usize,
    ) !void {
        const constant = try self.addConstant(allocator, value);

        try self.writeOpWithConstant(allocator, op, opLong, constant, line);
    }

    pub fn writeConstant(self: *Chunk, allocator: std.mem.Allocator, value: Value, line: usize) !void {
        try self.writeOpWithConstantValue(allocator, .Constant, .ConstantLong, value, line);
    }

    pub fn addConstant(self: *Chunk, allocator: std.mem.Allocator, value: Value) !usize {
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
            .Constant => self.constantInstruction("Constant", false, offset),
            .ConstantLong => self.constantInstruction("ConstantLong", true, offset),
            .Nil => simpleInstruction("Nil", offset),
            .True => simpleInstruction("True", offset),
            .False => simpleInstruction("False", offset),
            .Pop => simpleInstruction("Pop", offset),
            .GetLocal => self.byteInstruction("GetLocal", false, offset),
            .GetLocalLong => self.byteInstruction("GetLocalLong", true, offset),
            .SetLocal => self.byteInstruction("SetLocal", false, offset),
            .SetLocalLong => self.byteInstruction("SetLocalLong", true, offset),
            .GetGlobal => self.constantInstruction("GetGlobal", false, offset),
            .GetGlobalLong => self.constantInstruction("GetGlobalLong", true, offset),
            .DefineGlobal => self.constantInstruction("DefineGlobal", false, offset),
            .DefineGlobalLong => self.constantInstruction("DefineGlobalLong", true, offset),
            .SetGlobal => self.constantInstruction("SetGlobal", false, offset),
            .SetGlobalLong => self.constantInstruction("SetGlobalLong", true, offset),
            .GetUpvalue => self.byteInstruction("GetUpvalue", false, offset),
            .GetUpvalueLong => self.byteInstruction("GetUpvalueLong", true, offset),
            .SetUpvalue => self.byteInstruction("SetUpvalue", false, offset),
            .SetUpvalueLong => self.byteInstruction("SetUpvalueLong", true, offset),
            .Equal => simpleInstruction("Equal", offset),
            .Greater => simpleInstruction("Greater", offset),
            .Less => simpleInstruction("Less", offset),
            .Add => simpleInstruction("Add", offset),
            .Subtract => simpleInstruction("Subtract", offset),
            .Multiply => simpleInstruction("Multiply", offset),
            .Divide => simpleInstruction("Divide", offset),
            .Not => simpleInstruction("Not", offset),
            .Negate => simpleInstruction("Negate", offset),
            .Print => simpleInstruction("Print", offset),
            .Jump => self.jumpInstruction("Jump", 1, offset),
            .JumpIfFalse => self.jumpInstruction("JumpIfFalse", 1, offset),
            .Loop => self.jumpInstruction("Loop", -1, offset),
            .Call => self.byteInstruction("Call", false, offset),
            .Closure => self.closureInstruction(false, offset),
            .ClosureLong => self.closureInstruction(true, offset),
            .CloseUpvalue => simpleInstruction("CloseUpvalue", offset),
            .Return => simpleInstruction("Return", offset),
        };
    }

    pub fn getLine(self: *const Chunk, offset: usize) usize {
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

    fn constantInstruction(self: *const Chunk, name: []const u8, long: bool, offset: usize) usize {
        var off = offset + 1;
        const constant = self.read(long, off);
        if (long) {
            off += 3;
        } else {
            off += 1;
        }

        const value = self.constants.items[constant];

        std.debug.print("{s:<16} {d:4} '{f}'\n", .{ name, constant, value });

        return off;
    }

    fn byteInstruction(self: *const Chunk, name: []const u8, long: bool, offset: usize) usize {
        var off = offset + 1;
        const slot = self.read(long, off);
        if (long) {
            off += 3;
        } else {
            off += 1;
        }

        std.debug.print("{s:<16} {d:4}\n", .{ name, slot });

        return off;
    }

    fn jumpInstruction(self: *const Chunk, name: []const u8, sign: isize, offset: usize) usize {
        var off = offset + 1;
        const jump = self.readLong(off);
        off += 3;

        std.debug.print("{s:<16} {d:4} -> {d}\n", .{
            name,
            offset,
            @as(isize, @intCast(off)) +
                sign * @as(isize, @intCast(jump)),
        });

        return off;
    }

    fn closureInstruction(self: *const Chunk, long: bool, offset: usize) usize {
        var off = offset + 1;
        const constant = self.read(long, off);
        if (long) {
            off += 3;
        } else {
            off += 1;
        }

        std.debug.print("{s:<16} {d:4} {f}\n", .{
            if (long) "ClosureLong" else "Closure",
            constant,
            self.constants.items[constant],
        });

        const function = self.constants.items[constant].asObj().asFunction();
        for (0..function.upvalueCount) |_| {
            const originalOff = off;

            const indicator = self.code.items[off];
            off += 1;
            const isLong = (indicator & 1) != 0;
            const isLocal = (indicator >> 1) != 0;

            const index = self.read(isLong, off);
            if (isLong) {
                off += 3;
            } else {
                off += 1;
            }

            std.debug.print("{d:04}      |                     {s} {d}\n", .{
                originalOff,
                if (isLocal) "local" else "upvalue",
                index,
            });
        }

        return off;
    }

    fn read(self: *const Chunk, long: bool, offset: usize) usize {
        return if (long)
            self.readLong(offset)
        else
            self.code.items[offset];
    }

    fn readLong(self: *const Chunk, offset: usize) usize {
        return (@as(usize, self.code.items[offset]) << 16) +
            (@as(usize, self.code.items[offset + 1]) << 8) +
            self.code.items[offset + 2];
    }
};
