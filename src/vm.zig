const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const DEBUG_TRACE_EXECUTION = @import("debug.zig").DEBUG_TRACE_EXECUTION;

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;

const compile = @import("compiler.zig").compile;

fn add(a: f64, b: f64) f64 {
    return a + b;
}

fn sub(a: f64, b: f64) f64 {
    return a - b;
}

fn mul(a: f64, b: f64) f64 {
    return a * b;
}

fn div(a: f64, b: f64) f64 {
    return a / b;
}

pub const InterpretError = error{
    CompileError,
    RuntimeError,
};

pub const VM = struct {
    allocator: Allocator,
    chunk: Chunk,
    ip: usize,
    stack: ArrayList(Value),

    pub fn init(allocator: Allocator) !VM {
        return VM{
            .allocator = allocator,
            .chunk = undefined,
            .ip = 0,
            .stack = try ArrayList(Value).initCapacity(allocator, 0),
        };
    }

    pub fn deinit(self: *VM) void {
        self.stack.deinit(self.allocator);
    }

    pub fn interpret(self: *VM, source: []const u8) !void {
        self.chunk = try compile(self.allocator, source);
        defer self.chunk.deinit(self.allocator);
        self.ip = 0;

        try self.run();
    }

    fn run(self: *VM) !void {
        while (true) {
            if (DEBUG_TRACE_EXECUTION) {
                self.printStack();

                _ = self.chunk.disassembleInstruction(self.ip);
            }

            const instruction = self.readByte();
            const op = @as(OpCode, @enumFromInt(instruction));

            switch (op) {
                .Constant => {
                    const value = self.readConstant();
                    try self.push(value);
                },
                .ConstantLong => {
                    const value = self.readConstantLong();
                    try self.push(value);
                },
                .Add => try self.binary(add),
                .Subtract => try self.binary(sub),
                .Multiply => try self.binary(mul),
                .Divide => try self.binary(div),
                .Negate => {
                    const value = self.pop();
                    try self.push(Value{ .Number = -value.Number });
                },
                .Return => {
                    std.debug.print("{f}\n", .{self.stack.pop().?});
                    return;
                },
            }
        }
    }

    fn binary(self: *VM, comptime op: fn (f64, f64) f64) !void {
        const b = self.pop();
        const a = self.pop();
        try self.push(Value{ .Number = op(a.Number, b.Number) });
    }

    fn printStack(self: *VM) void {
        std.debug.print("          ", .{});
        for (self.stack.items) |value| {
            std.debug.print("[ {f} ]", .{value});
        }
        std.debug.print("\n", .{});
    }

    fn readByte(self: *VM) u8 {
        defer self.ip += 1;
        return self.chunk.code.items[self.ip];
    }

    fn readConstant(self: *VM) Value {
        return self.chunk.constants.items[self.readByte()];
    }

    fn readConstantLong(self: *VM) Value {
        const constant = (@as(usize, self.chunk.code.items[self.readByte()]) << 16) +
            (@as(usize, self.chunk.code.items[self.readByte()]) << 8) +
            self.chunk.code.items[self.readByte()];
        return self.chunk.constants.items[constant];
    }

    fn push(self: *VM, value: Value) !void {
        try self.stack.append(self.allocator, value);
    }

    fn pop(self: *VM) Value {
        return self.stack.pop().?;
    }
};
