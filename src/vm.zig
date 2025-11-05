const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const DEBUG_TRACE_EXECUTION = @import("debug.zig").DEBUG_TRACE_EXECUTION;

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;

const compile = @import("compiler.zig").compile;

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
                .Nil => try self.push(Value.nil()),
                .True => try self.push(Value.fromBool(true)),
                .False => try self.push(Value.fromBool(false)),
                .Equal => try self.push(Value.fromBool(self.pop().equals(self.pop()))),
                .Greater => try self.binary(Value.fromBool, gt),
                .Less => try self.binary(Value.fromBool, lt),
                .Add => try self.binary(Value.fromNumber, add),
                .Subtract => try self.binary(Value.fromNumber, sub),
                .Multiply => try self.binary(Value.fromNumber, mul),
                .Divide => try self.binary(Value.fromNumber, div),
                .Not => try self.push(Value.fromBool(self.pop().isFalsey())),
                .Negate => {
                    if (!self.peek(0).isNumber()) {
                        self.runtimeError("Operand must be a number.", .{});
                        return error.RuntimeError;
                    }
                    const value = self.pop();
                    try self.push(Value.fromNumber(-value.asNumber()));
                },
                .Return => {
                    std.debug.print("{f}\n", .{self.stack.pop().?});
                    return;
                },
            }
        }
    }

    fn binary(self: *VM, comptime valueType: anytype, comptime op: anytype) !void {
        if (!self.peek(0).isNumber() or !self.peek(1).isNumber()) {
            self.runtimeError("Operands must be numbers.", .{});
            return error.RuntimeError;
        }
        const b = self.pop().asNumber();
        const a = self.pop().asNumber();
        try self.push(valueType(op(a, b)));
    }

    fn printStack(self: *VM) void {
        std.debug.print("          ", .{});
        for (self.stack.items) |value| {
            std.debug.print("[ {f} ]", .{value});
        }
        std.debug.print("\n", .{});
    }

    fn resetStack(self: *VM) void {
        self.stack.clearRetainingCapacity();
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

    fn runtimeError(self: *VM, comptime fmt: []const u8, args: anytype) void {
        std.debug.print(fmt, args);

        const line = self.chunk.getLine(self.ip - 1);
        std.debug.print("\n[line {d}] in script\n", .{line});
        self.resetStack();
    }

    fn peek(self: *VM, distance: usize) Value {
        return self.stack.items[self.stack.items.len - 1 - distance];
    }

    fn push(self: *VM, value: Value) !void {
        try self.stack.append(self.allocator, value);
    }

    fn pop(self: *VM) Value {
        return self.stack.pop().?;
    }
};

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

fn gt(a: f64, b: f64) bool {
    return a > b;
}

fn lt(a: f64, b: f64) bool {
    return a < b;
}
