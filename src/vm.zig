const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const DEBUG_TRACE_EXECUTION = @import("debug.zig").DEBUG_TRACE_EXECUTION;

const Chunk = @import("chunk.zig").Chunk;
const Obj = @import("object.zig").Obj;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const Table = @import("table.zig").Table;

const compile = @import("compiler.zig").compile;

pub const InterpretError = error{
    CompileError,
    RuntimeError,
};

const CallFrame = struct {
    function: *Obj.Function,
    ip: usize,
    slots: []Value,
};

pub const VM = struct {
    allocator: Allocator,

    frames: ArrayList(CallFrame),
    stack: ArrayList(Value),
    globals: Table,
    strings: Table,
    objects: ?*Obj,

    pub fn init(allocator: Allocator) !VM {
        return VM{
            .allocator = allocator,
            .frames = try ArrayList(CallFrame).initCapacity(allocator, 0),
            .stack = try ArrayList(Value).initCapacity(allocator, 0),
            .globals = Table.init(allocator),
            .strings = Table.init(allocator),
            .objects = null,
        };
    }

    pub fn deinit(self: *VM) void {
        self.frames.deinit(self.allocator);
        self.stack.deinit(self.allocator);
        self.globals.deinit();
        self.strings.deinit();
        self.freeObjects();
    }

    fn freeObjects(self: *VM) void {
        var object = self.objects;
        while (object) |obj| {
            const next = obj.next;
            obj.deinit(self.allocator);
            object = next;
        }
    }

    pub fn interpret(self: *VM, source: []const u8) !void {
        std.debug.assert(self.stack.items.len == 0);
        defer std.debug.assert(self.stack.items.len == 0);

        const function = try compile(self, source);

        try self.push(function.obj.toValue());
        try self.call(function, 0);

        try self.run();
    }

    fn run(self: *VM) !void {
        while (true) {
            if (DEBUG_TRACE_EXECUTION) {
                self.printStack();

                _ = self.currentChunk().disassembleInstruction(self.currentFrame().ip);
            }

            const instruction = self.readByte();
            const op = @as(OpCode, @enumFromInt(instruction));

            switch (op) {
                .Constant, .ConstantLong => {
                    const value = self.readConstant(op == .ConstantLong);
                    try self.push(value);
                },
                .Nil => try self.push(Value.nil()),
                .True => try self.push(Value.fromBool(true)),
                .False => try self.push(Value.fromBool(false)),
                .Pop => _ = self.pop(),
                .GetLocal, .GetLocalLong => {
                    const slot = self.readBytes(op == .GetLocalLong);
                    try self.push(self.currentFrame().slots[slot]);
                },
                .SetLocal, .SetLocalLong => {
                    const slot = self.readBytes(op == .SetLocalLong);
                    self.currentFrame().slots[slot] = self.peek(0);
                },
                .GetGlobal, .GetGlobalLong => {
                    const name = self.readString(op == .GetGlobalLong);
                    if (self.globals.get(name)) |value| {
                        try self.push(value);
                    } else {
                        self.runtimeError("Undefined variable '{s}'.", .{name.buffer});
                        return error.RuntimeError;
                    }
                },
                .DefineGlobal, .DefineGlobalLong => {
                    const name = self.readString(op == .DefineGlobalLong);
                    _ = try self.globals.set(name, self.peek(0));
                    _ = self.pop();
                },
                .SetGlobal, .SetGlobalLong => {
                    const name = self.readString(op == .SetGlobalLong);
                    if (try self.globals.set(name, self.peek(0))) {
                        _ = self.globals.delete(name);
                        self.runtimeError("Undefined variable '{s}'.", .{name.buffer});
                        return error.RuntimeError;
                    }
                },
                .Equal => try self.push(Value.fromBool(self.pop().equals(self.pop()))),
                .Greater => try self.binary(Value.fromBool, gt),
                .Less => try self.binary(Value.fromBool, lt),
                .Add => {
                    const a = self.peek(1);
                    const b = self.peek(0);
                    if (a.isObj() and b.isObj() and
                        a.asObj().isString() and b.asObj().isString())
                    {
                        try self.concat();
                    } else if (a.isNumber() and b.isNumber()) {
                        _ = self.pop();
                        _ = self.pop();
                        try self.push(Value.fromNumber(a.asNumber() + b.asNumber()));
                    } else {
                        self.runtimeError("Operands must both be numbers or both be strings.", .{});
                        return error.RuntimeError;
                    }
                },
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
                .Print => {
                    std.debug.print("{f}\n", .{self.pop()});
                },
                .Jump => {
                    const offset = self.readBytes(true);
                    self.currentFrame().ip += offset;
                },
                .JumpIfFalse => {
                    const offset = self.readBytes(true);
                    if (self.peek(0).isFalsey()) self.currentFrame().ip += offset;
                },
                .Loop => {
                    const offset = self.readBytes(true);
                    self.currentFrame().ip -= offset;
                },
                .Call => {
                    const count = self.readByte();
                    try self.callValue(self.peek(count), count);
                },
                .Return => {
                    const result = self.pop();
                    const frame = self.frames.pop().?;
                    if (self.frames.items.len == 0) {
                        _ = self.pop();
                        return;
                    }

                    self.stack.items.len = frame.slots.ptr - self.stack.items.ptr;
                    try self.push(result);
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

    fn concat(self: *VM) !void {
        const b = self.pop().asObj().asString();
        const a = self.pop().asObj().asString();

        const buffer = try std.mem.concat(self.allocator, u8, &.{ a.buffer, b.buffer });
        const string = try Obj.String.take(self, buffer);
        try self.push(string.obj.toValue());
    }

    fn callValue(self: *VM, callee: Value, argCount: u8) !void {
        if (callee.isObj()) {
            switch (callee.asObj().type) {
                .Function => return self.call(callee.asObj().asFunction(), argCount),
                else => {},
            }
        }
        self.runtimeError("Can only call functions and classes.", .{});
        return error.RuntimeError;
    }

    fn call(self: *VM, callee: *Obj.Function, count: u8) !void {
        if (count != callee.arity) {
            self.runtimeError("Expected {d} arguments but got {d}.", .{ callee.arity, count });
            return error.RuntimeError;
        }

        // TODO: maybe add a stack limit or something.
        // it's not necessary now since our stack is an arraylist

        const frame = CallFrame{
            .function = callee,
            .ip = 0,
            .slots = self.stack.items[self.stack.items.len - @as(usize, @intCast(count)) - 1 ..],
        };
        try self.frames.append(self.allocator, frame);
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

    fn readString(self: *VM, long: bool) *Obj.String {
        return self.readConstant(long).asObj().asString();
    }

    fn readConstant(self: *VM, long: bool) Value {
        return self.currentChunk().constants.items[self.readBytes(long)];
    }

    fn readBytes(self: *VM, long: bool) usize {
        return if (long)
            (@as(usize, self.readByte()) << 16) +
                (@as(usize, self.readByte()) << 8) +
                self.readByte()
        else
            self.readByte();
    }

    fn readByte(self: *VM) u8 {
        defer self.currentFrame().ip += 1;
        return self.currentChunk().code.items[self.currentFrame().ip];
    }

    fn runtimeError(self: *VM, comptime fmt: []const u8, args: anytype) void {
        std.debug.print(fmt, args);
        std.debug.print("\n", .{});

        var i = self.frames.items.len;
        while (i > 0) {
            i -= 1;
            const frame = &self.frames.items[i];
            const function = frame.function;
            const line = function.chunk.getLine(frame.ip - 1);
            std.debug.print("[line {d}] in ", .{line});
            if (function.name) |name| {
                std.debug.print("{s}()\n", .{name.buffer});
            } else {
                std.debug.print("script\n", .{});
            }
        }

        self.resetStack();
    }

    fn currentFrame(self: *VM) *CallFrame {
        return &self.frames.items[self.frames.items.len - 1];
    }

    fn currentChunk(self: *VM) *Chunk {
        return &self.currentFrame().function.chunk;
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
