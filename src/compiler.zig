const std = @import("std");

const DEBUG_SHOW_TOKENS = @import("debug.zig").DEBUG_SHOW_TOKENS;
const DEBUG_PRINT_CODE = @import("debug.zig").DEBUG_PRINT_CODE;

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("scanner.zig").Token;
const TokenType = @import("scanner.zig").TokenType;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

const Precedence = enum(u8) {
    None,
    Assignment, // =
    Or, // or
    And, // and
    Equality, // == !=
    Comparison, // < > <= >=
    Term, // + -
    Factor, // * /
    Unary, // ! -
    Call, // . ()
    Primary,

    pub fn next(self: Precedence) Precedence {
        return @as(Precedence, @enumFromInt(@intFromEnum(self) + 1));
    }
};

pub fn compile(allocator: std.mem.Allocator, source: []const u8) !Chunk {
    var chunk = try Chunk.init(allocator);

    var parser = Parser.init(allocator, source, &chunk);
    parser.advance();
    try parser.expression();
    parser.consume(TokenType.Eof, "Expected end of expression.");
    try parser.end();

    if (parser.hadError) {
        chunk.deinit(allocator);
        return error.CompilerError;
    }
    return chunk;
}

fn showTokens(scanner: *Scanner) void {
    var line: usize = 0;

    while (true) {
        const token = scanner.scanToken();
        if (token.line != line) {
            std.debug.print("{d:4} ", .{line});
            line = token.line;
        } else {
            std.debug.print("   | ", .{});
        }
        std.debug.print("{d:2} '{s}'\n", .{ @intFromEnum(token.type), token.lexeme });

        if (token.type == .Eof) {
            std.debug.print("\n", .{});
            break;
        }
    }

    scanner.reset();
}

const Parser = struct {
    allocator: std.mem.Allocator,
    scanner: Scanner,
    source: []const u8,
    current: Token,
    previous: Token,
    hadError: bool,
    panicMode: bool,
    compilingChunk: *Chunk,

    pub fn init(allocator: std.mem.Allocator, source: []const u8, chunk: *Chunk) Parser {
        var scanner = Scanner.init(source);
        if (DEBUG_SHOW_TOKENS) {
            showTokens(&scanner);
        }

        return Parser{
            .allocator = allocator,
            .scanner = scanner,
            .source = source,
            .current = undefined,
            .previous = undefined,
            .hadError = false,
            .panicMode = false,
            .compilingChunk = chunk,
        };
    }

    pub fn expression(self: *Parser) !void {
        try self.parsePrecedence(.Assignment);
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) !void {
        self.advance();

        const prefixRule = getRule(self.previous.type).prefix orelse {
            self.errorAtPrevious("Expected expression.");
            return;
        };
        try prefixRule(self);

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.type).precedence)) {
            self.advance();
            const infixRule = getRule(self.previous.type).infix.?;
            try infixRule(self);
        }
    }

    fn unary(self: *Parser) !void {
        const op = self.previous.type;

        try self.parsePrecedence(.Unary);

        switch (op) {
            .Minus => {
                try self.emitOp(.Negate);
            },
            else => unreachable,
        }
    }

    fn binary(self: *Parser) !void {
        const op = self.previous.type;
        const rule = getRule(op);
        try self.parsePrecedence(rule.precedence.next());

        return switch (op) {
            .Plus => self.emitOp(.Add),
            .Minus => self.emitOp(.Subtract),
            .Star => self.emitOp(.Multiply),
            .Slash => self.emitOp(.Divide),
            else => unreachable,
        };
    }

    fn grouping(self: *Parser) !void {
        try self.expression();
        self.consume(.RightParen, "Expected ')' after expression.");
    }

    fn number(self: *Parser) !void {
        const value = try std.fmt.parseFloat(f64, self.previous.lexeme);
        try self.emitConstant(Value{ .Number = value });
    }

    pub fn consume(self: *Parser, tokenType: TokenType, message: []const u8) void {
        if (self.current.type == tokenType) {
            self.advance();
        } else {
            self.errorAtCurrent(message);
        }
    }

    pub fn advance(self: *Parser) void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();
            if (self.current.type != .Error) {
                break;
            }

            self.errorAtCurrent(self.current.lexeme);
        }
    }

    pub fn end(self: *Parser) !void {
        try self.emitReturn();

        if (DEBUG_PRINT_CODE) {
            if (!self.hadError) {
                self.currentChunk().disassemble("code");
                std.debug.print("\n", .{});
            }
        }
    }

    fn emitConstant(self: *Parser, value: Value) !void {
        try self.currentChunk().writeConstant(self.allocator, value, self.previous.line);
    }

    fn emitUnaryOp(self: *Parser, op: OpCode, byte: u8) !void {
        try self.emitOp(op);
        try self.emitByte(byte);
    }

    fn emitReturn(self: *Parser) !void {
        try self.emitOp(OpCode.Return);
    }

    fn emitOp(self: *Parser, op: OpCode) !void {
        try self.emitByte(@intFromEnum(op));
    }

    fn emitByte(self: *Parser, byte: u8) !void {
        try self.currentChunk().write(self.allocator, byte, self.previous.line);
    }

    fn currentChunk(self: *Parser) *Chunk {
        return self.compilingChunk;
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) void {
        self.errorAt(self.current, message);
    }

    fn errorAtPrevious(self: *Parser, message: []const u8) void {
        self.errorAt(self.previous, message);
    }

    fn errorAt(self: *Parser, token: Token, message: []const u8) void {
        if (self.panicMode) return;
        self.panicMode = true;

        std.debug.print("[line {d}] Error", .{token.line});

        if (token.type == .Eof) {
            std.debug.print(" at end", .{});
        } else if (token.type != .Error) {
            std.debug.print(" at '{s}'", .{token.lexeme});
        }

        std.debug.print(": {s}\n", .{message});
        self.hadError = true;
    }

    fn getRule(op: TokenType) ParseRule {
        return switch (op) {
            .LeftParen => .{ .prefix = grouping, .infix = null, .precedence = .None },
            .Minus => .{ .prefix = unary, .infix = binary, .precedence = .Term },
            .Plus => .{ .prefix = null, .infix = binary, .precedence = .Term },
            .Slash => .{ .prefix = null, .infix = binary, .precedence = .Factor },
            .Star => .{ .prefix = null, .infix = binary, .precedence = .Factor },
            .Number => .{ .prefix = number, .infix = null, .precedence = .None },
            else => .{ .prefix = null, .infix = null, .precedence = .None },
        };
    }
};

const ParseRule = struct {
    prefix: ?*const fn (self: *Parser) anyerror!void,
    infix: ?*const fn (self: *Parser) anyerror!void,
    precedence: Precedence,
};
