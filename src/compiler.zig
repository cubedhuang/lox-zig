const std = @import("std");

const DEBUG_SHOW_TOKENS = @import("debug.zig").DEBUG_SHOW_TOKENS;
const DEBUG_PRINT_CODE = @import("debug.zig").DEBUG_PRINT_CODE;

const ArrayList = std.ArrayList;

const Chunk = @import("chunk.zig").Chunk;
const Obj = @import("object.zig").Obj;
const OpCode = @import("chunk.zig").OpCode;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("scanner.zig").Token;
const TokenType = @import("scanner.zig").TokenType;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

pub fn compile(vm: *VM, source: []const u8) !Chunk {
    var chunk = try Chunk.init(vm.allocator);

    var parser = try Parser.init(vm, source, &chunk);
    defer parser.deinit();

    parser.advance();
    while (!parser.match(.Eof)) {
        try parser.declaration();
    }
    parser.consume(TokenType.Eof, "Expected end of expression.");
    try parser.end();

    if (parser.hadError) {
        chunk.deinit(vm.allocator);
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

const Compiler = struct {
    locals: ArrayList(Local),
    scopeDepth: usize,

    pub fn init(allocator: std.mem.Allocator) !Compiler {
        return Compiler{
            .locals = try ArrayList(Local).initCapacity(allocator, 0),
            .scopeDepth = 0,
        };
    }

    pub fn deinit(self: *Compiler, allocator: std.mem.Allocator) void {
        self.locals.deinit(allocator);
    }
};

const Local = struct {
    name: Token,
    depth: usize,

    pub const UNINITIALIZED = std.math.maxInt(usize);
};

const Parser = struct {
    vm: *VM,
    scanner: Scanner,
    source: []const u8,
    current: Token,
    previous: Token,
    hadError: bool,
    panicMode: bool,
    compilingChunk: *Chunk,
    compiler: Compiler,

    const Error = error{ OutOfMemory, InvalidCharacter };

    pub fn init(vm: *VM, source: []const u8, chunk: *Chunk) !Parser {
        var scanner = Scanner.init(source);
        if (DEBUG_SHOW_TOKENS) {
            showTokens(&scanner);
        }

        return Parser{
            .vm = vm,
            .scanner = scanner,
            .source = source,
            .current = undefined,
            .previous = undefined,
            .hadError = false,
            .panicMode = false,
            .compilingChunk = chunk,
            .compiler = try Compiler.init(vm.allocator),
        };
    }

    pub fn deinit(self: *Parser) void {
        self.compiler.deinit(self.vm.allocator);
    }

    pub fn declaration(self: *Parser) Error!void {
        if (self.match(.Var)) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }

        if (self.panicMode) {
            self.synchronize();
        }
    }

    fn varDeclaration(self: *Parser) !void {
        const global = try self.parseVariable("Expected variable name.");

        if (self.match(.Equal)) {
            try self.expression();
        } else {
            try self.emitOp(.Nil);
        }
        self.consume(.Semicolon, "Expected ';' after variable declaration");

        try self.defineVariable(global);
    }

    fn statement(self: *Parser) Error!void {
        if (self.match(.Print)) {
            try self.printStatement();
        } else if (self.match(.If)) {
            try self.ifStatement();
        } else if (self.match(.LeftBrace)) {
            self.beginScope();
            try self.block();
            try self.endScope();
        } else {
            try self.expressionStatement();
        }
    }

    fn printStatement(self: *Parser) !void {
        try self.expression();
        self.consume(.Semicolon, "Expected ';' after expression.");
        try self.emitOp(.Print);
    }

    fn ifStatement(self: *Parser) !void {
        self.consume(.LeftParen, "Expected '(' after 'if'.");
        try self.expression();
        self.consume(.RightParen, "Expected ')' after condition.");

        const thenJump = try self.emitJump(.JumpIfFalse);
        try self.emitOp(.Pop);
        try self.statement();
        const elseJump = try self.emitJump(.Jump);
        self.patchJump(thenJump);
        try self.emitOp(.Pop);
        if (self.match(.Else)) try self.statement();
        self.patchJump(elseJump);
    }

    fn block(self: *Parser) !void {
        while (!self.check(.RightBrace) and !self.check(.Eof)) {
            try self.declaration();
        }

        self.consume(.RightBrace, "Expected '}' after block.");
    }

    fn expressionStatement(self: *Parser) !void {
        try self.expression();
        self.consume(.Semicolon, "Expected ';' after expression.");
        try self.emitOp(.Pop);
    }

    fn expression(self: *Parser) Error!void {
        try self.parsePrecedence(.Assignment);
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) Error!void {
        self.advance();

        const canAssign = @intFromEnum(precedence) <= @intFromEnum(Precedence.Assignment);
        if (!try self.prefix(self.previous.type, canAssign)) {
            return;
        }

        while (@intFromEnum(precedence) <= @intFromEnum(getPrecedence(self.current.type))) {
            self.advance();
            if (!try self.infix(self.previous.type)) {
                return;
            }
        }

        if (canAssign and self.match(.Equal)) {
            self.err("Invalid assignment target");
        }
    }

    fn parseVariable(self: *Parser, errorMessage: []const u8) !usize {
        self.consume(.Identifier, errorMessage);

        try self.declareVariable();
        if (self.compiler.scopeDepth > 0) return 0;

        return self.identifierConstant(self.previous);
    }

    fn identifierConstant(self: *Parser, token: Token) !usize {
        return try self.currentChunk().addConstant(
            self.vm.allocator,
            (try Obj.String.copy(self.vm, token.lexeme)).obj.toValue(),
        );
    }

    fn resolveLocal(self: *Parser, compiler: Compiler, name: Token) ?usize {
        var i = compiler.locals.items.len;
        while (i > 0) {
            i -= 1;

            const local = compiler.locals.items[i];
            if (name.equals(local.name)) {
                if (local.depth == Local.UNINITIALIZED) {
                    self.err("Can't read local variable in its own initializer");
                }
                return i;
            }
        }

        return null;
    }

    fn declareVariable(self: *Parser) !void {
        if (self.compiler.scopeDepth == 0) return;

        const name = self.previous;
        var i = self.compiler.locals.items.len;
        while (i > 0) {
            i -= 1;

            const local = self.compiler.locals.items[i];
            if (local.depth != Local.UNINITIALIZED and local.depth < self.compiler.scopeDepth) {
                // only higher-scoped variables are before here
                break;
            }

            if (name.equals(local.name)) {
                self.err("Already a variable with this name in this scope.");
            }
        }

        try self.addLocal(name);
    }

    fn addLocal(self: *Parser, name: Token) !void {
        const local = Local{
            .name = name,
            .depth = Local.UNINITIALIZED,
        };
        try self.compiler.locals.append(self.vm.allocator, local);
    }

    fn defineVariable(self: *Parser, global: usize) !void {
        if (self.compiler.scopeDepth > 0) {
            self.markInitialized();
            return;
        }

        try self.emitOpWithConstant(
            .DefineGlobal,
            .DefineGlobalLong,
            global,
        );
    }

    fn markInitialized(self: *Parser) void {
        self.compiler.locals.items[self.compiler.locals.items.len - 1].depth =
            self.compiler.scopeDepth;
    }

    fn unary(self: *Parser) !void {
        const op = self.previous.type;

        try self.parsePrecedence(.Unary);

        return switch (op) {
            .Bang => self.emitOp(.Not),
            .Minus => self.emitOp(.Negate),
            else => unreachable,
        };
    }

    fn binary(self: *Parser) !void {
        const op = self.previous.type;
        try self.parsePrecedence(getPrecedence(op).next());

        return switch (op) {
            .BangEqual => self.emitOps(.Equal, .Not),
            .EqualEqual => self.emitOp(.Equal),
            .Greater => self.emitOp(.Greater),
            .GreaterEqual => self.emitOps(.Less, .Not),
            .Less => self.emitOp(.Less),
            .LessEqual => self.emitOps(.Greater, .Not),
            .Plus => self.emitOp(.Add),
            .Minus => self.emitOp(.Subtract),
            .Star => self.emitOp(.Multiply),
            .Slash => self.emitOp(.Divide),
            else => unreachable,
        };
    }

    fn literal(self: *Parser) !void {
        return switch (self.previous.type) {
            .Nil => self.emitOp(.Nil),
            .True => self.emitOp(.True),
            .False => self.emitOp(.False),
            else => unreachable,
        };
    }

    fn grouping(self: *Parser) !void {
        try self.expression();
        self.consume(.RightParen, "Expected ')' after expression.");
    }

    fn number(self: *Parser) !void {
        const value = try std.fmt.parseFloat(f64, self.previous.lexeme);
        try self.emitConstant(Value.fromNumber(value));
    }

    fn string(self: *Parser) !void {
        try self.emitConstant(try self.stringValue());
    }

    fn variable(self: *Parser, canAssign: bool) !void {
        try self.namedVariable(self.previous, canAssign);
    }

    fn namedVariable(self: *Parser, name: Token, canAssign: bool) !void {
        const arg, const setOp, const getOp = resolve: {
            if (self.resolveLocal(self.compiler, name)) |local| {
                break :resolve .{
                    local,
                    .{ OpCode.SetLocal, OpCode.SetLocalLong },
                    .{ OpCode.GetLocal, OpCode.GetLocalLong },
                };
            } else {
                break :resolve .{
                    try self.identifierConstant(name),
                    .{ OpCode.SetGlobal, OpCode.SetGlobalLong },
                    .{ OpCode.GetGlobal, OpCode.GetGlobalLong },
                };
            }
        };

        if (canAssign and self.match(.Equal)) {
            try self.expression();
            try self.emitOpWithConstant(setOp.@"0", setOp.@"1", arg);
        } else {
            try self.emitOpWithConstant(getOp.@"0", getOp.@"1", arg);
        }
    }

    fn stringValue(self: *Parser) !Value {
        const value = self.previous.lexeme[1 .. self.previous.lexeme.len - 1];
        return (try Obj.String.copy(self.vm, value)).obj.toValue();
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

    fn beginScope(self: *Parser) void {
        self.compiler.scopeDepth += 1;
    }

    fn endScope(self: *Parser) !void {
        self.compiler.scopeDepth -= 1;

        while (self.compiler.locals.items.len > 0 and
            self.compiler.locals.getLast().depth > self.compiler.scopeDepth)
        {
            try self.emitOp(.Pop);
            _ = self.compiler.locals.pop();
        }
    }

    fn synchronize(self: *Parser) void {
        self.panicMode = false;

        while (self.current.type != .Eof) {
            if (self.previous.type == .Semicolon) return;

            switch (self.current.type) {
                .Class,
                .Fun,
                .Var,
                .For,
                .If,
                .While,
                .Print,
                .Return,
                => return,
                else => {},
            }

            self.advance();
        }
    }

    pub fn match(self: *Parser, tokenType: TokenType) bool {
        if (!self.check(tokenType)) return false;
        self.advance();
        return true;
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

    pub fn check(self: *Parser, tokenType: TokenType) bool {
        return self.current.type == tokenType;
    }

    fn emitJump(self: *Parser, op: OpCode) !usize {
        try self.emitOp(op);
        inline for (0..3) |_| {
            try self.emitByte(0xFF);
        }
        return self.currentChunk().code.items.len - 3;
    }

    fn patchJump(self: *Parser, offset: usize) void {
        const jump = self.currentChunk().code.items.len - 3 - offset;

        if (jump > std.math.maxInt(u24)) {
            self.err("Too much code to jump over.");
        }

        self.currentChunk().code.items[offset] = @intCast(jump >> 16);
        self.currentChunk().code.items[offset + 1] = @intCast((jump >> 8) & 0xFF);
        self.currentChunk().code.items[offset + 2] = @intCast(jump & 0xFF);
    }

    fn emitOpWithConstant(self: *Parser, op: OpCode, opLong: OpCode, constant: usize) !void {
        try self.currentChunk().writeOpWithConstant(
            self.vm.allocator,
            op,
            opLong,
            constant,
            self.previous.line,
        );
    }

    fn emitConstant(self: *Parser, value: Value) !void {
        try self.currentChunk().writeConstant(self.vm.allocator, value, self.previous.line);
    }

    fn emitUnaryOp(self: *Parser, op: OpCode, byte: u8) !void {
        try self.emitOp(op);
        try self.emitByte(byte);
    }

    fn emitOps(self: *Parser, a: OpCode, b: OpCode) !void {
        try self.emitOp(a);
        try self.emitOp(b);
    }

    fn emitReturn(self: *Parser) !void {
        try self.emitOp(OpCode.Return);
    }

    fn emitOp(self: *Parser, op: OpCode) !void {
        try self.emitByte(@intFromEnum(op));
    }

    fn emitByte(self: *Parser, byte: u8) !void {
        try self.currentChunk().write(self.vm.allocator, byte, self.previous.line);
    }

    fn currentChunk(self: *Parser) *Chunk {
        return self.compilingChunk;
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) void {
        self.errorAt(self.current, message);
    }

    fn err(self: *Parser, message: []const u8) void {
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

    /// returns whether there was a success
    fn prefix(self: *Parser, op: TokenType, canAssign: bool) !bool {
        switch (op) {
            .LeftParen => try self.grouping(),
            .Minus => try self.unary(),
            .Bang => try self.unary(),
            .Identifier => try self.variable(canAssign),
            .String => try self.string(),
            .Number => try self.number(),
            .False => try self.literal(),
            .Nil => try self.literal(),
            .True => try self.literal(),
            else => {
                self.tokenError();
                return false;
            },
        }
        return true;
    }

    /// returns whether there was a success
    fn infix(self: *Parser, op: TokenType) !bool {
        switch (op) {
            .Minus => try self.binary(),
            .Plus => try self.binary(),
            .Slash => try self.binary(),
            .Star => try self.binary(),
            .BangEqual => try self.binary(),
            .EqualEqual => try self.binary(),
            .Greater => try self.binary(),
            .GreaterEqual => try self.binary(),
            .Less => try self.binary(),
            .LessEqual => try self.binary(),
            else => {
                self.tokenError();
                return false;
            },
        }
        return true;
    }

    fn tokenError(self: *Parser) void {
        self.err("Expected expression.");
    }
};

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

fn getPrecedence(op: TokenType) Precedence {
    return switch (op) {
        .LeftParen => .None,
        .RightParen => .None,
        .LeftBrace => .None,
        .RightBrace => .None,
        .Comma => .None,
        .Dot => .None,
        .Minus => .Term,
        .Plus => .Term,
        .Semicolon => .None,
        .Slash => .Factor,
        .Star => .Factor,
        .Bang => .None,
        .BangEqual => .Equality,
        .Equal => .None,
        .EqualEqual => .Equality,
        .Greater => .Comparison,
        .GreaterEqual => .Comparison,
        .Less => .Comparison,
        .LessEqual => .Comparison,
        .Identifier => .None,
        .String => .None,
        .Number => .None,
        .And => .None,
        .Class => .None,
        .Else => .None,
        .False => .None,
        .For => .None,
        .Fun => .None,
        .If => .None,
        .Nil => .None,
        .Or => .None,
        .Print => .None,
        .Return => .None,
        .Super => .None,
        .This => .None,
        .True => .None,
        .Var => .None,
        .While => .None,
        .Error => .None,
        .Eof => .None,
    };
}
