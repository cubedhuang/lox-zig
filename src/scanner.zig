const std = @import("std");

pub const TokenType = enum {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Error,
    Eof,
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: usize,

    pub fn equals(self: Token, other: Token) bool {
        return std.mem.eql(u8, self.lexeme, other.lexeme);
    }
};

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isAlpha(c: u8) bool {
    return c >= 'a' and c <= 'z' or
        c >= 'A' and c <= 'Z' or
        c == '_';
}

pub const Scanner = struct {
    source: []const u8,
    start: usize,
    current: usize,
    line: usize,

    pub fn init(source: []const u8) Scanner {
        return Scanner{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
        };
    }

    pub fn reset(self: *Scanner) void {
        self.start = 0;
        self.current = 0;
        self.line = 1;
    }

    pub fn scanToken(self: *Scanner) Token {
        self.skipWhitespace();
        self.start = self.current;

        if (self.isAtEnd()) {
            return self.makeToken(.Eof);
        }

        const c = self.peek();
        self.advance();
        return switch (c) {
            '(' => self.makeToken(.LeftParen),
            ')' => self.makeToken(.RightParen),
            '{' => self.makeToken(.LeftBrace),
            '}' => self.makeToken(.RightBrace),
            ';' => self.makeToken(.Semicolon),
            ',' => self.makeToken(.Comma),
            '.' => self.makeToken(.Dot),
            '-' => self.makeToken(.Minus),
            '+' => self.makeToken(.Plus),
            '/' => self.makeToken(.Slash),
            '*' => self.makeToken(.Star),
            '!' => self.makeToken(if (self.match('=')) .BangEqual else .Bang),
            '=' => self.makeToken(if (self.match('=')) .EqualEqual else .Equal),
            '<' => self.makeToken(if (self.match('=')) .LessEqual else .Less),
            '>' => self.makeToken(if (self.match('=')) .GreaterEqual else .Greater),
            '"' => self.makeString(),
            '0'...'9' => self.makeNumber(),
            else => {
                if (isAlpha(c)) return self.makeIdentifier();
                if (c == 0) return self.makeToken(.Eof);
                return self.makeError("Unexpected character");
            },
        };
    }

    fn makeToken(self: *Scanner, tokenType: TokenType) Token {
        return Token{
            .type = tokenType,
            .lexeme = self.source[self.start..self.current],
            .line = self.line,
        };
    }

    fn makeError(self: *Scanner, message: []const u8) Token {
        return Token{
            .type = .Error,
            .lexeme = message,
            .line = self.line,
        };
    }

    fn makeString(self: *Scanner) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            self.advance();
        }

        if (self.isAtEnd()) {
            return self.makeError("Unterminated string.");
        }

        // discard closing quote
        self.advance();
        return self.makeToken(.String);
    }

    fn makeNumber(self: *Scanner) Token {
        while (isDigit(self.peek())) {
            self.advance();
        }

        if (self.peek() == '.' and isDigit(self.peekNext())) {
            self.advance();

            while (isDigit(self.peek())) {
                self.advance();
            }
        }

        return self.makeToken(.Number);
    }

    fn makeIdentifier(self: *Scanner) Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) {
            self.advance();
        }

        return self.makeToken(self.identifierType());
    }

    fn identifierType(self: *Scanner) TokenType {
        return switch (self.source[self.start]) {
            'a' => self.checkKeyword(1, "nd", .And),
            'c' => self.checkKeyword(1, "lass", .Class),
            'e' => self.checkKeyword(1, "lse", .Else),
            'i' => self.checkKeyword(1, "f", .If),
            'n' => self.checkKeyword(1, "il", .Nil),
            'o' => self.checkKeyword(1, "r", .Or),
            'p' => self.checkKeyword(1, "rint", .Print),
            'r' => self.checkKeyword(1, "eturn", .Return),
            's' => self.checkKeyword(1, "uper", .Super),
            'v' => self.checkKeyword(1, "ar", .Var),
            'w' => self.checkKeyword(1, "hile", .While),
            'f' => if (self.current - self.start > 1)
                switch (self.source[self.start + 1]) {
                    'a' => self.checkKeyword(2, "lse", .False),
                    'o' => self.checkKeyword(2, "r", .For),
                    'u' => self.checkKeyword(1, "n", .Fun),
                    else => .Identifier,
                }
            else
                .Identifier,
            't' => if (self.current - self.start > 1)
                switch (self.source[self.start + 1]) {
                    'h' => self.checkKeyword(2, "is", .This),
                    'r' => self.checkKeyword(2, "ue", .True),
                    else => .Identifier,
                }
            else
                .Identifier,
            else => .Identifier,
        };
    }

    fn checkKeyword(
        self: *Scanner,
        start: usize,
        rest: []const u8,
        tokenType: TokenType,
    ) TokenType {
        return if (std.mem.eql(u8, self.source[self.start + start .. self.current], rest))
            tokenType
        else
            .Identifier;
    }

    fn skipWhitespace(self: *Scanner) void {
        while (true) {
            switch (self.peek()) {
                ' ', '\r', '\t' => self.advance(),
                '\n' => {
                    self.line += 1;
                    self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.peek() != expected) return false;
        self.current += 1;
        return true;
    }

    fn advance(self: *Scanner) void {
        self.current += 1;
    }

    fn peekNext(self: *Scanner) u8 {
        if (self.current >= self.source.len - 1) return 0;
        return self.source[self.current + 1];
    }

    fn peek(self: *Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    fn isAtEnd(self: *const Scanner) bool {
        return self.current >= self.source.len;
    }
};
