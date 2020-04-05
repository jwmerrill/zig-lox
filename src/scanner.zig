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
    tokenType: TokenType,
    lexeme: []const u8,
    line: usize,
};

fn isDigit(char: u8) bool {
    return '0' <= char and char <= '9';
}

fn isAlpha(char: u8) bool {
    return (('a' <= char and char <= 'z') or
        ('A' <= char and char <= 'Z') or
        char == '_');
}

pub const Scanner = struct {
    start: []const u8,
    current: usize,
    line: usize,

    pub fn init(source: []const u8) Scanner {
        return Scanner{
            .start = source,
            .current = 0,
            .line = 1,
        };
    }

    pub fn scanToken(self: *Scanner) Token {
        self.skipWhitespace();

        self.start = self.start[self.current..];
        self.current = 0;

        if (self.isAtEnd()) return self.makeToken(.Eof);

        const c = self.advance();

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
            '!' => self.makeToken(if (self.match('=')) TokenType.BangEqual else TokenType.Bang),
            '=' => self.makeToken(if (self.match('=')) TokenType.EqualEqual else TokenType.Equal),
            '<' => self.makeToken(if (self.match('=')) TokenType.LessEqual else TokenType.Less),
            '>' => self.makeToken(if (self.match('=')) TokenType.GreaterEqual else TokenType.Greater),
            '"' => self.scanString(),
            else => {
                if (isDigit(c)) return self.scanNumber();
                if (isAlpha(c)) return self.scanIdentifier();
                if (c == 0) return self.makeToken(.Eof);
                return self.makeError("Unexpected character.");
            },
        };
    }

    fn advance(self: *Scanner) u8 {
        const char = self.peek();
        self.current += 1;
        return char;
    }

    fn peek(self: *Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.start[self.current];
    }

    fn peekNext(self: *Scanner) u8 {
        if (self.current + 1 >= self.start.len) return 0;
        return self.start[self.current + 1];
    }

    fn match(self: *Scanner, char: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.peek() == char) return false;

        self.current += 1;

        return true;
    }

    fn isAtEnd(self: *Scanner) bool {
        return self.current >= self.start.len;
    }

    fn makeToken(self: *Scanner, tokenType: TokenType) Token {
        return Token{
            .tokenType = tokenType,
            .lexeme = self.start[0..self.current],
            .line = self.line,
        };
    }

    fn makeError(self: *Scanner, message: []const u8) Token {
        return Token{
            .tokenType = .Error,
            .lexeme = message,
            .line = self.line,
        };
    }

    fn scanString(self: *Scanner) Token {
        // TODO, what about escaped double quotes?
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.isAtEnd()) return self.makeError("Unterminated string.");

        // The closing quote
        _ = self.advance();
        return self.makeToken(.String);
    }

    fn scanNumber(self: *Scanner) Token {
        while (isDigit(self.peek())) _ = self.advance();

        // Look for a fractional part.
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            // Consume the "."
            _ = self.advance();

            while (isDigit(self.peek())) _ = self.advance();
        }

        return self.makeToken(.Number);
    }

    fn scanIdentifier(self: *Scanner) Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) _ = self.advance();

        return self.makeToken(self.identifierType());
    }

    fn identifierType(self: *Scanner) TokenType {
        return switch (self.peek()) {
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
            'f' => {
                return switch (self.peekNext()) {
                    'a' => self.checkKeyword(2, "lse", .False),
                    'o' => self.checkKeyword(2, "r", .For),
                    'u' => self.checkKeyword(2, "n", .Fun),
                    else => .Identifier,
                };
            },
            't' => {
                return switch (self.peekNext()) {
                    'h' => self.checkKeyword(2, "is", .This),
                    'r' => self.checkKeyword(2, "ue", .True),
                    else => .Identifier,
                };
            },
            else => .Identifier,
        };
    }

    fn checkKeyword(self: *Scanner, offset: usize, str: []const u8, tokenType: TokenType) TokenType {
        if (self.current != str.len + offset) return .Identifier;
        const sourceSlice = self.start[offset..self.current];
        std.debug.assert(sourceSlice.len == str.len);
        return if (std.mem.eql(u8, sourceSlice, str)) tokenType else .Identifier;
    }

    fn skipWhitespace(self: *Scanner) void {
        while (true) {
            switch (self.peek()) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        // A comment goes until the end of the line
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            _ = self.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }
};
