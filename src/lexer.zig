const token = @import("token.zig");
const Token = token.Token;
const TokenType = token.TokenType;
const std = @import("std");
pub const Lexer = struct {
    source: []u8,
    indentStack: IndentStack,

    currentIndex: usize,

    // number when no token on a line has been generated.
    // nil when it's already done something.
    lineBeginOffset: ?usize,

    const IndentStack = FixedStack(usize, 512);
    const Self = @This();

    pub fn init(src: []u8) Self {
        var self = Self{
            .source = src,
            .currentIndex = 0,
            .indentStack = IndentStack.init(),
            .lineBeginOffset = undefined,
        };
        self.indentStack.push(0);
        self.skipWhitespace();
        self.lineBeginOffset = self.currentIndex;
        return self;
    }

    pub fn nextToken(self: *Self) Token {
        if (self.isAtEnd()) {

            // remember to match dedents at the end.
            if (self.indentStack.top() > 0) {
                _ = self.indentStack.pop();
                return Token{ .type = .DEDENT, .from = self.lineBeginOffset orelse self.currentIndex, .to = self.currentIndex };
            }

            return Token{ .type = .EOF, .from = self.currentIndex, .to = self.currentIndex };
        }

        const from = self.currentIndex;
        const tt: TokenType = switch (self.nextChar()) {
            '\n' => .STMT_SEP,
            '(' => .LEFT_PAREN,
            ')' => .RIGHT_PAREN,
            ',' => .COMMA,
            '=' => .EQUALS,
            '+' => .PLUS,
            '*' => .TIMES,
            'a'...'z' => b: {
                self.identifier();

                // from https://www.openmymind.net/Switching-On-Strings-In-Zig/
                const Keyword = enum {
                    @"return",
                    @"if",
                    elif,
                    @"else",
                };
                const keyword = std.meta.stringToEnum(Keyword, self.scanned(from)) orelse {
                    break :b .IDENTIFIER;
                };

                break :b switch (keyword) {
                    .@"return" => .RETURN,
                    .@"if" => .IF,
                    .elif => .ELIF,
                    .@"else" => .ELSE,
                };
            },

            'A'...'Z' => b: {
                self.identifier();
                break :b .TYPE;
            },

            '0'...'9' => b: {
                self.integer();
                break :b .INTEGER;
            },
            else => .EOF,
        };
        const to = self.currentIndex;

        self.skipWhitespace();

        if (tt == .STMT_SEP) {
            // continually skip whitespace and newlines until we get another non STMT_SEP. we have to update the line beginning.
            var beginLine = to;
            while (!self.isAtEnd() and self.curChar() == '\n') {
                _ = self.nextChar();
                beginLine = self.currentIndex;
                self.skipWhitespace();
            }

            const off = self.currentIndex - beginLine;
            const currentIndent = self.indentStack.top();
            if (off > currentIndent) {
                self.indentStack.push(off);
                return Token{ .type = .INDENT, .from = to, .to = self.currentIndex };
            } else if (off < currentIndent) {
                _ = self.indentStack.pop();
                return Token{ .type = .DEDENT, .from = to, .to = self.currentIndex };
            }
        }

        return Token{ .type = tt, .from = from, .to = to };
    }

    fn scanned(self: Self, from: usize) []u8 {
        return self.source[from..self.currentIndex];
    }

    fn identifier(self: *Self) void {
        while (switch (self.curChar()) {
            'a'...'z', 'A'...'Z', '0'...'9' => true,
            '-' => std.ascii.isAlphanumeric(self.peekChar()), // a-b ok, a- b minus, etc.
            else => false,
        }) : (self.currentIndex += 1) {}
    }

    fn integer(self: *Self) void {
        while (std.ascii.isDigit(self.curChar())) : (self.currentIndex += 1) {}
    }

    fn nextChar(self: *Self) u8 {
        const c = self.source[self.currentIndex];
        self.currentIndex += 1;
        return c;
    }

    fn peekChar(self: Self) u8 {
        return if (self.currentIndex + 1 >= self.source.len)
            0
        else
            self.source[self.currentIndex + 1];
    }

    fn curChar(self: Self) u8 {
        return self.source[self.currentIndex];
    }

    pub fn finished(self: *const Self) bool {
        return self.isAtEnd() and self.indentStack.top() == 0;
    }

    // does not account for dedents
    fn isAtEnd(self: *const Self) bool {
        return self.currentIndex >= self.source.len;
    }

    fn skipWhitespace(self: *Self) void {
        while (!self.isAtEnd()) : (self.currentIndex += 1) {
            switch (self.curChar()) {
                ' ', '\t' => {},
                else => return,
            }
        }
    }
};

fn FixedStack(comptime t: type, comptime sz: usize) type {
    return struct {
        mem: [sz]t,
        current: usize,

        const Self = @This();

        fn init() Self {
            return .{
                .mem = undefined,
                .current = 0,
            };
        }

        // TODO: add overflow checking  and errors (but it should not happen when the code is correct, so I'm not sure)
        fn push(s: *Self, item: t) void {
            s.mem[s.current] = item;
            s.current += 1;
        }

        fn pop(s: *Self) t {
            s.current -= 1;
            return s.mem[s.current];
        }

        fn top(s: *const Self) t {
            return s.mem[s.current - 1];
        }
    };
}
