const token = @import("token.zig");
const Token = token.Token;
const TokenType = token.TokenType;
const std = @import("std");
const Common = @import("common.zig");
const Str = Common.Str;
const stack = @import("stack.zig");
const @"error" = @import("error.zig");
const Errors = @"error".Errors;
const Error = @"error".Error;

pub const Lexer = struct {
    source: Str,
    errors: ?*Errors,
    indentStack: IndentStack,
    currentIndex: usize,
    moduleName: Str,

    // number when no token on a line has been generated.
    // nil when it's already done something.
    lineBeginOffset: usize,
    line: usize,

    const IndentStack = stack.Fixed(usize, Common.MaxIndent);
    const Self = @This();

    pub fn init(src: Str, moduleName: Str, errors: ?*Errors) Self {
        var self = Self{
            .source = src,
            .currentIndex = 0,
            .indentStack = IndentStack.init(),
            .lineBeginOffset = undefined,
            .errors = errors,
            .moduleName = moduleName,
            .line = 1,
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
                return Token{
                    .type = .DEDENT,
                    .from = self.lineBeginOffset,
                    .to = self.currentIndex,
                    .line = self.line,
                };
            }

            return Token{
                .type = .EOF,
                .from = self.currentIndex,
                .to = self.currentIndex,
                .line = self.line,
            };
        }

        // MID QUICK COPYPASTA
        {
            const off = self.currentIndex - self.lineBeginOffset;
            const currentIndent = self.indentStack.top();
            if (off < currentIndent) {
                // SLIGHT copypasta!
                if (off <= self.indentStack.peek()) {
                    _ = self.indentStack.pop();
                    return Token{
                        .type = .DEDENT,
                        .from = self.lineBeginOffset,
                        .to = self.currentIndex,
                        .line = self.line,
                    };
                } else {
                    // TODO: REPORT
                    // NOTE: NOT REALLY, DON'T KNOW IF HERE IS BETTER OR AT THE END.
                }
            }
        }

        const from = self.currentIndex;

        // NOTE: currently, possible EOFs are unhadled. Fix dat.
        const nc = self.nextChar();
        const tt: TokenType = switch (nc) {
            '#' => b: {
                // annotation shit.
                // I wonder if it would be easier to parse annotations in LEXER? Might be a bit faster.
                if (self.curChar() == '[') {
                    _ = self.nextChar();
                    break :b .BEGIN_ANNOTATION;
                }
                self.skipLine();
                return self.nextToken();
            },
            ':' => .COLON,
            '\'' => b: {
                while (self.curChar() != '\'') {
                    if (self.curChar() == '\n') {
                        // TODO: ERROR.
                        unreachable;
                    }
                    _ = self.nextChar();
                }

                _ = self.nextChar(); // skip over last '
                break :b .STRING;
            },
            '\n' => .STMT_SEP,
            '(' => .LEFT_PAREN,
            ')' => .RIGHT_PAREN,
            '[' => .LEFT_SQBR,
            ']' => .RIGHT_SQBR,
            '{' => .LEFT_BRACE,
            '}' => .RIGHT_BRACE,
            '&' => .REF,
            '<' => b: {
                if (self.check('=')) {
                    break :b .LTEQ;
                }
                break :b .LT;
            },
            '>' => b: {
                if (self.check('=')) {
                    break :b .GTEQ;
                }
                break :b .GT;
            },
            ',' => .COMMA,
            '.' => .DOT,
            '=' => b: {
                if (self.check('=')) {
                    break :b .EQEQ;
                }

                break :b .EQUALS;
            },
            '+' => .PLUS,
            '-' => b: {
                if (self.check('>')) {
                    break :b .RIGHT_ARROW;
                }

                break :b .MINUS;
            },
            '*' => .TIMES,
            '/' => b: {
                if (self.check('=')) {
                    break :b .NOTEQ;
                }
                break :b .SLASH;
            },
            '@' => b: {
                self.identifier();
                break :b .INTRINSIC;
            },

            'a'...'z' => b: {
                self.identifier();

                // from https://www.openmymind.net/Switching-On-Strings-In-Zig/
                const Keyword = enum {
                    @"return",
                    @"break",
                    @"if",
                    elif,
                    @"else",
                    class,
                    inst,
                    external,
                    case,
                    @"while",
                    @"fn",
                    use,
                    pass,
                    as,
                    @"or",
                    @"and",
                    not,
                };
                const keyword = std.meta.stringToEnum(Keyword, self.scanned(from)) orelse {
                    break :b .IDENTIFIER;
                };

                break :b switch (keyword) {
                    .@"return" => .RETURN,
                    .@"break" => .BREAK,
                    .@"if" => .IF,
                    .elif => .ELIF,
                    .@"else" => .ELSE,
                    .class => .CLASS,
                    .inst => .INST,
                    .external => .EXTERNAL,
                    .case => .CASE,
                    .@"while" => .WHILE,
                    .@"fn" => .FN,
                    .use => .USE,
                    .pass => .PASS,
                    .as => .AS,
                    .@"or" => .OR,
                    .@"and" => .AND,
                    .not => .NOT,
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

            '_' => .UNDERSCORE,
            else => b: {
                std.debug.print("Weird ass char '{}' at {s}\n", .{ nc, self.source[self.currentIndex - 5 .. self.currentIndex + 5] });
                break :b .ERROR;
            },
        };
        const to = self.currentIndex;

        self.skipWhitespace();

        if (tt == .STMT_SEP) {
            self.line += 1;
            // continually skip whitespace and newlines until we get another non STMT_SEP. we have to update the line beginning.
            self.lineBeginOffset = to;
            while (!self.isAtEnd() and self.curChar() == '\n') {
                _ = self.nextChar();
                self.line += 1;
                self.lineBeginOffset = self.currentIndex;
                self.skipWhitespace();
            }

            const off = self.currentIndex - self.lineBeginOffset;
            const currentIndent = self.indentStack.top();
            if (off > currentIndent) {
                self.indentStack.push(off);
                return Token{
                    .type = .INDENT,
                    .from = to,
                    .to = self.currentIndex,
                    .line = self.line,
                };
            } else if (off < currentIndent) {
                // check if the indent is incorrect
                // eg.
                // if ...
                //    stmt1
                //   stmt2  <- report error for this one
                if (off <= self.indentStack.peek()) {
                    _ = self.indentStack.pop();
                    return Token{
                        .type = .DEDENT,
                        .from = to,
                        .to = self.currentIndex,
                        .line = self.line,
                    };
                } else {
                    self.reportError(.{ .IncorrectIndent = .{} });
                }
            }
        }

        return Token{
            .type = tt,
            .from = from,
            .to = to,
            .line = self.line,
        };
    }

    fn scanned(self: Self, from: usize) Str {
        return self.source[from..self.currentIndex];
    }

    fn identifier(self: *Self) void {
        while (switch (self.curChar()) {
            'a'...'z', 'A'...'Z', '0'...'9', '\'' => true,
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

    fn check(self: *Self, c: u8) bool {
        if (self.curChar() == c) {
            _ = self.nextChar();
            return true;
        }

        return false;
    }

    fn curChar(self: *const Self) u8 {
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
        while (!self.isAtEnd()) {
            switch (self.curChar()) {
                ' ', '\t' => {
                    self.currentIndex += 1;
                },
                '#' => {
                    if (self.peekChar() == '[') {
                        return;
                    }
                    self.skipLine();
                },
                else => return,
            }
        }
    }

    fn skipLine(self: *Self) void {
        while (!self.isAtEnd() and self.curChar() != '\n') {
            _ = self.nextChar();
        }
    }

    fn reportError(self: *const Self, err: Error) void {
        if (self.errors) |errors| {
            errors.append(.{ .err = err, .module = .{
                .name = self.moduleName,
                .source = self.source,
            } }) catch {
                // I LITERALLY DON'T CARE OMGGGGGG.
                // ITS NOT EVEN THAT IMPORTANT.
                std.debug.print("LEXER: FAILED TO APPEND ERROR, BUT IDC\n", .{});
            };
        }
    }
};
