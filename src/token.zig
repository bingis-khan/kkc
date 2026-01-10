const common = @import("common.zig");
const Str = common.Str;
pub const Token = struct {
    type: TokenType,
    from: usize,
    to: usize,
    line: usize,

    const Self = @This();
    pub fn literal(self: Self, src: Str) Str {
        return src[self.from..self.to];
    }

    pub fn toLocation(self: Self, src: Str, moduleName: Str) common.Location {
        return .{
            .from = self.from,
            .to = self.to,
            .line = self.line,
            .module = .{
                .name = moduleName,
                .source = src,
            },
        };
    }

    pub fn isWhitespace(self: *const Self) bool {
        return switch (self.type) {
            .INDENT, .DEDENT, .STMT_SEP => true,
            else => false,
        };
    }
};

pub const TokenType = enum {
    LEFT_PAREN,
    RIGHT_PAREN,
    COMMA,
    COLON,
    RIGHT_ARROW,
    UNDERSCORE,

    STMT_SEP,

    INDENT,
    DEDENT,

    IDENTIFIER,
    INTRINSIC,
    TYPE,
    NUMTYNAME,

    INTEGER,
    STRING,
    PASS,

    EQUALS,
    EQEQ,
    NOTEQ,
    GT,
    GTEQ,
    LT,
    LTEQ,
    PLUS,
    MINUS,
    TIMES,
    SLASH,
    OR,
    AND,
    NOT,

    REF, // &, both ref and deref
    DOT, // .member

    LEFT_BRACE,
    RIGHT_BRACE,

    // keywords
    RETURN,
    IF,
    ELIF,
    ELSE,
    AS,
    BREAK,

    WHILE,
    FOR,
    IN,
    CASE,
    FN,
    USE,

    // class
    CLASS,
    INST,

    // other
    EXTERNAL,

    BEGIN_ANNOTATION,
    LEFT_SQBR,
    RIGHT_SQBR,

    EOF,

    ERROR,
    INCORRECT_INDENT, // should not cause parsing to fail! just report.
};
