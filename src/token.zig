const common = @import("common.zig");
const Str = common.Str;
pub const Token = struct {
    type: TokenType,
    from: usize,
    to: usize,

    const Self = @This();
    pub fn literal(self: Self, src: Str) Str {
        return src[self.from..self.to];
    }

    pub fn toLocation(self: Self, src: Str) common.Location {
        return .{ .from = self.from, .to = self.to, .source = src };
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
    TYPE,
    INTEGER,
    STRING,

    EQUALS,
    EQEQ,
    GT,
    LT,
    PLUS,
    MINUS,
    TIMES,

    REF, // &, both ref and deref
    DOT, // .member

    LEFT_BRACE,
    RIGHT_BRACE,

    RETURN,
    IF,
    ELIF,
    ELSE,

    WHILE,
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
};
