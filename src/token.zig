pub const Token = struct {
    type: TokenType,
    from: usize,
    to: usize,

    const Self = @This();
    const Str = @import("common.zig").Str;
    pub fn literal(self: Self, src: Str) Str {
        return src[self.from..self.to];
    }
};

pub const TokenType = enum {
    LEFT_PAREN,
    RIGHT_PAREN,
    COMMA,

    RETURN,
    IF,
    ELIF,
    ELSE,

    IDENTIFIER,
    TYPE,
    INTEGER,

    EQUALS,
    PLUS,

    STMT_SEP,

    INDENT,
    DEDENT,

    EOF,
};
