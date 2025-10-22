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

    STMT_SEP,

    INDENT,
    DEDENT,

    IDENTIFIER,
    TYPE,
    INTEGER,

    EQUALS,
    PLUS,
    TIMES,

    REF, // &, both ref and deref
    DOT, // .member

    LEFT_BRACE,
    RIGHT_BRACE,

    RETURN,
    IF,
    ELIF,
    ELSE,

    // class
    CLASS,
    INST,

    EOF,
};
