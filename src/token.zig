pub const Token = struct {
    type: TokenType,
    from: usize,
    to: usize,
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
