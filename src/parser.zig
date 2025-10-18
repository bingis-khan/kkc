const std = @import("std");
const token = @import("token.zig");
const TokenType = token.TokenType;
const Token = token.Token;
const Lexer = @import("lexer.zig").Lexer;

pub const Parser = struct {
    lexer: Lexer,
    currentToken: Token,

    const Self = @This();
    pub fn init(l: Lexer) Self {
        var parser = Self{
            .lexer = l,
            .currentToken = undefined,
        };

        parser.currentToken = parser.lexer.nextToken();
        return parser;
    }

    pub fn parse(self: *Self) !void {
        std.debug.print("in parser\n", .{});

        while (self.consume(.EOF) == null) {
            self.toplevel() catch {
                std.debug.print("Err.\n", .{});
                sync_to_next_toplevel();
                return;
            };
        }

        std.debug.print("parsing success\n", .{});
    }

    fn toplevel(self: *Self) !void {
        if (self.consume(.IDENTIFIER)) |id| {
            _ = id;

            // fn
            if (self.consume(.LEFT_PAREN)) |_| {
                while (self.consume(.RIGHT_PAREN) == null) {
                    try self.expect(.IDENTIFIER);
                    if (self.consume(.TYPE)) |_| {}
                }

                try self.body();
            }

            // constant
            else if (self.consume(.EQUALS)) |_| {
                _ = undefined;
            } else {
                return self.err(void, "Expect function or constant definition", .{});
            }
        } else {
            return self.err(void, "Unexpected definition", .{});
        }

        // consume statement separators
        while (self.consume(.STMT_SEP) != null) {}
    }

    fn body(self: *Self) !void {
        try self.expect(.INDENT);

        while (self.consume(.DEDENT) == null) {
            try self.statement();
        }
    }

    fn statement(self: *Self) ParseError!void {
        if (self.consume(.RETURN)) |_| {
            try self.expression();
        } else if (self.consume(.IDENTIFIER)) |v| {
            _ = v;
            try self.expect(.EQUALS);
            try self.expression();
        } else if (self.consume(.IF)) |_| {
            try self.expression();
            try self.body();
        } else {
            return self.err(void, "Expect statement.", .{});
        }

        if (self.currentToken.type != .DEDENT) {
            try self.expect(.STMT_SEP);
        }
    }

    fn expression(self: *Self) !void {
        // placeholder!
        try self.expect(.IDENTIFIER);
    }

    fn expect(self: *Self, tt: TokenType) !void {
        _ = self.consume(tt) orelse return self.err(void, "Expect {}", .{tt});
    }

    fn consume(self: *Self, tt: TokenType) ?Token {
        const tok = self.currentToken;
        if (tok.type == tt) {
            self.currentToken = self.lexer.nextToken();
            return tok;
        } else {
            return null;
        }
    }

    fn err(self: *Self, comptime t: type, comptime fmt: []const u8, args: anytype) !t {
        std.debug.print(fmt ++ " at {}\n", args ++ .{self.currentToken});
        return error.ParseError;
    }

    // this might be in the tokenizer.
    fn sync_to_next_toplevel() void {}

    const ParseError = error{ParseError};
};
