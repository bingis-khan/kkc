const std = @import("std");
const token = @import("token.zig");
const TokenType = token.TokenType;
const Token = token.Token;

pub fn parse() !void {
    std.debug.print("in parser\n", .{});

    while (!check(.EOF)) {
        toplevel() catch {
            sync_to_next_toplevel();
            return;
        };
    }
}

fn toplevel() !void {
    if (consume(.IDENTIFIER)) |id| {
        _ = id;

        // fn
        if (consume(.LEFT_PAREN)) |_| {
            _ = try expect(.RIGHT_PAREN);
            _ = undefined;
        }

        // constant
        else if (consume(.EQUALS)) |_| {
            _ = undefined;
        } else {
            try err("Expect function or constant definition, but got %");
        }
    }
}

fn expect(tt: TokenType) !Token {
    _ = tt;
    return undefined;
}

fn consume(tt: TokenType) ?Token {
    _ = tt;
    return undefined;
}

fn check(tt: TokenType) bool {
    _ = tt;
    return undefined;
}

fn err(comptime s: []const u8) !void {
    _ = s;
    return error.ParseError;
}

// this might be in the tokenizer.
fn sync_to_next_toplevel() void {}

const ParseError = error{ParseError};
