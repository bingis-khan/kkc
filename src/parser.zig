const std = @import("std");
const token = @import("token.zig");
const TokenType = token.TokenType;
const Token = token.Token;
const Lexer = @import("lexer.zig").Lexer;
const AST = @import("ast.zig");

pub const Parser = struct {
    lexer: Lexer,
    currentToken: Token,
    arena: std.mem.Allocator,

    const Self = @This();
    pub fn init(l: Lexer, arena: std.mem.Allocator) Self {
        var parser = Self{
            .lexer = l,
            .arena = arena,
            .currentToken = undefined,
        };

        parser.currentToken = parser.lexer.nextToken();
        return parser;
    }

    pub fn parse(self: *Self) !AST {
        std.debug.print("in parser\n", .{});

        var decs = std.ArrayList(AST.Declaration).init(self.arena);
        while (self.consume(.EOF) == null) {
            const dec = self.toplevel() catch {
                std.debug.print("Err.\n", .{});
                sync_to_next_toplevel();
                return error.ParseError;
            };

            // consume statement separators
            while (self.consume(.STMT_SEP) != null) {}

            try decs.append(dec);
        }

        std.debug.print("parsing success\n", .{});

        return AST{ .declarations = decs.items };
    }

    fn toplevel(self: *Self) !AST.Declaration {
        if (self.consume(.IDENTIFIER)) |id| {

            // fn
            if (self.consume(.LEFT_PAREN)) |_| {
                var params = std.ArrayList(AST.Function.Param).init(self.arena);
                while (self.consume(.RIGHT_PAREN) == null) {
                    const pnt = try self.expect(.IDENTIFIER);
                    const pt = try self.mtyp();
                    try params.append(.{
                        .pn = pnt.literal(self.lexer.source),
                        .pt = pt,
                    });
                }

                const ret = try self.mtyp();
                const fnBody = try self.body();
                const fnd = AST.Function{
                    .name = id.literal(self.lexer.source),
                    .params = params.items,
                    .ret = ret,
                    .body = fnBody,
                };

                const fndec = AST.Declaration{ .Function = fnd };

                return fndec;
            }

            // constant
            else if (self.check(.EQUALS)) {
                _ = undefined;
                return self.err(AST.Declaration, "todo: unimplemented", .{});
            } else {
                return self.err(AST.Declaration, "Expect function or constant definition", .{});
            }
        } else {
            return self.err(AST.Declaration, "Unexpected definition", .{});
        }
    }

    fn body(self: *Self) ![]*AST.Stmt {
        try self.devour(.INDENT);

        var stmts = std.ArrayList(*AST.Stmt).init(self.arena);
        while (!self.check(.DEDENT)) {
            const stmt = try self.statement();
            try stmts.append(stmt);
        }

        return stmts.items;
    }

    fn statement(self: *Self) error{ ParseError, OutOfMemory }!*AST.Stmt {
        const stmtVal: AST.Stmt = b: {
            if (self.check(.RETURN)) {
                const expr = try self.expression();
                break :b .{ .Return = expr };
            } else if (self.consume(.IDENTIFIER)) |v| {
                try self.devour(.EQUALS);
                const expr = try self.expression();
                break :b .{ .VarDec = .{
                    .varName = v.literal(self.lexer.source),
                    .varValue = expr,
                } };
            } else if (self.check(.IF)) {
                const cond = try self.expression();
                const bTrue = try self.body();

                var elifs = std.ArrayList(AST.Stmt.Elif).init(self.arena);
                while (self.check(.ELIF)) {
                    const elifCond = try self.expression();
                    const elifBody = try self.body();
                    try elifs.append(AST.Stmt.Elif{ .cond = elifCond, .body = elifBody });
                }

                const elseBody = if (self.check(.ELSE))
                    try self.body()
                else
                    null;

                // incomplete
                break :b .{ .If = .{
                    .cond = cond,
                    .bTrue = bTrue,
                    .bOthers = elifs.items,
                    .bElse = elseBody,
                } };
            } else {
                return self.err(*AST.Stmt, "Expect statement.", .{});
            }
        };

        if (self.currentToken.type != .DEDENT) {
            try self.devour(.STMT_SEP);
        }

        const stmt = try self.arena.create(AST.Stmt);
        stmt.* = stmtVal;

        return stmt;
    }

    fn expression(self: *Self) !*AST.Expr {
        // placeholder!
        const v = try self.expect(.IDENTIFIER);
        const e = try self.arena.create(AST.Expr);
        e.* = .{ .Var = v.literal(self.lexer.source) };
        return e;
    }

    fn mtyp(self: *Self) !?*AST.Type {
        // temp
        const con_t = self.consume(.TYPE) orelse return null;
        const t = try self.arena.create(AST.Type);
        t.* = AST.Type{ .Con = .{
            .typename = con_t.literal(self.lexer.source),
            .application = &.{},
        } };

        return t;
    }

    fn typ(self: *Self) !AST.Type {
        // temp
        const con_t = try self.expect(.TYPE);
        return AST.Type{ .Con = .{
            .typename = con_t.literal(self.lexer.source),
            .application = .{},
        } };
    }

    fn expect(self: *Self, tt: TokenType) !Token {
        return self.consume(tt) orelse return self.err(Token, "Expect {}", .{tt});
    }

    fn devour(self: *Self, tt: TokenType) !void {
        _ = try self.expect(tt);
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

    fn check(self: *Self, tt: TokenType) bool {
        return self.consume(tt) != null;
    }

    fn err(self: *Self, comptime t: type, comptime fmt: []const u8, args: anytype) !t {
        std.debug.print(fmt ++ " at {}\n", args ++ .{self.currentToken});
        return error.ParseError;
    }

    // this might be in the tokenizer.
    fn sync_to_next_toplevel() void {}

    const ParseError = error{ParseError};
};
