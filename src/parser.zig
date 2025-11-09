const std = @import("std");
const token = @import("token.zig");
const TokenType = token.TokenType;
const Token = token.Token;
const Lexer = @import("lexer.zig").Lexer;
const AST = @import("ast.zig");
const Common = @import("common.zig");
const Str = Common.Str;
const Loc = Common.Location;
const UniqueGen = @import("UniqueGen.zig");
const Unique = UniqueGen.Unique;
const Error = @import("error.zig").Error;
const stack = @import("stack.zig");
const TypeContext = @import("TypeContext.zig");

const ModuleResult = struct {
    ast: AST,
    errors: Errors,
};

// fuck it. let's do it one pass.
arena: std.mem.Allocator,

errors: Errors,

// parser zone
lexer: Lexer,
currentToken: Token,

// resolver zone
gen: struct {
    vars: UniqueGen,
    // cons: UniqueGen,
    // mems: UniqueGen,
    // classes: UniqueGen,
    // instances: UniqueGen,
},
scope: Scope,

// type zone
typeContext: TypeContext,

const Self = @This();
pub fn init(l: Lexer, arena: std.mem.Allocator) Self {
    var parser = Self{
        .arena = arena,
        .errors = Errors.init(arena), // TODO: use GPA

        // parser
        .lexer = l,
        .currentToken = undefined,

        // resolver
        .scope = Scope.init(arena), // TODO: use GPA
        .gen = .{
            .vars = UniqueGen.init(),
        },

        // typeshit
        .typeContext = TypeContext.init(arena),
    };

    parser.currentToken = parser.lexer.nextToken();
    return parser;
}

pub fn parse(self: *Self) !ModuleResult {
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

    return .{
        .ast = AST{ .declarations = decs.items },
        .errors = self.errors,
    };
}

fn toplevel(self: *Self) !AST.Declaration {
    if (self.consume(.IDENTIFIER)) |id| {
        // fn
        if (self.consume(.LEFT_PAREN)) |_| {
            const fnv = try self.newVar(id);
            var params = std.ArrayList(AST.Function.Param).init(self.arena);
            if (!self.check(.RIGHT_PAREN)) {
                while (true) {
                    const pnt = try self.expect(.IDENTIFIER);
                    const pt = try self.mtyp();
                    const v = try self.newVar(pnt);
                    try params.append(.{
                        .pn = v,
                        .pt = pt,
                    });

                    if (!self.check(.COMMA)) break;
                }

                try self.devour(.RIGHT_PAREN);
            }

            const ret = try self.mtyp();
            const fnBody = try self.body();
            const fnd = AST.Function{
                .name = fnv,
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

    self.scope.beginScope();
    var stmts = std.ArrayList(*AST.Stmt).init(self.arena);
    while (!self.check(.DEDENT)) {
        const stmt = try self.statement();
        try stmts.append(stmt);
    }
    self.scope.endScope();

    return stmts.items;
}

fn statement(self: *Self) error{ ParseError, OutOfMemory }!*AST.Stmt {
    const stmtVal: AST.Stmt = b: {
        if (self.check(.RETURN)) {
            const expr = try self.expression();

            try self.endStmt();
            break :b .{ .Return = expr };
        } else if (self.consume(.IDENTIFIER)) |v| {
            // here, choose between identifier and call
            if (self.check(.EQUALS)) {
                const expr = try self.expression();

                try self.endStmt();

                break :b .{ .VarDec = .{
                    .varDef = try self.newVar(v),
                    .varValue = expr,
                } };
            } else if (self.check(.LEFT_PAREN)) {
                // parse call
                return self.err(*AST.Stmt, "todo.", .{});
            } else {
                return self.err(*AST.Stmt, "Expect statement.", .{});
            }
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

    self.consumeSeps();

    const stmt = try self.arena.create(AST.Stmt);
    stmt.* = stmtVal;

    return stmt;
}

// for single line statements that do not contain a body.
fn endStmt(self: *Self) !void {
    if (self.peek().type != .DEDENT) {
        try self.devour(.STMT_SEP);
    }
}

fn consumeSeps(self: *Self) void {
    while (self.check(.STMT_SEP)) {}
}

// jon blow my c0c :3
fn expression(self: *Self) !*AST.Expr {
    return self.precedenceExpression(0);
}

fn precedenceExpression(self: *Self, minPrec: u32) ParserError!*AST.Expr {
    var left = try self.term();

    while (true) {
        const node = try self.increasingPrecedenceExpression(left, minPrec);
        if (node == left) break;

        left = node;
    }

    return left;
}

fn increasingPrecedenceExpression(self: *Self, left: *AST.Expr, minPrec: u32) !*AST.Expr {
    const binop = getBinOp(self.peek()) orelse return left;
    const nextPrec = binOpPrecedence(binop);

    if (nextPrec <= minPrec) {
        return left;
    } else {
        self.skip(); // if accepted, consume

        if (binop == .Call) {
            var params = std.ArrayList(*AST.Expr).init(self.arena);
            if (!self.check(.RIGHT_PAREN)) {
                while (true) {
                    try params.append(try self.expression());
                    if (!self.check(.COMMA)) break;
                }

                try self.devour(.RIGHT_PAREN);
            }

            return self.allocExpr(.{
                .t = undefined,
                .e = .{ .Call = .{
                    .callee = left,
                    .args = params.items,
                } },
            });
        }

        const right = try self.precedenceExpression(nextPrec);

        return self.allocExpr(.{
            .t = undefined,
            .e = .{ .BinOp = .{ .op = binop, .l = left, .r = right } },
        });
    }
}

fn term(self: *Self) !*AST.Expr {
    // TODO: maybe make some function to automatically allocate memory when expr succeeds?
    if (self.consume(.IDENTIFIER)) |v| {
        return self.allocExpr(.{
            .t = undefined,
            .e = .{ .Var = try self.lookupVar(v) },
        });
    } else if (self.consume(.INTEGER)) |i| {
        return self.allocExpr(.{
            .t = undefined,
            .e = .{ .Int = std.fmt.parseInt(i64, i.literal(self.lexer.source), 10) catch unreachable },
        });
    } else if (self.check(.LEFT_PAREN)) {
        const expr = try self.expression();
        try self.devour(.RIGHT_PAREN);
        return expr;
    } else {
        return self.err(*AST.Expr, "Unexpected term ", .{});
    }
}

fn allocExpr(self: Self, ev: AST.Expr) error{OutOfMemory}!*AST.Expr {
    const e = try self.arena.create(AST.Expr);
    e.* = ev;
    return e;
}

fn getBinOp(tok: Token) ?AST.BinOp {
    return switch (tok.type) {
        .PLUS => .Plus,
        .TIMES => .Times,
        .LEFT_PAREN => .Call,
        else => null,
    };
}

// here and not in AST.BinOp, because precedence only matters for parsing.
fn binOpPrecedence(op: AST.BinOp) u32 {
    return switch (op) {
        // 0 means it won't be consumed, like a sentinel value.
        .Plus => 1,
        .Times => 2,

        .Call => 10,
        .RecordAccess => 10,
        else => unreachable,
    };
}

fn mtyp(self: *Self) !AST.Type {
    // temp
    if (self.consume(.TYPE)) |conT| {
        return try self.typeContext.newType(.{ .Con = .{
            .typename = conT.literal(self.lexer.source),
            .application = &.{},
        } });
    } else {
        return try self.typeContext.fresh();
    }
}

fn typ(self: *Self) !AST.Type {
    // temp
    const conT = try self.expect(.TYPE);
    return AST.Type{ .Con = .{
        .typename = conT.literal(self.lexer.source),
        .application = .{},
    } };
}

// resolver zone
const Scope = struct {
    al: std.mem.Allocator,
    scopes: stack.Fixed(CurrentScope, Common.MaxIndent),

    pub fn init(al: std.mem.Allocator) @This() {
        const Scopes = stack.Fixed(CurrentScope, Common.MaxIndent);
        var scopes = Scopes.init();
        const defaultScope = CurrentScope.init(al);
        scopes.push(defaultScope);
        return .{
            .al = al,
            .scopes = scopes,
        };
    }

    pub fn currentScope(self: *@This()) *CurrentScope {
        return self.scopes.topp();
    }

    pub fn beginScope(self: *@This()) void {
        self.scopes.push(CurrentScope.init(self.al));
    }

    pub fn endScope(self: *@This()) void {
        _ = self.scopes.pop();
    }
};
const CurrentScope = struct {
    const VarLookup = std.StringHashMap(AST.Var);
    vars: VarLookup,

    fn init(al: std.mem.Allocator) @This() {
        return .{
            .vars = VarLookup.init(al),
        };
    }
};

pub fn newVar(self: *Self, varTok: Token) !AST.Var {
    const varName = varTok.literal(self.lexer.source);
    const thisVar = AST.Var{ .name = varName, .uid = self.gen.vars.newUnique() };
    try self.scope.currentScope().vars.put(varName, thisVar);
    return thisVar;
}

pub fn lookupVar(self: *Self, varTok: Token) !AST.Var {
    const varName = varTok.literal(self.lexer.source);
    var lastVars = self.scope.scopes.iterateFromTop();
    while (lastVars.next()) |cursc| {
        if (cursc.vars.get(varName)) |v| {
            return v;
        }
    } else {
        const placeholderVar = AST.Var{
            .name = varName,
            .uid = self.gen.vars.newUnique(),
        };
        try self.errors.append(.{
            .UndefinedVariable = .{ .varname = placeholderVar, .loc = .{
                .from = varTok.from,
                .to = varTok.to,
                .source = self.lexer.source,
            } },
        });

        // return placeholder var after an error.
        return placeholderVar;
    }
}

// parser zone
fn expect(self: *Self, tt: TokenType) !Token {
    return self.consume(tt) orelse return self.err(Token, "Expect {}", .{tt});
}

fn devour(self: *Self, tt: TokenType) !void {
    _ = try self.expect(tt);
}

fn peek(self: *const Self) Token {
    return self.currentToken;
}

fn consume(self: *Self, tt: TokenType) ?Token {
    const tok = self.currentToken;
    if (tok.type == tt) {
        self.skip();
        return tok;
    } else {
        return null;
    }
}

fn check(self: *Self, tt: TokenType) bool {
    return self.consume(tt) != null;
}

fn skip(self: *Self) void {
    self.currentToken = self.lexer.nextToken();
}

fn err(self: *Self, comptime t: type, comptime fmt: []const u8, args: anytype) !t {
    std.debug.print(fmt ++ " at {}\n", args ++ .{self.currentToken});
    std.debug.print("{s}\n", .{self.lexer.source[self.currentToken.from -% 5 .. @min(self.lexer.source.len, self.currentToken.to +% 5)]});
    return error.ParseError;
}

// this might be in the tokenizer.
fn sync_to_next_toplevel() void {}

const ParseError = error{ParseError};
const ParserError = error{ ParseError, OutOfMemory }; // full error set when it cannot be inferred.

// stores user errors.
//   might this def put it somewhere else.
const Errors = std.ArrayList(Error);
