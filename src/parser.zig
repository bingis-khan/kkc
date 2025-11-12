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
const @"error" = @import("error.zig");
const Error = @"error".Error;
const Errors = @"error".Errors;
const stack = @import("stack.zig");
const TypeContext = @import("TypeContext.zig");
const Prelude = @import("Prelude.zig");

const ModuleResult = struct {
    ast: AST,
    // errors: Errors,
};

// fuck it. let's do it one pass.
arena: std.mem.Allocator,

errors: *Errors,

// parser zone
lexer: Lexer,
currentToken: Token,

// resolver zone
gen: struct {
    vars: UniqueGen,
    types: UniqueGen,
    cons: UniqueGen,
    // mems: UniqueGen,
    // classes: UniqueGen,
    // instances: UniqueGen,
},
scope: Scope,

// type zone
typeContext: TypeContext,
prelude: ?Prelude,
returnType: ?AST.Type,

const Self = @This();
pub fn init(l: Lexer, errors: *Errors, arena: std.mem.Allocator) !Self {
    const context = try TypeContext.init(arena, errors);
    var parser = Self{
        .arena = arena,
        .errors = errors, // TODO: use GPA

        // parser
        .lexer = l,
        .currentToken = undefined,

        // resolver
        .scope = Scope.init(arena), // TODO: use GPA
        .gen = .{
            .vars = UniqueGen.init(),
            .types = UniqueGen.init(),
            .cons = UniqueGen.init(),
        },

        // typeshit
        .typeContext = context,
        .returnType = null,
        .prelude = null,
    };

    parser.currentToken = parser.lexer.nextToken();
    return parser;
}

pub fn parse(self: *Self) !ModuleResult {
    std.debug.print("in parser\n", .{});

    var decs = std.ArrayList(*AST.Stmt).init(self.arena);
    while (self.consume(.EOF) == null) {
        const dec = self.statement() catch |e| {
            std.debug.print("Err.\n", .{});
            sync_to_next_toplevel();
            return e;
        };

        // consume statement separators
        self.consumeSeps();

        if (dec != null) try decs.append(dec.?);
    }

    std.debug.print("parsing success\n", .{});

    return .{
        .ast = AST{ .toplevel = decs.items },
        // .errors = self.errors,
    };
}

fn dataDef(self: *Self, typename: Token) !void {
    if (!self.check(.INDENT)) {
        // Add type without any constructors
        try self.newData(try Common.allocOne(self.arena, AST.Data, .{
            .uid = self.gen.types.newUnique(),
            .name = typename.literal(self.lexer.source),
            .cons = &.{},
        }));
        return;
    }

    const data = try self.arena.create(AST.Data);
    data.uid = self.gen.types.newUnique();
    data.name = typename.literal(self.lexer.source);

    var cons = std.ArrayList(AST.Con).init(self.arena);
    while (!self.check(.DEDENT)) {
        const conName = try self.expect(.TYPE);
        var tys = std.ArrayList(AST.Type).init(self.arena);
        while (!(self.check(.STMT_SEP) or (self.peek().type == .DEDENT))) { // we must not consume the last DEDENT, as it's used to terminate the whole type declaration.
            // TODO: for now, no complicated types!
            const ty = try self.typ();
            try tys.append(ty);
        }
        self.consumeSeps();

        try cons.append(.{
            .uid = self.gen.cons.newUnique(),
            .name = conName.literal(self.lexer.source),
            .tys = tys.items,
            .data = data,
        });
    }

    data.cons = cons.items;

    try self.newData(data);
}

fn function(self: *Self, id: Token) !AST.Stmt {
    const fnv = try self.newVar(id);
    var params = std.ArrayList(AST.Function.Param).init(self.arena);
    if (!self.check(.RIGHT_PAREN)) {
        while (true) {
            const pnt = try self.expect(.IDENTIFIER);
            const pt = try self.mtyp();
            const v = try self.newVar(pnt);
            try self.typeContext.unify(v.t, pt);
            try params.append(.{
                .pn = v,
                .pt = pt,
            });

            if (!self.check(.COMMA)) break;
        }

        try self.devour(.RIGHT_PAREN);
    }

    const ret = try self.mtyp();

    // make type from function
    const paramTs = try self.arena.alloc(AST.Type, params.items.len);
    for (params.items, 0..) |et, i| {
        paramTs[i] = et.pt;
    }
    const fnType = try self.typeContext.newType(.{
        .Fun = .{ .args = paramTs, .ret = ret },
    });
    try self.typeContext.unify(fnv.t, fnType);

    // set return and parse body
    const oldReturnType = self.returnType;
    self.returnType = ret;
    const fnBody = try self.body();
    self.returnType = oldReturnType;

    const fnd = AST.Function{
        .name = fnv,
        .params = params.items,
        .ret = ret,
        .body = fnBody,
    };

    const fndec = AST.Stmt{ .Function = fnd };

    return fndec;
}

fn body(self: *Self) ![]*AST.Stmt {
    try self.devour(.INDENT);

    self.scope.beginScope();
    var stmts = std.ArrayList(*AST.Stmt).init(self.arena);
    while (!self.check(.DEDENT)) {
        const stmt = try self.statement();
        if (stmt) |s| {
            try stmts.append(s);
        }
    }
    self.scope.endScope();

    return stmts.items;
}

fn statement(self: *Self) ParserError!?*AST.Stmt {
    const stmtVal: ?AST.Stmt = b: {
        if (self.check(.RETURN)) {
            const expr = try self.expression();
            try self.typeContext.unify(expr.t, try self.getReturnType());

            try self.endStmt();
            break :b .{ .Return = expr };
        } else if (self.consume(.IDENTIFIER)) |v| {
            // here, choose between identifier and call
            if (self.check(.EQUALS)) {
                const expr = try self.expression();
                const vv = try self.newVar(v);
                try self.typeContext.unify(vv.t, expr.t);

                try self.endStmt();

                break :b .{ .VarDec = .{
                    .varDef = vv,
                    .varValue = expr,
                } };
            } else if (self.check(.LEFT_PAREN)) {
                // parse call
                // SIKE
                // right now, parse function
                break :b try self.function(v);
            } else {
                return self.err(*AST.Stmt, "Expect statement.", .{});
            }
        } else if (self.consume(.TYPE)) |typename| {
            try self.dataDef(typename);
            break :b null;
        } else if (self.check(.IF)) {
            const cond = try self.expression();
            try self.typeContext.unify(cond.t, try self.defined(.Bool));
            const bTrue = try self.body();

            var elifs = std.ArrayList(AST.Stmt.Elif).init(self.arena);
            while (self.check(.ELIF)) {
                const elifCond = try self.expression();
                try self.typeContext.unify(elifCond.t, try self.defined(.Bool));
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

        // (if we forget to return an expression, this should catch it)
        unreachable;
    };

    self.consumeSeps();

    if (stmtVal) |stmtValForSure| {
        const stmt = try self.arena.create(AST.Stmt);
        stmt.* = stmtValForSure;
        return stmt;
    }

    return null;
}

// for single line statements that do not contain a body.
// (why? in IF and such, a dedent is equivalent to STMTSEP, so we must accept both. it depends on the type of statement.)
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

            // how a function is represented.
            const paramTs = try self.arena.alloc(AST.Type, params.items.len);
            for (params.items, 0..) |et, i| {
                paramTs[i] = et.t;
            }

            const retType = try self.typeContext.fresh();

            const callType = try self.typeContext.newType(.{
                .Fun = .{ .args = paramTs, .ret = retType },
            });

            try self.typeContext.unify(callType, left.t);

            return self.allocExpr(.{
                .t = retType,
                .e = .{ .Call = .{
                    .callee = left,
                    .args = params.items,
                } },
            });
        }

        const right = try self.precedenceExpression(nextPrec);

        const exprType = switch (binop) {
            .Plus,
            .Minus,
            .Times,
            .Divide,
            .GreaterThan,
            .LessThan,
            .GreaterEqualThan,
            .LessEqualThan,
            => b: {
                const intTy = try self.defined(.Int);
                try self.typeContext.unify(left.t, intTy);
                try self.typeContext.unify(right.t, intTy);
                break :b intTy;
            },

            .Equals, .NotEquals => b: {
                try self.typeContext.unify(left.t, right.t);
                break :b try self.defined(.Bool);
            },

            else => unreachable,
        };

        return self.allocExpr(.{
            .t = exprType,
            .e = .{ .BinOp = .{ .op = binop, .l = left, .r = right } },
        });
    }
}

fn term(self: *Self) !*AST.Expr {
    // TODO: maybe make some function to automatically allocate memory when expr succeeds?
    if (self.consume(.IDENTIFIER)) |v| {
        const dv = try self.lookupVar(v);
        return self.allocExpr(.{
            .t = dv.t,
            .e = .{ .Var = dv },
        });
    } else if (self.consume(.TYPE)) |con| {
        const ct = try self.instantiateCon(con);
        return self.allocExpr(.{
            .e = .{ .Con = ct.con },
            .t = ct.t,
        });
    } else if (self.consume(.INTEGER)) |i| {
        return self.allocExpr(.{
            .t = try self.defined(.Int),
            .e = .{ .Int = std.fmt.parseInt(i64, i.literal(self.lexer.source), 10) catch unreachable },
        });
    } else if (self.check(.LEFT_PAREN)) {
        const expr = try self.expression();
        try self.devour(.RIGHT_PAREN);
        return expr;
    } else {
        return self.err(*AST.Expr, "Unexpected term", .{});
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
        .EQEQ => .Equals,
        .LEFT_PAREN => .Call,
        else => null,
    };
}

// here and not in AST.BinOp, because precedence only matters for parsing.
fn binOpPrecedence(op: AST.BinOp) u32 {
    return switch (op) {
        // 0 means it won't be consumed, like a sentinel value.
        .Equals => 1,
        .Plus => 2,
        .Times => 3,

        .Call => 10,
        .RecordAccess => 10,
        else => unreachable,
    };
}

fn mtyp(self: *Self) !AST.Type {
    // temp
    if (self.consume(.TYPE)) |conT| {
        return try self.typeContext.newType(.{ .Con = .{
            .type = try self.lookupData(conT),
            .application = &.{},
        } });
    } else {
        return try self.typeContext.fresh();
    }
}

fn typ(self: *Self) !AST.Type {
    // temp
    const conT = try self.expect(.TYPE);
    return try self.typeContext.newType(.{ .Con = .{
        .type = try self.lookupData(conT),
        .application = &.{},
    } });
}

// resolver zone
// VARS
fn newVar(self: *@This(), varTok: Token) !AST.Var {
    const varName = varTok.literal(self.lexer.source);
    const thisVar = AST.Var{
        .name = varName,
        .uid = self.gen.vars.newUnique(),
        .t = try self.typeContext.fresh(),
    };
    try self.scope.currentScope().vars.put(varName, thisVar);
    return thisVar;
}

fn lookupVar(self: *@This(), varTok: Token) !AST.Var {
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
            .t = try self.typeContext.fresh(),
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

// TYPES
// (requires US to generate a new unique.)
fn newData(self: *@This(), data: *AST.Data) !void {
    // add type
    try self.scope.currentScope().types.put(data.name, data);

    // add constructors
    for (data.cons) |*con| {
        try self.scope.currentScope().cons.put(con.name, con);
    }
}

fn lookupData(self: *Self, tyTok: Token) !*AST.Data {
    const typename = tyTok.literal(self.lexer.source);
    if (self.maybeLookupType(typename)) |ty| {
        return ty;
    } else {
        const placeholderType = try Common.allocOne(self.arena, AST.Data, .{
            .name = typename,
            .uid = self.gen.vars.newUnique(),
            .cons = &.{},
        });
        const location = Common.Location{
            .from = tyTok.from,
            .to = tyTok.to,
            .source = self.lexer.source,
        };
        try self.errors.append(.{
            .UndefinedType = .{ .typename = typename, .loc = location },
        });

        return placeholderType;
    }
}

fn maybeLookupType(self: *Self, typename: Str) ?*AST.Data {
    var lastScopes = self.scope.scopes.iterateFromTop();
    while (lastScopes.next()) |cursc| {
        if (cursc.types.get(typename)) |t| {
            return t;
        }
    } else {
        return null;
    }
}

// CONS
fn newCon(self: *@This(), con: *AST.Con) !void {
    try self.scope.currentScope().cons.put(con.name, con);
}

fn instantiateCon(self: *@This(), conTok: Token) !struct { con: *AST.Con, t: AST.Type } {
    const conName = conTok.literal(self.lexer.source);
    var lastVars = self.scope.scopes.iterateFromTop();
    while (lastVars.next()) |cursc| {
        if (cursc.cons.get(conName)) |con| {
            // found con. now we instantiate it.
            if (con.tys.len == 0) {
                return .{ .con = con, .t = try self.typeContext.newType(.{ .Con = .{ .type = con.data, .application = &.{} } }) };
            } else {
                var args = std.ArrayList(AST.Type).init(self.arena);
                for (con.tys) |ty| {
                    try args.append(ty);
                }
                return .{ .con = con, .t = try self.typeContext.newType(.{ .Fun = .{
                    .args = args.items,
                    .ret = try self.typeContext.newType(.{ .Con = .{
                        .type = con.data,
                        .application = &.{},
                    } }),
                } }) };
            }
        }
    } else {
        const data = try self.arena.create(AST.Data);
        data.uid = self.gen.types.newUnique();
        data.name = conName;
        data.cons = try self.arena.alloc(AST.Con, 1);
        data.cons[0] = .{
            .uid = self.gen.cons.newUnique(),
            .name = conName,
            .tys = &.{},
            .data = data,
        };
        try self.errors.append(.{
            .UndefinedCon = .{ .conname = conName, .loc = .{
                .from = conTok.from,
                .to = conTok.to,
                .source = self.lexer.source,
            } },
        });

        // return placeholder var after an error.
        return .{ .con = &data.cons[0], .t = try self.typeContext.fresh() };
    }
}

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
    vars: std.StringHashMap(AST.Var),
    types: std.StringHashMap(*AST.Data),
    cons: std.StringHashMap(*AST.Con),

    fn init(al: std.mem.Allocator) @This() {
        return .{
            .vars = std.StringHashMap(AST.Var).init(al),
            .types = std.StringHashMap(*AST.Data).init(al),
            .cons = std.StringHashMap(*AST.Con).init(al),
        };
    }
};

// typechecking zone
fn getReturnType(self: *Self) !AST.Type {
    return self.returnType orelse try self.defined(.Int);
}

fn defined(self: *Self, predefinedType: Prelude.PremadeType) !AST.Type {
    return if (self.prelude) |prelude|
        prelude.defined(predefinedType)
    else b: {
        const data = self.maybeLookupType(Prelude.PremadeTypeName.get(predefinedType)) orelse break :b error.PreludeError;
        // too much work. We should create a TypeRef and then cache it.
        break :b try self.typeContext.newType(.{ .Con = .{
            .type = data,
            .application = &.{},
        } });
    };
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
const ParserError = error{ ParseError, PreludeError, OutOfMemory }; // full error set when it cannot be inferred.
