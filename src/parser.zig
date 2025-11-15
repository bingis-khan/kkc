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
const Set = @import("Set.zig").Set;

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
    tvars: UniqueGen,
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
            .tvars = UniqueGen.init(),
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
    self.consumeSeps();
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
    self.scope.beginScope(null);

    // tvars
    var tvars = std.ArrayList(AST.TVar).init(self.arena);
    while (self.consume(.IDENTIFIER)) |tvname| {
        const tv = try self.newTVar(tvname);
        try tvars.append(tv);
    }

    if (!self.check(.INDENT)) {
        // Add type without any constructors
        try self.newData(try Common.allocOne(self.arena, AST.Data, .{
            .uid = self.gen.types.newUnique(),
            .name = typename.literal(self.lexer.source),
            .cons = &.{},
            .scheme = .{
                .tvars = tvars.items,
            },
        }));
        return;
    }

    const data = try self.arena.create(AST.Data);
    data.uid = self.gen.types.newUnique();
    data.name = typename.literal(self.lexer.source);
    data.scheme = .{ .tvars = tvars.items }; // TODO: check for repeating tvars and such.

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

    self.scope.endScope();
    try self.newData(data);
}

fn function(self: *Self, id: Token) !AST.Stmt {
    const fun = try self.newFunction(id);

    // already begin env
    var env = Env.init(self.arena);
    self.scope.beginScope(&env);

    var params = std.ArrayList(AST.Function.Param).init(self.arena);
    if (!self.check(.RIGHT_PAREN)) {
        while (true) {
            const pnt = try self.expect(.IDENTIFIER);
            const v = try self.newVar(pnt);
            const nextTok = self.peek().type;
            if (nextTok != .COMMA and nextTok != .RIGHT_PAREN) {
                const pt = try self.sepTyo(true);
                try self.typeContext.unify(v.t, pt);
            }
            try params.append(.{
                .pn = v.v,
                .pt = v.t,
            });

            if (!self.check(.COMMA)) break;
        }

        try self.devour(.RIGHT_PAREN);
    }

    // prepare params for a function type.
    const paramTs = try self.arena.alloc(AST.Type, params.items.len);
    for (params.items, 0..) |et, i| {
        paramTs[i] = et.pt;
    }

    const ret = try self.typeContext.fresh();
    if (self.check(.RIGHT_ARROW)) {
        const retTy = try self.sepTyo(true);
        try self.typeContext.unify(ret, retTy);
    }

    const fnBody = if (self.check(.COLON)) b: {
        const expr = try self.expression();
        const stmts = try self.arena.alloc(*AST.Stmt, 1);
        stmts[0] = try Common.allocOne(self.arena, AST.Stmt, .{
            .Return = expr,
        });
        break :b stmts;
    } else b: {
        // set return and parse body
        const oldReturnType = self.returnType;
        self.returnType = ret;
        const fnBody = try self.body();
        self.returnType = oldReturnType;

        break :b fnBody;
    };

    // after typechecking inside the function, create scheme.
    // TODO: unfinished, we don't care about the environment yet.
    const definedTVars = self.scope.currentScope().tvars;
    self.scope.endScope(); // finish env.

    const scheme = try self.mkSchemeforFunction(&definedTVars, params.items, ret, env.items);

    // NOTE: I assign all at once so tha the compiler ensures I leave no field uninitialized.
    fun.* = AST.Function{
        .name = fun.name,
        .params = params.items,
        .ret = ret,
        .body = fnBody,
        .scheme = scheme,
        .env = env.items,
    };

    const fndec = AST.Stmt{ .Function = fun };

    return fndec;
}

fn body(self: *Self) ![]*AST.Stmt {
    try self.devour(.INDENT);

    self.scope.beginScope(null);
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
            const expr = if (!self.isEndStmt())
                try self.expression()
            else bb: {
                const t = try self.defined(.Unit);
                const con = &self.typeContext.getType(t).Con.type.cons[0]; // WARNING: funny casts
                break :bb try self.allocExpr(.{
                    .t = t,
                    .e = .{ .Con = con },
                });
            };

            try self.typeContext.unify(expr.t, try self.getReturnType());

            try self.endStmt();
            break :b .{ .Return = expr };
        } else if (self.consume(.IDENTIFIER)) |v| {
            // here, choose between identifier and call
            if (self.check(.EQUALS)) {
                const expr = try self.expression();
                const vt = try self.newVar(v);
                try self.typeContext.unify(vt.t, expr.t);

                try self.endStmt();

                break :b .{ .VarDec = .{
                    .varDef = vt.v,
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

fn isEndStmt(self: *const Self) bool {
    const tt = self.peek().type;
    return tt == .DEDENT or tt == .STMT_SEP;
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
                .Fun = .{
                    .args = paramTs,
                    .ret = retType,
                    .env = try self.typeContext.newEnv(null),
                },
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
        const dv = try self.instantiateVar(v);
        return self.allocExpr(.{
            .t = dv.t,
            .e = switch (dv.v) {
                .Var => |vv| .{ .Var = vv },
                .Fun => |fun| .{ .Fun = fun },
            },
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

fn typ(self: *Self) ParserError!AST.Type {
    return self.typo(false);
}

// type-o
fn typo(self: *Self, inDeclaration: bool) ParserError!AST.Type {
    // temp
    if (self.consume(.TYPE)) |ty| {
        const ity = try self.instantiateType(ty);
        if (ity.tyArgs.len != 0) {
            try self.errors.append(.{ .MismatchingKind = .{ .data = ity.data, .expect = ity.tyArgs.len, .actual = 0 } });
        }
        return ity.t;
    } else if (self.consume(.IDENTIFIER)) |tv| { // TVAR
        return self.typeContext.newType(.{ .TVar = try self.lookupTVar(tv, inDeclaration) });
    } else if (self.check(.LEFT_PAREN)) {
        const ty = try self.sepTy();
        try self.devour(.RIGHT_PAREN);
        return ty;
    } else {
        return try self.err(AST.Type, "Expect type", .{});
    }
    unreachable;
}

fn sepTy(self: *Self) !AST.Type {
    return self.sepTyo(false);
}
fn sepTyo(self: *Self, inDeclaration: bool) !AST.Type {
    if (self.consume(.TYPE)) |tyName| {
        const ty = try self.instantiateType(tyName);
        var tyArgs = std.ArrayList(AST.Type).init(self.arena);
        while (true) {
            const tokType = self.peek().type;
            if (!(tokType == .LEFT_PAREN or tokType == .TYPE or tokType == .IDENTIFIER)) { // bad bad works
                break;
            }

            try tyArgs.append(try self.typo(inDeclaration));
        }

        try self.typeContext.unifyParams(ty.tyArgs, tyArgs.items);

        // there's a possibility it's a function!
        if (self.check(.RIGHT_ARROW)) {
            const args = try self.arena.alloc(AST.Type, 1);
            args[0] = ty.t;
            const ret = try self.sepTyo(inDeclaration);
            return try self.typeContext.newType(.{ .Fun = .{
                .args = args,
                .ret = ret,
                .env = try self.typeContext.newEnv(null),
            } });
        } else {
            return ty.t;
        }
    } else if (self.check(.LEFT_PAREN)) {
        // try parse function (but it can also be an extra paren!)
        var args = std.ArrayList(AST.Type).init(self.arena);
        while (!self.check(.RIGHT_PAREN)) {
            try args.append(try self.sepTyo(inDeclaration));
            if (self.peek().type != .RIGHT_PAREN) {
                try self.devour(.COMMA);
            }
        }

        if (self.check(.RIGHT_ARROW)) {
            const ret = try self.typo(inDeclaration);
            return try self.typeContext.newType(.{ .Fun = .{
                .ret = ret,
                .args = args.items,
                .env = try self.typeContext.newEnv(null),
            } });
        } else if (args.items.len == 1) { // just parens!
            return args.items[0];
        } else if (args.items.len == 0) {
            // only Unit tuple is supported.
            return try self.defined(.Unit);
        } else { // this LOOKS like a tuple, but we don't support tuples yet!
            try self.errors.append(.{ .TuplesNotYetSupported = .{} });
            return self.typeContext.fresh();
        }
    } else {
        return try self.typo(inDeclaration);
    }
    unreachable;
}

// resolver zone
// VARS
fn newVar(self: *@This(), varTok: Token) !struct { v: AST.Var, t: AST.Type } {
    const varName = varTok.literal(self.lexer.source);
    const t = try self.typeContext.fresh();
    const thisVar = AST.Var{
        .name = varName,
        .uid = self.gen.vars.newUnique(),
    };
    try self.scope.currentScope().vars.put(varName, .{ .Var = .{ .v = thisVar, .t = t } });
    return .{ .v = thisVar, .t = t };
}

fn newFunction(self: *@This(), funNameTok: Token) !*AST.Function {
    const varName = funNameTok.literal(self.lexer.source);
    const thisVar = AST.Var{
        .name = varName,
        .uid = self.gen.vars.newUnique(),
    };
    const funPtr = try self.arena.create(AST.Function);
    funPtr.name = thisVar;
    try self.scope.currentScope().vars.put(varName, .{ .Fun = funPtr });
    return funPtr;
}

fn instantiateVar(self: *@This(), varTok: Token) !AST.VarInst {
    const varName = varTok.literal(self.lexer.source);
    var lastVars = self.scope.scopes.iterateFromTop();
    while (lastVars.nextPtr()) |cursc| {
        if (cursc.vars.get(varName)) |vorf| {
            const varInst: AST.VarInst = switch (vorf) {
                .Var => |vt| .{ .v = .{ .Var = vt.v }, .t = vt.t },
                .Fun => |fun| b: {
                    const match = try self.instantiateScheme(fun.scheme);

                    // mk new, instantiated type
                    var params = std.ArrayList(AST.Type).init(self.arena);
                    for (fun.params) |p| {
                        try params.append(try self.mapType(&match, p.pt));
                    }

                    const ret = try self.mapType(&match, fun.ret);
                    const funTy = try self.typeContext.newType(.{ .Fun = .{
                        .args = params.items,
                        .ret = ret,
                        .env = try self.typeContext.newEnv(fun.env),
                    } });
                    break :b .{ .v = .{ .Fun = fun }, .t = funTy };
                },
            };

            // TODO: I should make a separate function, but I'm still not sure about the interface.
            var lastScope = self.scope.scopes.iterateFromTop();
            while (lastScope.nextPtr()) |sc| {
                if (sc == cursc) break; // we are in the scope the var was defined in, so don't add it to its env.

                if (sc.env) |env| {
                    try env.append(varInst);
                }
            }

            return varInst;
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
        const t = try self.typeContext.fresh();

        // return placeholder var after an error.
        return .{ .v = .{ .Var = placeholderVar }, .t = t };
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

fn instantiateData(self: *Self, data: *AST.Data) !struct {
    t: AST.Type,
    tyArgs: []AST.Type,
    match: AST.Match,
} {
    const match = try self.instantiateScheme(data.scheme);

    return .{
        .t = try self.typeContext.newType(.{ .Con = .{
            .type = data,
            .application = match,
        } }),
        .tyArgs = match.tvars,
        .match = match,
    };
}

fn instantiateType(self: *Self, tyTok: Token) !struct {
    data: *AST.Data,
    t: AST.Type,
    tyArgs: []AST.Type,
    match: AST.Match,
} {
    const typename = tyTok.literal(self.lexer.source);
    if (self.maybeLookupType(typename)) |data| {
        const dt = try self.instantiateData(data);
        return .{
            .data = data,
            .t = dt.t,
            .tyArgs = dt.tyArgs,
            .match = dt.match,
        };
    } else {
        const placeholderType = try Common.allocOne(self.arena, AST.Data, .{
            .name = typename,
            .uid = self.gen.vars.newUnique(),
            .cons = &.{},
            .scheme = .{ .tvars = &.{} },
        });
        const location = Common.Location{
            .from = tyTok.from,
            .to = tyTok.to,
            .source = self.lexer.source,
        };
        try self.errors.append(.{
            .UndefinedType = .{ .typename = typename, .loc = location },
        });

        const match = AST.Match.empty(placeholderType.scheme);
        return .{
            .data = placeholderType,
            .t = try self.typeContext.newType(.{ .Con = .{
                .type = placeholderType,
                .application = match,
            } }),
            .tyArgs = &.{},
            .match = match,
        };
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

// TVar
fn newTVar(self: *@This(), tvTok: Token) !AST.TVar {
    const tvname = tvTok.literal(self.lexer.source);
    const tv: AST.TVar = .{
        .uid = self.gen.tvars.newUnique(),
        .name = tvname,
    };
    try self.scope.currentScope().tvars.put(tvname, tv);
    return tv;
}

// basically, sometimes when we look up tvars in declarations, we want to define them. slightly hacky, but makes stuff easier.
fn lookupTVar(self: *Self, tvTok: Token, inDeclaration: bool) !AST.TVar {
    const tvname = tvTok.literal(self.lexer.source);
    var lastScopes = self.scope.scopes.iterateFromTop();
    while (lastScopes.next()) |cursc| {
        if (cursc.tvars.get(tvname)) |tv| {
            return tv;
        }
    } else {
        // create a new var then.
        if (!inDeclaration) {
            try self.errors.append(.{ .UndefinedTVar = .{
                .tvname = tvname,
                .loc = .{
                    .from = tvTok.from,
                    .to = tvTok.to,
                    .source = self.lexer.source,
                },
            } });
        }
        return try self.newTVar(tvTok);
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
            const dt = try self.instantiateData(con.data);

            // found con. now we instantiate it.
            if (con.tys.len == 0) {
                return .{ .con = con, .t = dt.t };
            } else {
                var args = std.ArrayList(AST.Type).init(self.arena);
                for (con.tys) |ty| {
                    try args.append(try self.mapType(&dt.match, ty));
                }
                return .{
                    .con = con,
                    .t = try self.typeContext.newType(.{
                        .Fun = .{
                            .args = args.items,
                            .ret = dt.t,
                            .env = try self.typeContext.newEnv(&.{}), // nocheckin: we have to figure out if the env is the same.
                        },
                    }),
                };
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
        data.scheme = AST.Scheme.empty();
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

// SCHEMES
fn instantiateScheme(self: *Self, scheme: AST.Scheme) !AST.Match {
    var tvars = std.ArrayList(AST.Type).init(self.arena);
    for (scheme.tvars) |_| {
        try tvars.append(try self.typeContext.fresh());
    }
    return .{
        .scheme = scheme,
        .tvars = tvars.items,
    };
}

const FTVs = Set(FTV, struct {
    pub fn eql(ctx: @This(), a: FTV, b: FTV) bool {
        _ = ctx;
        return a.tyv == b.tyv;
    }

    pub fn hash(ctx: @This(), k: FTV) u64 {
        _ = ctx;
        // return @truncate(k.tyv);
        return k.tyv;
    }
});
const FTV = struct { tyv: AST.TyVar, t: AST.Type };
fn mkSchemeforFunction(self: *Self, alreadyDefinedTVars: *const std.StringHashMap(AST.TVar), params: []AST.Function.Param, ret: AST.Type, env: AST.Env) !AST.Scheme {
    // Function local stuff.
    var funftvs = FTVs.init(self.arena);
    try self.ftvs(&funftvs, ret);
    for (params) |p| {
        try self.ftvs(&funftvs, p.pt);
    }

    // environment stuff.
    var envftvs = FTVs.init(self.arena);
    for (env) |inst| {
        try self.ftvs(&envftvs, inst.t);
    }

    // now, remove the tyvars from env here.
    funftvs.difference(&envftvs);

    // make tvars out of them
    // TODO: assign pretty names ('a, 'b, etc.).
    var tvars = std.ArrayList(AST.TVar).init(self.arena);

    // add defined tvars in this function.
    var tvit = alreadyDefinedTVars.valueIterator();
    while (tvit.next()) |tvar| {
        try tvars.append(tvar.*);
    }

    var it = funftvs.iterator();
    while (it.next()) |e| {
        const name = try std.fmt.allocPrint(self.arena, "'{}", .{e.tyv});
        const tv = AST.TVar{
            .name = name,
            .uid = self.gen.tvars.newUnique(),
        };
        try tvars.append(tv);
        const tvt = try self.typeContext.newType(.{ .TVar = tv });
        try self.typeContext.unify(e.t, tvt);
    }

    return .{ .tvars = tvars.items };
}

fn ftvs(self: *Self, store: *FTVs, tref: AST.Type) !void {
    const t = self.typeContext.getType(tref);
    switch (t) {
        .TyVar => |tyv| try store.insert(.{ .tyv = tyv, .t = tref }),
        .Con => |con| {
            for (con.application.tvars) |mt| {
                try self.ftvs(store, mt);
            }
        },
        .Fun => |fun| {
            for (fun.args) |arg| {
                try self.ftvs(store, arg);
            }

            try self.ftvs(store, fun.ret);
        },
        .TVar => {},
    }
}

pub fn mapType(self: *Self, match: *const AST.Match, ty: AST.Type) !AST.Type {
    const t = self.typeContext.getType(ty);
    return switch (t) {
        .Con => |con| b: {
            var tvars = std.ArrayList(AST.Type).init(self.arena);
            var changed = false;
            for (con.application.tvars) |oldTy| {
                const newTy = try self.mapType(match, oldTy);
                changed = changed or !newTy.eq(oldTy);
                try tvars.append(newTy);
            }

            if (!changed) {
                tvars.deinit();
                break :b ty;
            }

            break :b try self.typeContext.newType(.{ .Con = .{
                .type = con.type,
                .application = .{
                    .scheme = con.application.scheme,
                    .tvars = tvars.items,
                },
            } });
        },
        .Fun => |fun| b: {
            var changed = false;

            var args = std.ArrayList(AST.Type).init(self.arena);
            for (fun.args) |oldTy| {
                const newTy = try self.mapType(match, oldTy);
                changed = changed or !newTy.eq(oldTy);
                try args.append(newTy);
            }

            const ret = try self.mapType(match, fun.ret);
            changed = changed or !ret.eq(fun.ret);

            const env = if (self.typeContext.envContext.items[fun.env.id]) |env| bb: {
                var envChanged = false;
                var nuenv = std.ArrayList(AST.VarInst).init(self.arena);
                for (env) |inst| {
                    const newTy = try self.mapType(match, inst.t);
                    envChanged = envChanged or !newTy.eq(inst.t);
                    try nuenv.append(.{ .v = inst.v, .t = newTy });
                }

                changed = changed or envChanged;

                break :bb if (envChanged) try self.typeContext.newEnv(nuenv.items) else fun.env;
            } else try self.typeContext.newEnv(null); // IMPORTANT: must instantiate new env..

            if (!changed) {
                args.deinit();
                break :b ty;
            }

            break :b try self.typeContext.newType(.{ .Fun = .{
                .args = args.items,
                .ret = ret,
                .env = env,
            } });
        },
        .TVar => |tv| match.mapTVar(tv) orelse ty,
        .TyVar => ty,
    };
}

const Scope = struct {
    al: std.mem.Allocator,
    scopes: stack.Fixed(CurrentScope, Common.MaxIndent),

    pub fn init(al: std.mem.Allocator) @This() {
        const Scopes = stack.Fixed(CurrentScope, Common.MaxIndent);
        var scopes = Scopes.init();
        const defaultScope = CurrentScope.init(al, null);
        scopes.push(defaultScope);
        return .{
            .al = al,
            .scopes = scopes,
        };
    }

    pub fn currentScope(self: *@This()) *CurrentScope {
        return self.scopes.topp();
    }

    pub fn beginScope(self: *@This(), env: ?*Env) void {
        self.scopes.push(CurrentScope.init(self.al, env));
    }

    pub fn endScope(self: *@This()) void {
        _ = self.scopes.pop();
    }

    // ENVS
    // pub fn beginEnv(self: *@This()) []VarInst {
    //     self.scopes.push(CurrentScope.init(self.al, CurrentScope.Env.init(self.al)));
    // }

    // pub fn endEnv(self: *@This()) void { // actually, return env.
    //     const sc = self.scopes.pop();

    //     if (sc.env) |env| {
    //         return env.items;
    //     } else {
    //         // this should not happen. begin/end scope must be perfectly matched.
    //         unreachable;
    //     }
    // }
};

const CurrentScope = struct {
    const VarOrFun = union(enum) {
        Var: struct { v: AST.Var, t: AST.Type },
        Fun: *AST.Function,
    };

    vars: std.StringHashMap(VarOrFun),
    types: std.StringHashMap(*AST.Data),
    cons: std.StringHashMap(*AST.Con),
    tvars: std.StringHashMap(AST.TVar),

    env: ?*Env,

    fn init(al: std.mem.Allocator, env: ?*Env) @This() {
        return .{
            .vars = std.StringHashMap(VarOrFun).init(al),
            .types = std.StringHashMap(*AST.Data).init(al),
            .cons = std.StringHashMap(*AST.Con).init(al),
            .tvars = std.StringHashMap(AST.TVar).init(al),
            .env = env,
        };
    }

    // in the future - scopes are actually safe to deallocate.
    fn deinit() void {}
};

const Env = std.ArrayList(AST.VarInst);

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
            .application = AST.Match.empty(data.scheme),
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

// NOTE: later, we don't have to specify a return value. Just always follow it with "return unreachable".
fn err(self: *Self, comptime t: type, comptime fmt: []const u8, args: anytype) !t {
    std.debug.print(fmt ++ " at {}\n", args ++ .{self.currentToken});
    std.debug.print("{s}\n", .{self.lexer.source[self.currentToken.from -% 5 .. @min(self.lexer.source.len, self.currentToken.to +% 5)]});
    return error.ParseError;
}

// this might be in the tokenizer.
fn sync_to_next_toplevel() void {}

const ParseError = error{ParseError};
const ParserError = error{ ParseError, PreludeError, OutOfMemory }; // full error set when it cannot be inferred.
