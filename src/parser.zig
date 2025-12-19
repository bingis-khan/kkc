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
const Module = @import("Module.zig");
const Modules = @import("Modules.zig");
const Intrinsic = @import("Intrinsic.zig");

// fuck it. let's do it one pass.
arena: std.mem.Allocator,

errors: *Errors,
name: Str,

// parser zone
lexer: Lexer,
currentToken: Token,
mode: ParsingMode, // A bit of a hack. I think we should utilize the zigger's polymorphism to make different parsers (akin to the type parser)

// resolver zone
gen: *Modules.Gen,
scope: Scope,
base: Module.BasePath,

// type zone
typeContext: *TypeContext,
prelude: ?Prelude,

returnType: ?AST.Type,
returned: ReturnStatus, // statement level check if something was returned.
triedReturningAtAll: bool, // function level check if we even tried returning something. if this is true, returnType must not be null.

selfType: ?AST.Type, // this is used in class and instance context. Whenever it's defined, the user is able to reference the instance type by '_'. In nested instances, obviously points to the innermost one.
associations: std.ArrayList(Association),
modules: *Modules,
importedModules: Imports,

const ReturnStatus = enum {
    Nah,
    Returned,
    Errored,

    fn alternative(self: @This(), other: @This()) @This() {
        if (self == .Nah or other == .Nah) return .Nah;
        if (self == .Errored or other == .Errored) return .Errored;
        return .Returned;
    }
};
const Imports = std.HashMap(Module.Path, ?Module, Module.PathCtx, std.hash_map.default_max_load_percentage);

const Self = @This();
pub fn init(l: Lexer, prelude: ?Prelude, base: Module.BasePath, moduleName: Str, modules: *Modules, errors: *Errors, context: *TypeContext, arena: std.mem.Allocator) !Self {
    var parser = Self{
        .arena = arena,
        .errors = errors, // TODO: use GPA
        .name = moduleName,

        // parser
        .lexer = l,
        .currentToken = undefined,
        .mode = .Normal,

        // resolver
        .scope = Scope.init(arena), // TODO: use GPA
        .gen = &modules.gen,
        .base = base,

        // typeshit
        .typeContext = context,
        .returnType = null,
        .triedReturningAtAll = false,
        .returned = .Nah,
        .selfType = null,
        .prelude = prelude,

        .associations = std.ArrayList(Association).init(arena),

        .modules = modules,
        .importedModules = Imports.init(arena),
    };

    parser.currentToken = parser.lexer.nextToken();
    return parser;
}

pub fn parse(self: *Self) !Module {
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

    try self.solveAvailableConstraints();

    if (self.associations.items.len > 0) {
        try self.errors.append(.{ .ConstraintsLeft = .{
            .module = self.name,
            .numConstraints = self.associations.items.len,
        } });
    }

    // std.debug.print("parsing success\n", .{});

    return .{
        .ast = AST{ .toplevel = decs.items },
        .exports = self.scopeToExports(),
    };
}

pub fn addExports(self: *Self, exports: *const Module.Exports) !void {
    try addToHash(&self.scope.currentScope().vars, exports.vars);
    try addToHash(&self.scope.currentScope().cons, exports.cons);
    try addToHash(&self.scope.currentScope().types, exports.types);
    try addToHash(&self.scope.currentScope().instances, exports.instances);
}

fn addToHash(dest: anytype, src: anytype) !void {
    var it = src.iterator();
    while (it.next()) |e| {
        try dest.put(e.key_ptr.*, e.value_ptr.*);
    }
}

// NOTE: assumes Module.Exports now owns the thing.
fn scopeToExports(self: *Self) Module.Exports {
    std.debug.assert(self.scope.scopes.current == 1);

    const scope = self.scope.currentScope();
    return .{
        .vars = scope.vars,
        .types = scope.types,
        .cons = scope.cons,
        .instances = scope.instances,
    };
}

fn dataDef(self: *Self, typename: Token, extraTVar: ?Token) !void {
    const uid = self.gen.types.newUnique();
    self.scope.beginScope(null);
    const data = b: {
        defer self.scope.endScope();

        // tvars
        var tvars = std.ArrayList(AST.TVar).init(self.arena);
        if (extraTVar) |tvname| {
            const tv = try self.newTVar(tvname.literal(self.lexer.source), .{ .Data = uid });
            try tvars.append(tv);
        }
        while (self.consume(.IDENTIFIER)) |tvname| {
            const tv = try self.newTVar(tvname.literal(self.lexer.source), .{ .Data = uid });
            try tvars.append(tv);
        }

        if (!self.check(.INDENT)) {
            // Add type without any constructors
            break :b try Common.allocOne(self.arena, AST.Data{
                .uid = uid,
                .name = typename.literal(self.lexer.source),
                .stuff = .{ .cons = &.{} },
                .scheme = .{
                    .tvars = tvars.items,
                    .envVars = &.{},
                    .associations = &.{}, // TODO: when I add fake class names as types, this will  be non-empty.
                },
            });
        }

        const data = try self.arena.create(AST.Data);
        data.uid = self.gen.types.newUnique();
        data.name = typename.literal(self.lexer.source);
        data.scheme = .{
            .tvars = tvars.items,
            .envVars = &.{}, // TEMP
            .associations = &.{},
        }; // TODO: check for repeating tvars and such.

        var cons = std.ArrayList(AST.Con).init(self.arena);
        var recs = std.ArrayList(AST.Record).init(self.arena);
        var tag: u32 = 0;
        while (!self.check(.DEDENT)) {
            if (self.consume(.IDENTIFIER)) |recname| {
                // record
                const t = try Type.init(self, .{ .Data = data.uid }).sepTyo();
                try recs.append(.{
                    .field = recname.literal(self.lexer.source),
                    .t = t,
                });
                try self.endStmt();
            } else if (self.consume(.TYPE)) |conName| {
                // constructor
                var tys = std.ArrayList(AST.Type).init(self.arena);
                while (!(self.check(.STMT_SEP) or (self.peek().type == .DEDENT))) { // we must not consume the last DEDENT, as it's used to terminate the whole type declaration.
                    // TODO: for now, no complicated types!
                    const ty = try Type.init(self, .{ .Data = data.uid }).typ();
                    try tys.append(ty);
                }
                self.consumeSeps();

                try cons.append(.{
                    .uid = self.gen.cons.newUnique(),
                    .name = conName.literal(self.lexer.source),
                    .tys = tys.items,
                    .data = data,
                    .tagValue = tag,
                });

                tag += 1;
            } else unreachable;
        }

        if (cons.items.len > 0 and recs.items.len > 0) {
            try self.errors.append(.{ .RecordsAndConstructorsPresent = .{} });
        }

        if (cons.items.len > 0) {
            data.stuff = .{ .cons = cons.items };
        } else {
            data.stuff = .{ .recs = recs.items };
        }

        break :b data;
    };
    try self.newData(data);
}

fn function(self: *Self, fun: *AST.Function) !*AST.Function {
    const oldTriedReturningAtAll = self.triedReturningAtAll;
    const oldReturned = self.returned;
    defer {
        self.triedReturningAtAll = oldTriedReturningAtAll;
        self.returned = oldReturned;
    }
    self.triedReturningAtAll = false;
    self.returned = .Nah;

    // already begin env
    var env = Env.init(self.arena);

    self.scope.beginScope(&env);

    var params = std.ArrayList(*AST.Decon).init(self.arena);
    if (!self.check(.RIGHT_PAREN)) {
        while (true) {
            const decon = try self.deconstruction();
            const nextTok = self.peek().type;
            if (nextTok != .COMMA and nextTok != .RIGHT_PAREN) {
                const pt = try Type.init(
                    self,
                    .{ .Function = fun.name.uid },
                ).sepTyo();
                try self.typeContext.unify(decon.t, pt);
            }
            try params.append(decon);

            if (!self.check(.COMMA)) break;
        }

        try self.devour(.RIGHT_PAREN);
    }

    // prepare params for a function type.
    const paramTs = try self.arena.alloc(AST.Type, params.items.len);
    for (params.items, 0..) |et, i| {
        paramTs[i] = et.t;
    }

    // -> ty
    const ret = try self.typeContext.fresh();
    if (self.check(.RIGHT_ARROW)) {
        const retTy = try Type.init(self, .{ .Function = fun.name.uid }).sepTyo();
        try self.typeContext.unify(ret, retTy);
    }

    const constraints_ = try self.constraints();

    const fnBody = if (self.check(.COLON)) b: {
        const expr = try self.expression();
        const stmts = try self.arena.alloc(*AST.Stmt, 1);
        stmts[0] = try Common.allocOne(self.arena, AST.Stmt{
            .Return = expr,
        });
        try self.typeContext.unify(ret, expr.t);
        break :b stmts;
    } else b: {
        // set return and parse body
        const oldReturnType = self.returnType;
        self.returnType = ret;
        const fnBodyAndReturnStatus = try self.body();
        var fnBody = fnBodyAndReturnStatus.stmts;
        const returnStatus = fnBodyAndReturnStatus.returnStatus;

        if (!self.triedReturningAtAll) {
            try fnBody.append(try Common.allocOne(self.arena, try self.unitReturn()));
        } else if (returnStatus == .Nah) retcheck: {
            switch (self.typeContext.getType(self.returnType.?)) {
                .Con => |c| if (c.type.uid == (try self.defined(.Unit)).data.uid) {
                    try fnBody.append(try Common.allocOne(self.arena, try self.unitReturn()));
                    break :retcheck;
                },
                else => {},
            }

            // otherwise
            try self.errors.append(.{ .MissingReturn = .{} });
        }

        self.returnType = oldReturnType;

        break :b fnBody.items;
    };

    // const lastStmt = fnBody[fnBody.len - 1];

    try self.solveAvailableConstraints();

    // after typechecking inside the function, create scheme.
    // TODO: unfinished, we don't care about the environment yet.
    const definedTVars = self.scope.currentScope().tvars;
    self.scope.endScope(); // finish env.

    const scheme = try self.mkSchemeforFunction(&definedTVars, params.items, ret, env.items, fun.name.uid, &constraints_);

    // NOTE: I assign all at once so tha the compiler ensures I leave no field uninitialized.
    fun.* = AST.Function{
        .name = fun.name,
        .params = params.items,
        .ret = ret,
        .body = fnBody,
        .scheme = scheme,
        .env = env.items,
    };

    return fun;
}

fn body(self: *Self) !struct { stmts: std.ArrayList(*AST.Stmt), returnStatus: ReturnStatus } {
    std.debug.assert(self.returned != .Returned);
    const oldReturned = self.returned;
    defer self.returned = oldReturned;

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

    return .{ .stmts = stmts, .returnStatus = self.returned };
}

fn statement(self: *Self) ParserError!?*AST.Stmt {
    if (self.returned == .Returned) {
        try self.errors.append(.{ .UnreachableCode = .{} });
        self.returned = .Errored;
    }

    const annotations = try self.parseAnnotation();

    const stmtVal: ?AST.Stmt = b: {
        if (self.check(.PASS)) {
            // pass here is a half debug statement. it can also take an integer, which should be printed at runtime (this is for matching in a debugger.)
            var label: ?i64 = null;
            if (self.consume(.INTEGER)) |sint| {
                label = std.fmt.parseInt(i64, sint.literal(self.lexer.source), 10) catch unreachable;
            }

            try self.endStmt();

            break :b .{ .Pass = label };
        } // pass
        else if (self.check(.RETURN)) {
            const expr = if (!self.isEndStmt()) bb: {
                const pm = self.foldFromHere();
                const e = try self.expression();
                try self.finishFold(pm);
                break :bb e;
            } else bb: {
                try self.endStmt();
                const t = try self.definedType(.Unit);
                const con = &self.typeContext.getType(t).Con.type.stuff.cons[0]; // WARNING: funny casts
                break :bb try self.allocExpr(.{
                    .t = t,
                    .e = .{ .Con = con },
                });
            };

            self.triedReturningAtAll = true;
            if (self.returned != .Errored) self.returned = .Returned;

            try self.typeContext.unify(expr.t, try self.getReturnType());

            break :b .{ .Return = expr };
        } // return
        else if (self.check(.USE)) {
            var modpath = std.ArrayList(Str).init(self.arena);
            const firstMod = try self.expect(.TYPE);
            try modpath.append(firstMod.literal(self.lexer.source));
            while (self.check(.DOT)) {
                const mod = try self.expect(.TYPE);
                try modpath.append(mod.literal(self.lexer.source));
            }

            const mmodule = try self.loadModuleFromPath(modpath.items);

            // IMPORT LIST YO.
            if (self.check(.INDENT)) {
                while (true) {
                    if (self.consume(.IDENTIFIER)) |v| {
                        try self.endStmt();
                        if (mmodule) |mod| {
                            const varName = v.literal(self.lexer.source);
                            if (mod.lookupVar(varName)) |vv| {
                                try self.scope.currentScope().vars.put(varName, vv);
                            } else {
                                try self.errors.append(.{ .ModuleDoesNotExportThing = .{} });
                            }
                        }
                    } else if (self.consume(.TYPE)) |tt| {
                        const typeName = tt.literal(self.lexer.source);
                        try self.endStmt();
                        const dataOrClass: ?Module.DataOrClass = bb: {
                            if (mmodule) |mod| {
                                const doc = mod.lookupData(typeName) orelse {
                                    try self.errors.append(.{ .UndefinedType = .{ .typename = typeName, .loc = tt.toLocation(self.lexer.source) } });
                                    break :bb null;
                                };
                                try self.scope.currentScope().types.put(typeName, doc);
                                break :bb doc;
                            } else {
                                break :bb null;
                            }
                        };

                        if (self.check(.LEFT_PAREN)) {
                            if (!self.check(.RIGHT_PAREN)) while (true) {
                                if (self.consume(.IDENTIFIER)) |vt| {
                                    const vname = vt.literal(self.lexer.source);
                                    if (dataOrClass) |doc| {
                                        switch (doc) {
                                            .Class => |c| {
                                                for (c.classFuns) |cfun| {
                                                    if (Common.streq(cfun.name.name, vname)) {
                                                        try self.scope.currentScope().vars.put(vname, .{ .ClassFun = cfun });
                                                        break;
                                                    }
                                                } else {
                                                    try self.errors.append(.{ .ClassDoesNotExportThing = .{} });
                                                }
                                            },
                                            .Data => try self.errors.append(.{ .ModuleDoesNotExportThing = .{} }),
                                        }
                                    }
                                } else if (self.consume(.TYPE)) |ct| {
                                    const cname = ct.literal(self.lexer.source);
                                    if (dataOrClass) |doc| {
                                        switch (doc) {
                                            .Data => |d| {
                                                for (d.stuff.cons) |*con| {
                                                    if (Common.streq(con.name, cname)) {
                                                        try self.scope.currentScope().cons.put(cname, con);
                                                        break;
                                                    }
                                                } else {
                                                    try self.errors.append(.{ .DataDoesNotExportThing = .{} });
                                                }
                                            },
                                            .Class => try self.errors.append(.{ .ModuleDoesNotExportThing = .{} }),
                                        }
                                    }
                                } else {
                                    return self.err(*AST.Stmt, "Expect imported stuff", .{});
                                }

                                if (self.check(.RIGHT_PAREN)) break;
                                try self.devour(.COMMA);
                            };
                        }
                    } else {
                        return self.err(*AST.Stmt, "Expect import", .{});
                    }

                    if (self.check(.DEDENT)) break;
                }
            } else {
                try self.endStmt();
            }

            break :b null;
        } // use
        else if (self.check(.FN)) {
            const v = try self.expect(.IDENTIFIER);
            try self.devour(.LEFT_PAREN);
            const funptr = try self.newFunction(v);
            const fun = try self.function(funptr);
            const fndec = AST.Stmt{ .Function = fun };
            break :b fndec;
        } // function
        else if (self.consume(.IDENTIFIER)) |v| {
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
            } else if (self.check(.LTEQ)) { // basic mutation (different token.)
                // COPYPASTA
                const e = try self.expression();
                try self.endStmt();

                const vtsc = try self.lookupVar(&.{}, v);
                const vv = switch (vtsc.vorf) {
                    .Var => |vt| vt,
                    else => bb: {
                        try self.errors.append(.{ .TryingToMutateNonVar = .{} });
                        break :bb try self.newVar(v);
                    },
                };

                try self.typeContext.unify(vv.t, e.t);
                break :b .{ .VarMut = .{
                    .varRef = vv.v,
                    .accessors = &.{},
                    .varValue = e,
                } };
            } else if (self.check(.LT)) {
                const vtsc = try self.lookupVar(&.{}, v);
                const vv = switch (vtsc.vorf) {
                    .Var => |vt| vt,
                    else => bb: {
                        try self.errors.append(.{ .TryingToMutateNonVar = .{} });
                        break :bb try self.newVar(v);
                    },
                };

                var innerTy = vv.t;
                var accessors = std.ArrayList(AST.Stmt.Accessor).init(self.arena);
                while (true) {
                    if (self.check(.REF)) {
                        try accessors.append(.{ .tBefore = innerTy, .acc = .Deref });

                        const ptr = (try self.defined(.Ptr)).dataInst;

                        try self.typeContext.unify(innerTy, ptr.t);
                        innerTy = ptr.tyArgs[0];
                    } else if (self.check(.DOT)) {
                        const name = try self.expect(.IDENTIFIER);
                        const field = name.literal(self.lexer.source);
                        try accessors.append(.{
                            .tBefore = innerTy,
                            .acc = .{ .Access = field },
                        });

                        const ft = try self.typeContext.field(innerTy, field);
                        innerTy = ft;
                    } else break;
                }

                try self.devour(.EQUALS);
                const e = try self.expression();

                try self.typeContext.unify(innerTy, e.t);

                // TEMP
                // if (vtsc.sc != self.scope.currentScope()) {
                //     try self.errors.append(.{ .CannotDirectlyMutateVarFromEnv = .{} });
                // }

                try self.endStmt();
                break :b .{ .VarMut = .{
                    .varRef = vv.v,
                    .accessors = accessors.items,
                    .varValue = e,
                } };
            } // mutation
            else {
                // try parsing expression yo as a variable n shiii
                const vv = try self.instantiateVar(&.{}, v);
                const e = try self.finishExpression(try self.allocExpr(.{ .t = vv.t, .e = .{ .Var = .{ .v = vv.v, .match = vv.m } } }));
                try self.endStmt();
                break :b .{ .Expr = e };
            }
        } // var or mut
        else if (self.check(.UNDERSCORE)) {
            try self.devour(.EQUALS);
            const e = try self.expression();
            try self.endStmt();
            break :b .{ .Expr = e };
        } // assignment to nothing
        else if (self.consume(.TYPE)) |typename| {
            const maybeExpr: ?AST.Stmt = funny: {
                if (self.peek().type == .DOT) { // DON'T CONSUME!
                    const qe = try self.qualified(typename);
                    const e = try self.finishExpression(qe);
                    break :funny .{ .Expr = e };
                }

                // can be a tvar or a postfix call.
                const lexstate = self.lexer;
                const curtok = self.currentToken;
                if (self.consume(.IDENTIFIER)) |mtv| {
                    if (self.peek().type == .LEFT_PAREN) {
                        // postfix call.
                        self.lexer = lexstate; // AHH AHSDH FUCK I DID IT, NO!!
                        self.currentToken = curtok;
                        // ITS OBVIOUS I SHOULD USE A `data` KEYWORD OR SOMETHING LIKE THIS BRUHHHH.
                        // BUT MUH QT SYNTAX :OOOOOOOO
                        const ce = try self.constructorExpression(&.{}, typename);
                        break :funny .{ .Expr = try self.finishExpression(ce) };
                    }

                    try self.dataDef(typename, mtv);
                    break :funny null;
                }

                // BRITTLE AS HELL.
                if (self.peek().type == .INDENT or self.peek().type == .STMT_SEP) {
                    // TODO: parse non-qualified postfix expression alls.
                    try self.dataDef(typename, null);
                    break :funny null;
                }

                // actually, this is probably an expression, so parse it as one.
                const ce = try self.constructorExpression(&.{}, typename);
                const e = try self.finishExpression(ce);
                break :funny .{ .Expr = e };
            };

            // NOTE: Experimental shit.
            // It seems that if I'm parsing a real expression (so not null), I always terminate the statement. I might do the same thing for all cases, because I happen to forget them sometimes.
            if (maybeExpr != null) try self.endStmt();
            break :b maybeExpr;
        } // type
        else if (self.check(.IF)) {
            const cond = try self.expression();
            try self.typeContext.unify(cond.t, try self.definedType(.Bool));
            const bTrueBod = try self.body();
            const bTrue = bTrueBod.stmts.items;
            var returnStatus = bTrueBod.returnStatus;

            var elifs = std.ArrayList(AST.Stmt.Elif).init(self.arena);
            while (self.check(.ELIF)) {
                const elifCond = try self.expression();
                try self.typeContext.unify(elifCond.t, try self.definedType(.Bool));
                const elifBodyAndStatus = try self.body();
                returnStatus = returnStatus.alternative(elifBodyAndStatus.returnStatus);
                try elifs.append(AST.Stmt.Elif{ .cond = elifCond, .body = elifBodyAndStatus.stmts.items });
            }

            const elseBody = if (self.check(.ELSE)) els: {
                const bod = try self.body();
                returnStatus = returnStatus.alternative(bod.returnStatus);
                break :els bod.stmts.items;
            } else els: {
                returnStatus = .Nah;
                break :els null;
            };

            self.returned = returnStatus;

            // incomplete
            break :b .{ .If = .{
                .cond = cond,
                .bTrue = bTrue,
                .bOthers = elifs.items,
                .bElse = elseBody,
            } };
        } // if
        else if (self.check(.WHILE)) {
            const cond = try self.expression();
            const boolTy = try self.definedType(.Bool);
            try self.typeContext.unify(cond.t, boolTy);
            const bod = try self.body();
            self.returned = bod.returnStatus;
            break :b .{ .While = .{
                .cond = cond,
                .body = bod.stmts.items,
            } };
        } // while
        else if (self.check(.CASE)) {
            const switchOn = try self.expression();

            var returnStatus = ReturnStatus.Returned; // mempty-like
            var cases = std.ArrayList(AST.Case).init(self.arena);
            try self.devour(.INDENT);
            self.scope.beginScope(null);
            while (!self.check(.DEDENT)) {
                const decon = try self.deconstruction();
                try self.typeContext.unify(switchOn.t, decon.t);
                const bod = try self.body();
                try cases.append(.{ .decon = decon, .body = bod.stmts.items });
                returnStatus = returnStatus.alternative(bod.returnStatus);
            }
            self.scope.endScope();

            self.returned = returnStatus;

            // check here for exhaustiveness. If not exhaustive, add .Nah OR I disallow non-exhaustive cases.

            break :b .{
                .Switch = .{
                    .switchOn = switchOn,
                    .cases = cases.items,
                },
            };
        } // case
        else if (self.check(.CLASS)) {
            const className = try self.expect(.TYPE);
            const uid = self.gen.types.newUnique();

            const oldSelf = self.selfType;
            const selfVar = try self.newTVar("_", .{ .ClassFunction = uid });
            const selfType = try self.typeContext.newType(.{
                .TVar = selfVar,
            });
            self.selfType = selfType;

            const class = try self.arena.create(AST.Class);

            var classFuns = std.ArrayList(*AST.ClassFun).init(self.arena);
            try self.devour(.INDENT);
            while (!self.check(.DEDENT)) {
                const classFun = try self.classFunction(.{ .tvar = selfVar, .t = selfType }, class);
                self.consumeSeps();
                try classFuns.append(classFun);
            }

            self.selfType = oldSelf;

            class.* = AST.Class{
                .uid = uid,
                .name = className.literal(self.lexer.source),
                .classFuns = classFuns.items,
                .selfType = selfVar,
            };

            try self.newClass(class);

            return null;
        } // class
        else if (self.check(.INST)) {
            const className = try self.expect(.TYPE);
            const class = (self.maybeLookupType(className.literal(self.lexer.source)) orelse unreachable).Class;
            const typeName = try self.expect(.TYPE);
            const data = (self.maybeLookupType(typeName.literal(self.lexer.source)) orelse unreachable).Data;

            const oldSelf = self.selfType;
            const instantiatedSelfType = try self.typeContext.newType(.{
                .Con = .{
                    .type = data,
                    .application = try self.instantiateScheme(data.scheme),
                },
            });
            self.selfType = instantiatedSelfType;
            defer self.selfType = oldSelf;

            var instFuns = std.ArrayList(AST.Instance.InstFun).init(self.arena);
            try self.devour(.INDENT);
            while (true) { // while1
                const funName = try self.expect(.IDENTIFIER);
                try self.devour(.LEFT_PAREN);
                const funptr = try self.arena.create(AST.Function);
                funptr.name = AST.Var{
                    .name = funName.literal(self.lexer.source),
                    .uid = self.gen.vars.newUnique(),
                };
                const fun = try self.function(funptr);

                // now, "associate it" with a class function
                for (class.classFuns) |classFun| {
                    if (!Common.streq(classFun.name.name, fun.name.name)) continue;

                    // class function found. unify types.
                    // todo
                    try instFuns.append(.{ .fun = fun, .classFunId = classFun.uid });
                    break;
                } else {
                    // error that instance function is not found.
                    unreachable;
                }

                if (self.check(.DEDENT)) break;
            }

            const instance = try Common.allocOne(self.arena, AST.Instance{
                .uid = self.gen.instances.newUnique(),
                .class = class,
                .data = data,
                .instFuns = instFuns.items,
            });

            try self.addInstance(instance);

            break :b .{
                .Instance = instance,
            };
        } // instance
        else if (self.check(.EXTERNAL)) {
            if (self.consume(.IDENTIFIER)) |nameTok| {
                try self.externalFun(nameTok, annotations);
                // single statement
            } // single stmt
            else if (self.check(.INDENT)) {
                while (!self.check(.DEDENT)) {
                    const funAnns = try self.parseAnnotation(); // this could be later freed.
                    const allAnns = try std.mem.concat(self.arena, AST.Annotation, &[_][]const AST.Annotation{ annotations, funAnns });
                    const nameTok = try self.expect(.IDENTIFIER);
                    try self.externalFun(nameTok, allAnns);
                }
            } // multiple
            else {
                unreachable; // TODO: err
            }

            break :b null;
        } // external function
        else {
            // this is kinda funny now, but try to parse an expression in this case.
            const pm = self.foldFromHere();
            const e = try self.expression();
            try self.finishFold(pm);
            break :b .{
                .Expr = e,
            };
            // return self.err(*AST.Stmt, "Expect statement.", .{});
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

fn externalFun(self: *Self, nameTok: Token, annotations: []AST.Annotation) !void {
    const uid = self.gen.vars.newUnique();
    const name = nameTok.literal(self.lexer.source);
    var env = Env.init(self.arena);
    self.scope.beginScope(&env);

    try self.devour(.LEFT_PAREN);

    var params = std.ArrayList(AST.ExternalFunction.Param).init(self.arena);
    if (!self.check(.RIGHT_PAREN)) {
        while (true) {
            const pname = try self.expect(.IDENTIFIER);
            const v = try self.newVar(pname); // pointless fresh.
            const t = try Type.init(self, .{ .Function = uid }).sepTyo();
            try params.append(.{ .pn = v.v, .pt = t });

            if (self.check(.RIGHT_PAREN)) {
                break;
            }
            try self.devour(.COMMA);
        }
    }

    try self.devour(.RIGHT_ARROW);
    const ret = try Type.init(self, .{ .Function = uid }).sepTyo();
    try self.endStmt();

    var definedTVars = std.ArrayList(AST.TVar).init(self.arena);
    var it = self.scope.currentScope().tvars.valueIterator();
    while (it.next()) |tvar| {
        try definedTVars.append(tvar.*);
    }
    self.scope.endScope();

    const scheme = AST.Scheme{
        .tvars = definedTVars.items,
        .associations = &.{},
        .envVars = &.{},
    };

    const extfun = try Common.allocOne(self.arena, AST.ExternalFunction{
        .name = .{
            .name = name,
            .uid = uid,
        },
        .params = params.items,
        .ret = ret,
        .scheme = scheme,
        .anns = annotations,
    });

    try self.scope.currentScope().vars.put(name, .{ .Extern = extfun });
}

// try parse annotations.
// It's possible to divide annotations in multiple lines:
//  #[linkname 'printf']
//  #[dylib 'libc.so']
//  external printf(fmt Ptr Char, x a) -> Void
// TODO: check if statements make sense in context.
fn parseAnnotation(self: *Self) ![]AST.Annotation {
    var annotations = std.ArrayList(AST.Annotation).init(self.arena); // nothing is allocated when there are no annotations.
    while (self.check(.BEGIN_ANNOTATION)) {
        if (!self.check(.RIGHT_SQBR)) while (true) {
            const annName = try self.expect(.IDENTIFIER);

            var annParams = std.ArrayList(Str).init(self.arena);
            while (self.consume(.STRING)) |param| {
                const litWithQuotes = param.literal(self.lexer.source);
                const lit = litWithQuotes[1 .. litWithQuotes.len - 1];
                try annParams.append(lit);
            }

            try annotations.append(.{
                .name = annName.literal(self.lexer.source),
                .params = annParams.items,
            });

            if (self.check(.RIGHT_SQBR)) break;
            try self.devour(.COMMA);
        };

        try self.endStmt();
        self.consumeSeps();
    }

    return annotations.items;
}

fn unitReturn(self: *Self) !AST.Stmt {
    const t = try self.definedType(.Unit);
    const con = &self.typeContext.getType(t).Con.type.stuff.cons[0]; // WARNING: funny casts
    try self.typeContext.unify(t, self.returnType.?);
    return .{ .Return = try self.allocExpr(.{
        .t = t,
        .e = .{ .Con = con },
    }) };
}

// for single line statements that do not contain a body.
// (why? in IF and such, a dedent is equivalent to STMTSEP, so we must accept both. it depends on the type of statement.)
fn endStmt(self: *Self) !void {
    if (self.peek().type != .DEDENT and self.peek().type != .EOF) {
        try self.devour(.STMT_SEP);
    }
}

fn isEndStmt(self: *Self) bool {
    const tt = self.peek().type;
    return tt == .DEDENT or tt == .STMT_SEP;
}

fn consumeSeps(self: *Self) void {
    while (self.check(.STMT_SEP)) {}
}

fn classFunction(self: *Self, classSelf: struct { tvar: AST.TVar, t: AST.Type }, class: *AST.Class) !*AST.ClassFun {
    const funName = try self.expect(.IDENTIFIER);
    const uid = self.gen.classFuns.newUnique();

    self.scope.beginScope(null); // let's capture all tvars.
    var params = std.ArrayList(AST.ClassFun.Param).init(self.arena);
    try self.devour(.LEFT_PAREN);
    if (!self.check(.RIGHT_PAREN)) while (true) {
        // consume identifier if possible.
        if (self.check(.IDENTIFIER)) {}

        try params.append(.{ .t = try Type.init(self, .{ .ClassFunction = uid }).sepTyo() });

        if (self.check(.RIGHT_PAREN)) {
            break;
        }

        try self.devour(.COMMA);
    };

    // another new eye candy - default Unit
    const ret = if (self.check(.RIGHT_ARROW)) try Type.init(self, .{ .ClassFunction = uid }).sepTyo() else try self.definedType(.Unit);

    // constraints
    const constraints_ = try self.constraints();
    _ = constraints_;

    try self.endStmt();
    const tvarsMap = self.scope.currentScope().tvars;
    self.scope.endScope();

    // make a scheme from deze vars yo.
    var tvars = std.ArrayList(AST.TVar).init(self.arena);
    try tvars.append(classSelf.tvar);
    var tvit = tvarsMap.valueIterator();
    while (tvit.next()) |tvar| {
        try tvars.append(tvar.*);
    }

    const scheme = AST.Scheme{
        .tvars = tvars.items,
        .envVars = &.{}, // TEMP
        .associations = &.{}, // NOTE: this will change when class constraints are allowed on functions. EDIT: Or not??? We have no constraints to specify, because these constraints are not based on class function calls. Right now, I'll leave it alone because of our "broken" type system.
    };

    return try Common.allocOne(self.arena, AST.ClassFun{
        .uid = uid,
        .name = .{
            .name = funName.literal(self.lexer.source),
            .uid = self.gen.vars.newUnique(),
        },
        .params = params.items,
        .ret = ret,
        .scheme = scheme,
        .self = classSelf.t,
        .class = class,
    });
}

// parse constraints <= constr (, constr)*
const Constraints = std.HashMap(AST.TVar, std.ArrayList(*AST.Class), AST.TVar.comparator(), std.hash_map.default_max_load_percentage);
fn constraints(self: *Self) !Constraints {
    var constraints_ = Constraints.init(self.arena); // maybe I should just use pointers to vars?
    if (self.check(.LTEQ)) {
        while (true) {
            const classTok = try self.expect(.TYPE);
            const tvt = try self.expect(.IDENTIFIER);

            if (self.maybeLookupType(classTok.literal(self.lexer.source))) |dataOrClass| b: {
                switch (dataOrClass) {
                    .Class => |class| {
                        const tvarName = tvt.literal(self.lexer.source);
                        const tvar = self.scope.currentScope().tvars.get(tvarName) orelse {
                            // NOTE: we're looking only at current scope, so we won't find any named tvars from other functions.
                            try self.errors.append(.{ .ConstrainedNonExistentTVar = .{ .tvname = tvarName } });
                            break :b;
                        };

                        const e = try constraints_.getOrPutValue(tvar, std.ArrayList(*AST.Class).init(self.arena)); // NOTE: source says it's not allocating anything until an element is inserted.

                        try e.value_ptr.append(class);
                    },
                    .Data => unreachable,
                }
            } else {
                unreachable; // TODO: error
            }

            if (!self.check(.COMMA)) break;
        }
    }

    return constraints_;
}

fn deconstruction(self: *Self) !*AST.Decon {
    const decon: AST.Decon = if (self.consume(.IDENTIFIER)) |vn| b: {
        const v = try self.newVar(vn);
        break :b .{
            .t = v.t,
            .d = .{ .Var = v.v },
        };
    } // var
    else if (self.check(.UNDERSCORE)) b: {
        break :b .{
            .t = try self.typeContext.fresh(),
            .d = .{ .None = .{} },
        };
    } // ignore var
    else if (self.consume(.TYPE)) |cn| b: {
        const con = bb: {
            if (self.check(.DOT)) {
                var modpath = std.ArrayList(Str).init(self.arena);
                try modpath.append(cn.literal(self.lexer.source));
                loop: while (true) {
                    if (self.consume(.TYPE)) |tn| {
                        if (self.check(.DOT)) {
                            try modpath.append(tn.literal(self.lexer.source));
                            continue :loop;
                        } else {
                            break :bb try self.instantiateCon(modpath.items, tn);
                        }
                    } else {
                        unreachable; // TODO
                    }
                }
            } else {
                break :bb try self.instantiateCon(&.{}, cn);
            }
        };
        var decons: []*AST.Decon = &.{};
        var args: []AST.Type = &.{};
        if (self.check(.LEFT_PAREN)) {
            var ds = std.ArrayList(*AST.Decon).init(self.arena);
            var tys = std.ArrayList(AST.Type).init(self.arena);

            while (true) { // while1
                const d = try self.deconstruction();
                try ds.append(d);
                try tys.append(d.t);

                if (self.check(.RIGHT_PAREN)) break;

                try self.devour(.COMMA);
            }

            decons = ds.items;
            args = tys.items;
        }

        try self.typeContext.unifyParams(con.tys, args);
        break :b .{
            .t = con.t,
            .d = .{
                .Con = .{
                    .con = con.con,
                    .decons = decons,
                },
            },
        };
    } // con decon
    else if (self.check(.LEFT_BRACE)) b: {
        const t = try self.typeContext.fresh();

        var fields = std.ArrayList(AST.Decon.Field).init(self.arena);
        while (true) {
            const fieldTok = try self.expect(.IDENTIFIER);
            const fieldName = fieldTok.literal(self.lexer.source);

            const fieldTy = try self.typeContext.field(t, fieldName);

            if (self.check(.COLON)) {
                const decon = try self.deconstruction();
                try self.typeContext.unify(fieldTy, decon.t);
                try fields.append(.{
                    .field = fieldName,
                    .decon = decon,
                });
            } else {
                const vnt = try self.newVar(fieldTok);
                try self.typeContext.unify(fieldTy, vnt.t);
                try fields.append(.{ .field = fieldName, .decon = try Common.allocOne(self.arena, AST.Decon{ .t = fieldTy, .d = .{ .Var = vnt.v } }) });
            }
            if (!self.check(.COMMA)) break;
        }
        try self.devour(.RIGHT_BRACE);

        // TODO: right now we only care that the deconstructed struct has all the fields defined. basically { <whatever we write>, ... }
        // later expect the user to write `...` to ignore extra fields.
        break :b .{
            .t = t,
            .d = .{ .Record = fields.items },
        };
    } // record deccon
    else {
        return self.err(*AST.Decon, "Expect decon", .{});
    };

    return try Common.allocOne(self.arena, decon);
}

// jon blow my c0c :3
fn expression(self: *Self) !*AST.Expr {
    return self.precedenceExpression(0);
}

fn precedenceExpression(self: *Self, minPrec: u32) ParserError!*AST.Expr {
    var left = try self.term(minPrec);

    while (true) {
        const node = try self.increasingPrecedenceExpression(left, minPrec);
        if (node == left) break;

        left = node;
    }

    return left;
}

fn finishExpression(self: *Self, leftmost: *AST.Expr) ParserError!*AST.Expr {
    var left = leftmost;

    while (true) {
        const node = try self.increasingPrecedenceExpression(left, 0);
        if (node == left) break;

        left = node;
    }

    return left;
}

fn increasingPrecedenceExpression(self: *Self, left: *AST.Expr, minPrec: u32) !*AST.Expr {
    const optok = self.peek();
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

        if (binop == .PostfixCall) {
            const funt: *AST.Expr = try self.qualified(optok);

            var params = std.ArrayList(*AST.Expr).init(self.arena);
            try params.append(left);
            _ = try self.devour(.LEFT_PAREN);
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

            try self.typeContext.unify(callType, funt.t);

            return self.allocExpr(.{
                .t = retType,
                .e = .{ .Call = .{
                    .callee = funt,
                    .args = params.items,
                } },
            });
        }

        if (binop == .Deref) {
            const ptr = (try self.defined(.Ptr)).dataInst;
            try self.typeContext.unify(ptr.t, left.t);
            return self.allocExpr(.{
                .t = ptr.tyArgs[0],
                .e = .{
                    .UnOp = .{ .op = .Deref, .e = left },
                },
            });
        }

        if (binop == .RecordAccess) {
            const mem = try self.expect(.IDENTIFIER);
            const t = try self.typeContext.field(left.t, mem.literal(self.lexer.source));
            return self.allocExpr(.{
                .t = t,
                .e = .{ .UnOp = .{
                    .e = left,
                    .op = .{
                        .Access = mem.literal(self.lexer.source),
                    },
                } },
            });
        }

        if (binop == .As) {
            const t = try Type.init(self, null).sepTyo();
            try self.typeContext.unify(t, left.t);
            return self.allocExpr(.{
                .t = t,
                // NOTE: we generate a new node only for error reporting. we don't really need it otherwise.
                .e = .{ .UnOp = .{
                    .e = left,
                    .op = .{ .As = t },
                } },
            });
        }

        const right = try self.precedenceExpression(nextPrec);

        const exprType = switch (binop) {
            .Plus,
            .Minus,
            .Times,
            .Divide,
            => b: {
                const intTy = try self.definedType(.Int);
                try self.typeContext.unify(left.t, intTy);
                try self.typeContext.unify(right.t, intTy);
                break :b intTy;
            },

            .GreaterThan,
            .LessThan,
            .GreaterEqualThan,
            .LessEqualThan,
            => b: {
                const intTy = try self.definedType(.Int);
                try self.typeContext.unify(left.t, intTy);
                try self.typeContext.unify(right.t, intTy);
                const boolTy = try self.definedType(.Bool);
                break :b boolTy;
            },

            .Equals, .NotEquals => b: {
                try self.typeContext.unify(left.t, right.t);
                break :b try self.definedType(.Bool);
            },

            else => unreachable,
        };

        return self.allocExpr(.{
            .t = exprType,
            .e = .{ .BinOp = .{ .op = binop, .l = left, .r = right } },
        });
    }
}

fn term(self: *Self, minPrec: u32) !*AST.Expr {
    // parse unary prefix.
    // I'm not sure if this is good, but getting a reference of something is not really done outside of function calls / constructors.
    // If *nothing* has been parsed, you may get a reference.
    if (minPrec == 0 and self.check(.REF)) {
        const n = try self.expression();
        const ptr = (try self.defined(.Ptr)).dataInst;
        try self.typeContext.unify(ptr.tyArgs[0], n.t);
        return self.allocExpr(.{
            .e = .{ .UnOp = .{ .op = .Ref, .e = n } },
            .t = ptr.t,
        });
    }

    // TODO: maybe make some function to automatically allocate memory when expr succeeds?
    if (self.consume(.IDENTIFIER)) |v| {
        const dv = try self.instantiateVar(&.{}, v);
        return self.allocExpr(.{
            .t = dv.t,
            .e = .{ .Var = .{ .v = dv.v, .match = dv.m } },
        });
    } // var
    else if (self.consume(.INTRINSIC)) |intrTok| {
        // for intrinsics, we must IMMEDIATELY parse the call - we don't want to deal with them being passed around.
        if (Intrinsic.findByName(intrTok.literal(self.lexer.source)[1..])) |intr| {
            // parse any required arguments brah.
            var args = std.ArrayList(*AST.Expr).init(self.arena);
            if (intr.args > 0) {
                try self.devour(.LEFT_PAREN);
                for (0..intr.args) |i| {
                    try args.append(try self.expression());
                    if (i != intr.args - 1) {
                        try self.devour(.COMMA);
                    } else {
                        try self.devour(.RIGHT_PAREN);
                    }
                }
            }

            const t = switch (intr.ty) {
                .cast => try self.typeContext.fresh(),
                .undefined => try self.typeContext.fresh(),
                .@"size-of" => try self.definedType(.Int),
                .@"offset-ptr" => b: {
                    // arg 1
                    const ptr = (try self.defined(.Ptr)).dataInst;
                    std.debug.assert(args.items.len == 2); // right now, we have a parse error that aborts execution if number of args is not correct. When it changes,must change this.
                    try self.typeContext.unify(ptr.t, args.items[0].t);

                    // arg 2
                    try self.typeContext.unify(try self.definedType(.Int), args.items[1].t);

                    break :b ptr.t;
                },
            };

            return self.allocExpr(.{
                .t = t,
                .e = .{
                    .Intrinsic = .{
                        .intr = intr,
                        .args = args.items,
                    },
                },
            });
        } else {
            std.debug.print("{s}\n", .{intrTok.literal(self.lexer.source)[1..]});
            unreachable; // Error!
        }
    } // intrinsic
    else if (self.consume(.TYPE)) |con| {
        return try self.qualified(con);
    } // con
    else if (self.consume(.INTEGER)) |i| {
        return self.allocExpr(.{
            .t = try self.definedType(.Int),
            .e = .{ .Int = std.fmt.parseInt(i64, i.literal(self.lexer.source), 10) catch unreachable },
        });
    } // var
    else if (self.consume(.STRING)) |s| {
        return try self.stringLiteral(s);
    } // string
    else if (self.check(.LEFT_PAREN)) {
        const expr = try self.expression();
        try self.devour(.RIGHT_PAREN);
        return expr;
    } // grouping
    else if (self.check(.LEFT_BRACE)) {
        const definitions = try self.someRecordDefinition();

        // TODO: deduplicate (and, in this case, error out)
        const typeFields = try self.arena.alloc(AST.TypeF(AST.Type).Field, definitions.len);
        for (definitions, 0..) |def, i| {
            typeFields[i] = .{ .t = def.value.t, .field = def.field };
        }
        const t = try self.typeContext.newType(.{
            .Anon = typeFields,
        });

        return self.allocExpr(.{ .e = .{ .AnonymousRecord = definitions }, .t = t });
    } // anonymous struct.
    else {
        return self.err(*AST.Expr, "Unexpected term", .{});
    }
}

fn qualified(self: *Self, first: Token) !*AST.Expr {
    if (first.type == .IDENTIFIER) {
        const dv = try self.instantiateVar(&.{}, first);
        return self.allocExpr(.{
            .t = dv.t,
            .e = .{ .Var = .{ .v = dv.v, .match = dv.m } },
        });
    } // single identifier
    else if (first.type == .TYPE) {
        if (self.check(.DOT)) {
            // fallthrough
        } else if (self.check(.LEFT_BRACE)) {
            return try self.namedRecordDefinition(&.{}, first);
        } else {
            return try self.constructorExpression(&.{}, first);
        }
    } // single constructor
    else unreachable;

    var modpath = std.ArrayList(Str).init(self.arena);
    try modpath.append(first.literal(self.lexer.source));

    loop: while (true) {
        if (self.consume(.TYPE)) |possibleCon| {
            if (self.check(.DOT)) {
                try modpath.append(possibleCon.literal(self.lexer.source));
                continue :loop;
            } else if (self.check(.LEFT_BRACE)) {
                return try self.namedRecordDefinition(modpath.items, possibleCon);
            } else {
                return try self.constructorExpression(modpath.items, possibleCon);
            }
        } else if (self.consume(.IDENTIFIER)) |v| {
            const dv = try self.instantiateVar(modpath.items, v);
            return self.allocExpr(.{
                .t = dv.t,
                .e = .{ .Var = .{ .v = dv.v, .match = dv.m } },
            });
        } else {
            return self.err(*AST.Expr, "Unfinished module shit.\n", .{});
        }
    }
}

fn namedRecordDefinition(self: *Self, modpath: Module.Path, name: Token) !*AST.Expr {
    const definitions = try self.someRecordDefinition();

    // instantiate it.
    const mDataOrClass = try self.findQualifiedDataOrClass(modpath, name);
    if (mDataOrClass) |dataOrClass| {
        switch (dataOrClass) {
            .Data => |data| {
                switch (data.stuff) {
                    .recs => |dataFields| {
                        const dataInst = try self.instantiateData(data);
                        const match = dataInst.match;

                        // check if all fields were defined
                        for (dataFields) |dataField| {
                            for (definitions) |def| {
                                if (Common.streq(dataField.field, def.field)) {
                                    try self.typeContext.unify(def.value.t, try self.typeContext.mapType(match, dataField.t));
                                    break;
                                }
                            } else {
                                //missing field
                                try self.errors.append(.{ .MissingField = .{ .field = dataField.field } });
                            }
                        }

                        return self.allocExpr(.{ .t = dataInst.t, .e = .{
                            .NamedRecord = .{
                                .data = data,
                                .fields = definitions,
                            },
                        } });
                    },
                    .cons => {
                        try self.errors.append(.{ .DataIsNotARecord = .{ .data = data } });
                        // fallthrough to return placeholder.
                    },
                }
            },
            .Class => {
                // ~fallthrough and return placeholder.~
                // I THOUGHT THIS WOULD BE AN ERROR.
                // BUT SINCE WE WANT ANONYMOUS STRUCTS TO COERCE TO TYPES, MAYBE THIS SHOULD SPAWN A
                //  - fresh type
                //  - with class constraint
                //  - with field constraints.
                unreachable;
            },
        }
    } else {
        // error already reported.
        // fall to add placeholder.
    }
    // self.instantiateCon(modpath: Module.Path, conTok: Token)
    unreachable;
}

// either anonymous or normal :)
// checks for duplicates.
fn someRecordDefinition(self: *Self) ![]AST.Expr.Field {
    var definitions = std.ArrayList(AST.Expr.Field).init(self.arena);
    while (true) {
        const fieldTok = try self.expect(.IDENTIFIER);
        const fieldName = fieldTok.literal(self.lexer.source);
        try self.devour(.COLON);
        const expr = try self.expression();

        // check for duplication.
        for (definitions.items) |field| {
            if (Common.streq(field.field, fieldName)) {
                try self.errors.append(.{
                    .DuplicateField = .{ .field = fieldName },
                });
                break;
            }
        } else {
            // not a duplicate.
            try definitions.append(.{ .field = fieldName, .value = expr });
        }

        if (!self.check(.COMMA)) break;
    }
    try self.devour(.RIGHT_BRACE);

    return definitions.items;
}

// right now only used for records. In the future, will be used for qualifying types themselves.
// ALSO, TODO we might abstract away the stuff about getting the module, because it's annoying and it's not immediately obvious how I should handle that error. So, a fn (modpath) -> ?Module (but also throw error when module == null)
fn findQualifiedDataOrClass(self: *Self, modpath: Module.Path, tok: Token) !?Module.DataOrClass {
    const name = tok.literal(self.lexer.source);
    if (modpath.len == 0) {
        if (self.maybeLookupType(name)) |dataOrClass| {
            return dataOrClass;
        } else {
            try self.errors.append(.{ .UndefinedType = .{
                .typename = name,
                .loc = tok.toLocation(self.lexer.source),
            } });
            return null;
        }
    } else {
        if (self.importedModules.get(modpath)) |mmod| {
            if (mmod) |mod| {
                if (mod.lookupData(name)) |data| {
                    return data;
                } else {
                    try self.errors.append(.{
                        .UndefinedType = .{
                            .typename = name,
                            .loc = tok.toLocation(self.lexer.source),
                        },
                    });
                    return null;
                }
            } else {
                // circular dep??
                // return null;
                unreachable;
            }
        } else {
            try self.errors.append(.{ .UnimportedModule = .{} });
            return null;
        }
    }

    unreachable;
}

// properly instantiates a constructor expression. (remembers function types n shit)
fn constructorExpression(self: *Self, modpath: Module.Path, name: Token) !*AST.Expr {
    const ct = try self.instantiateCon(modpath, name);
    const t = if (ct.tys.len == 0) ct.t else try self.typeContext.newType(.{
        .Fun = .{
            .args = ct.tys,
            .ret = ct.t,
            .env = try self.typeContext.newEnv(&.{}), // nocheckin: we have to figure out if the env is the same.
        },
    });
    return self.allocExpr(.{
        .e = .{ .Con = ct.con },
        .t = t,
    });
}

// TODO: handle errors in literals
fn stringLiteral(self: *Self, st: Token) !*AST.Expr {
    const og = st.literal(self.lexer.source); // this includes single quotes
    var e: ?*AST.Expr = null;
    var s = std.ArrayList(u8).init(self.arena);
    var i: usize = 1;
    var last: usize = i;
    while (i < og.len - 1) {
        const ci = i;
        const c = og[ci];
        if (c == '\\') {
            i += 2;
            switch (og[i - 1]) {
                '(' => {
                    const start = i;
                    while (og[i] != ')') i += 1;
                    const end = i;
                    i += 1;

                    if (last != ci) {
                        const se = try self.allocExpr(.{
                            .e = .{
                                .Str = try s.toOwnedSlice(),
                            },
                            .t = try self.definedType(.ConstStr),
                        });
                        if (e) |ee| {
                            e = try self.strConcat(
                                ee,
                                se,
                            );
                        } else {
                            e = se;
                        }
                    }

                    const v = try self.instantiateVar(&.{}, .{ .from = st.from + start, .to = st.from + end, .type = .IDENTIFIER });

                    const ve = try self.allocExpr(.{
                        .e = .{ .Var = .{
                            .v = v.v,
                            .match = v.m,
                        } },
                        .t = v.t,
                    });

                    if (e) |ee| {
                        e = try self.strConcat(ee, ve);
                    } else {
                        e = ve;
                    }
                    last = i;
                },
                't' => try s.append('\t'),
                'n' => try s.append('\n'),
                '\\' => try s.append('\\'),
                else => unreachable, // TODO handle errors
            }
        } else {
            try s.append(c);
            i += 1;
        }
    }

    if (last != i) {
        const se = try self.allocExpr(.{
            .e = .{
                .Str = try s.toOwnedSlice(),
            },
            .t = try self.definedType(.ConstStr),
        });
        if (e) |ee| {
            e = try self.strConcat(
                ee,
                se,
            );
        } else {
            e = se;
        }
    }
    return e orelse self.allocExpr(.{ .e = .{ .Str = &.{} }, .t = try self.definedType(.ConstStr) });
}

fn strConcat(self: *Self, l: *AST.Expr, r: *AST.Expr) !*AST.Expr {
    const sc = try self.defined(.StrConcat);
    const sci = sc.dataInst;
    try self.typeContext.unify(sci.tyArgs[0], l.t);
    try self.typeContext.unify(sci.tyArgs[1], r.t);
    const args = try self.arena.alloc(*AST.Expr, 2);
    args[0] = l;
    args[1] = r;
    return self.allocExpr(.{
        .t = sci.t,
        .e = .{ .Call = .{
            .callee = try self.allocExpr(.{
                .t = try self.typeContext.newType(.{ .Fun = .{
                    .ret = sci.t,
                    .args = sci.tyArgs,
                    .env = try self.typeContext.newEnv(&.{}),
                } }),
                .e = .{ .Con = &sc.data.stuff.cons[0] },
            }),
            .args = args,
        } },
    });
}

fn allocExpr(self: *const Self, ev: AST.Expr) error{OutOfMemory}!*AST.Expr {
    const e = try self.arena.create(AST.Expr);
    e.* = ev;
    return e;
}

fn getBinOp(tok: Token) ?AST.BinOp {
    return switch (tok.type) {
        .PLUS => .Plus,
        .MINUS => .Minus,
        .TIMES => .Times,
        .EQEQ => .Equals,
        .LT => .LessThan,
        .GT => .GreaterThan,
        .LEFT_PAREN => .Call,
        .REF => .Deref,
        .IDENTIFIER => .PostfixCall,
        .TYPE => .PostfixCall,
        .DOT => .RecordAccess,
        .AS => .As,
        else => null,
    };
}

// here and not in AST.BinOp, because precedence only matters for parsing.
fn binOpPrecedence(op: AST.BinOp) u32 {
    return switch (op) {
        // 0 means it won't be consumed, like a sentinel value.
        .As => 1,

        .Equals => 3,
        .LessThan => 3,
        .GreaterThan => 3,

        .Plus => 4,
        .Minus => 4,
        .Times => 6,
        .Divide => 6,

        .Call => 10,
        .RecordAccess => 10,
        .Deref => 10,
        .PostfixCall => 10,
        else => unreachable,
    };
}

const Type = struct {
    binding: ?AST.TVar.Binding,
    parser: *Self,

    fn init(self: *Self, binding: ?AST.TVar.Binding) @This() {
        return .{ .binding = binding, .parser = self };
    }

    // type-o
    fn typ(this: *const @This()) ParserError!AST.Type {
        const self = this.parser;
        // temp
        if (self.consume(.TYPE)) |ty| {
            const ity = try self.instantiateType(ty);
            if (ity.tyArgs.len != 0) {
                try self.errors.append(.{ .MismatchingKind = .{ .data = ity.data, .expect = ity.tyArgs.len, .actual = 0 } });
            }
            return ity.t;
        } else if (self.consume(.IDENTIFIER)) |tv| { // TVAR
            return self.typeContext.newType(.{ .TVar = try self.lookupTVar(tv, this.binding) });
        } else if (self.check(.LEFT_PAREN)) {
            const ty = try this.sepTyo();
            try self.devour(.RIGHT_PAREN);
            return ty;
        } else if (self.check(.UNDERSCORE)) {
            if (self.selfType) |t| {
                return t;
            } else {
                // TODO: signal error
                unreachable;
            }
        } else if (self.check(.LEFT_BRACE)) {
            var fields = std.ArrayList(AST.TypeF(AST.Type).Field).init(self.arena);
            while (true) {
                const field = try self.expect(.IDENTIFIER);
                try self.devour(.COLON);
                const t = try this.sepTyo();
                try fields.append(.{
                    .t = t,
                    .field = field.literal(self.lexer.source),
                });

                if (!self.check(.COMMA)) break;
            }
            try self.devour(.RIGHT_BRACE);

            return try self.typeContext.newType(.{ .Anon = fields.items });
        } else {
            return try self.err(AST.Type, "Expect type", .{});
        }
        unreachable;
    }

    fn sepTyo(this: *const @This()) !AST.Type {
        const self = this.parser;
        if (self.consume(.TYPE)) |tyName| {
            var tyArgs = std.ArrayList(AST.Type).init(self.arena);
            while (true) {
                const tokType = self.peek().type;
                if (!(tokType == .LEFT_PAREN or tokType == .TYPE or tokType == .IDENTIFIER or tokType == .UNDERSCORE)) { // bad bad works
                    break;
                }

                try tyArgs.append(try this.typ());
            }

            const ty = try self.instantiateType(tyName);

            // simply check arity.
            try self.typeContext.unifyParams(ty.tyArgs, tyArgs.items);

            // there's a possibility it's a function!
            if (self.check(.RIGHT_ARROW)) {
                const args = try self.arena.alloc(AST.Type, 1);
                args[0] = ty.t;
                const ret = try this.sepTyo();
                return try self.typeContext.newType(.{
                    .Fun = .{
                        .args = args,
                        .ret = ret,
                        .env = if (this.binding != null)
                            // in general case
                            try self.typeContext.newEnv(null)
                        else
                            // in external functions, assume no environment.
                            try self.typeContext.newEnv(&.{}),
                    },
                });
            } else {
                return ty.t;
            }
        } else if (self.check(.LEFT_PAREN)) {
            // try parse function (but it can also be an extra paren!)
            var args = std.ArrayList(AST.Type).init(self.arena);
            while (!self.check(.RIGHT_PAREN)) {
                try args.append(try this.sepTyo());
                if (self.peek().type != .RIGHT_PAREN) {
                    try self.devour(.COMMA);
                }
            }

            if (self.check(.RIGHT_ARROW)) {
                const ret = try this.typ();
                return try self.typeContext.newType(.{ .Fun = .{
                    .ret = ret,
                    .args = args.items,
                    .env = try self.typeContext.newEnv(null),
                } });
            } else if (args.items.len == 1) { // just parens!
                return args.items[0];
            } else if (args.items.len == 0) {
                // only Unit tuple is supported.
                return try self.definedType(.Unit);
            } else { // this LOOKS like a tuple, but we don't support tuples yet!
                try self.errors.append(.{ .TuplesNotYetSupported = .{} });
                return self.typeContext.fresh();
            }
        } else if (self.consume(.IDENTIFIER)) |tv| {
            const tvt = try self.typeContext.newType(.{
                .TVar = try self.lookupTVar(tv, this.binding),
            });
            if (self.check(.RIGHT_ARROW)) {
                const args = try self.arena.alloc(AST.Type, 1);
                args[0] = tvt;
                const ret = try this.sepTyo();
                return self.typeContext.newType(.{ .Fun = .{
                    .args = args,
                    .ret = ret,
                    .env = try self.typeContext.newEnv(null),
                } });
            } else {
                return tvt;
            }
        } else {
            return try this.typ();
        }
        unreachable;
    }
};

// resolver zone
fn loadModuleFromPath(self: *Self, path: Module.Path) !?Module {
    const mmod = try self.modules.loadModule(.{ .ByModulePath = .{ .base = self.base, .path = path } }, .{});
    try self.importedModules.put(path, mmod);

    // automatically add instances (like muh haskells)
    if (mmod) |mod| {
        var it = mod.exports.instances.iterator();
        while (it.next()) |inst| {
            try self.scope.currentScope().instances.put(inst.key_ptr.*, inst.value_ptr.*);
        }
    }
    return mmod;
}

// VARS
fn newVar(self: *@This(), varTok: Token) !Module.VarAndType {
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

fn lookupVar(self: *Self, modpath: Module.Path, varTok: Token) !struct {
    vorf: Module.VarOrFun,
    sc: ?*CurrentScope,
} {
    const varName = varTok.literal(self.lexer.source);
    if (modpath.len == 0) {
        var lastVars = self.scope.scopes.iterateFromTop();
        while (lastVars.nextPtr()) |cursc| {
            if (cursc.vars.get(varName)) |vorf| {
                return .{ .vorf = vorf, .sc = cursc };
            }
        }
    } else {
        if (self.importedModules.get(modpath)) |mmod| {
            if (mmod) |mod| {
                if (mod.lookupVar(varName)) |vorf| {
                    return .{
                        .vorf = vorf,
                        .sc = null,
                    };
                } else {
                    // FALLTHROUGH.
                    // TODO: set source module to be of that found module.
                }
            } else {
                // i dunno, probably some other error. Ignore ig?
                unreachable; // TEMP. I JUST NEED TO SEE WHEN THAT HAPPENS.
            }
        } else {
            try self.errors.append(.{ .UnimportedModule = .{} });
        }
    }

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
    return .{
        .vorf = .{
            .Var = .{ .v = placeholderVar, .t = t },
        },
        .sc = null,
    };
}

fn instantiateVar(self: *@This(), modpath: Module.Path, varTok: Token) !struct {
    v: AST.Expr.VarType,
    t: AST.Type,
    m: *AST.Match(AST.Type),
} {
    const vorfAndScope = try self.lookupVar(modpath, varTok);
    const vorf = vorfAndScope.vorf;
    const cursc = vorfAndScope.sc;
    const varInst: AST.VarInst = switch (vorf) {
        .Var => |vt| .{
            .v = .{ .Var = vt.v },
            .t = vt.t,
            .m = try Common.allocOne(self.arena, AST.Match(AST.Type).empty(AST.Scheme.empty())),
        },
        .Fun => |fun| b: {
            const funTyAndMatch = try self.instantiateFunction(fun);
            break :b .{
                .v = .{ .Fun = fun },
                .t = funTyAndMatch.t,
                .m = funTyAndMatch.m,
            };
        },

        .ClassFun => |cfun| b: {
            const match = try self.instantiateScheme(cfun.scheme);

            // mk new, instantiated type
            var params = std.ArrayList(AST.Type).init(self.arena);
            for (cfun.params) |p| {
                try params.append(try self.typeContext.mapType(match, p.t));
            }

            const ret = try self.typeContext.mapType(match, cfun.ret);
            const funTy = try self.typeContext.newType(.{ .Fun = .{
                .args = params.items,
                .ret = ret,
                .env = try self.typeContext.newEnv(null),
            } });

            const classSelf = try self.typeContext.mapType(match, cfun.self);

            const instances = try self.getInstancesForClass(cfun.class);

            const ref: *?AST.Match(AST.Type).AssocRef = try Common.allocOne(self.arena, @as(?AST.Match(AST.Type).AssocRef, null));
            const varInst = AST.VarInst{
                .v = .{
                    .ClassFun = .{
                        .cfun = cfun,
                        .ref = ref,
                    },
                },
                .t = funTy,
                .m = match,
            };

            try self.addAssociation(.{
                .from = classSelf,
                .to = funTy,
                .classFun = cfun,
                .instances = instances,
                .ref = ref,
            });
            break :b varInst;
        },

        .Extern => |extfun| {
            const match = try self.instantiateScheme(extfun.scheme);

            var params = std.ArrayList(AST.Type).init(self.arena);
            for (extfun.params) |p| {
                try params.append(try self.typeContext.mapType(match, p.pt));
            }

            const ret = try self.typeContext.mapType(match, extfun.ret);
            const funTy = try self.typeContext.newType(.{
                .Fun = .{
                    .args = params.items,
                    .ret = ret,
                    .env = try self.typeContext.newEnv(&.{}),
                },
            });

            // NOTE: we just return. External functions are not added to the environment.
            return .{
                .v = .{
                    .ExternalFun = extfun,
                },
                .t = funTy,
                .m = match,
            };
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
    return .{
        .v = switch (varInst.v) {
            .Var => |vv| .{ .Var = vv },
            .Fun => |fun| .{ .Fun = fun },
            .ClassFun => |cfun| .{
                .ClassFun = .{
                    .cfun = cfun.cfun,
                    .ref = cfun.ref,
                },
            },
        },
        .t = varInst.t,
        .m = varInst.m,
    };
}

fn instantiateFunction(self: *Self, fun: *AST.Function) !struct { t: AST.Type, m: *AST.Match(AST.Type) } {
    const match = try self.instantiateScheme(fun.scheme);

    // mk normal, uninstantiated type.
    var params = std.ArrayList(AST.Type).init(self.arena);
    for (fun.params) |p| {
        try params.append(p.t);
    }

    const funTy = try self.typeContext.newType(.{
        .Fun = .{
            .args = params.items,
            .ret = fun.ret,
            .env = try self.typeContext.newEnv(fun.env), // this is sussy. maybe we should also keep the "newEnv" still.
        },
    });
    return .{ .t = try self.typeContext.mapType(match, funTy), .m = match };
}

// CURRENTLY VERY SLOW!
fn getInstancesForClass(self: *Self, class: *AST.Class) !Module.DataInstance {
    // OPTIMIZATION POSSIBILITY: Instance declarations happen often in sequence, then are used. Right now, the list is copied each time. Instead, we can make instances copied on demand.
    //  1. lots of instance declarations, then class:
    //      copy hashmap and insert into associations and assign to function class
    //  2. lots of function class calls, then instance
    //      copy hashmap and then insert into associations and add instance
    // basically, when unchanged, pass the current one and create a copy when it needs to be changed.
    var foundInsts = Module.DataInstance.init(self.arena);
    var scopeIt = self.scope.scopes.iterateFromTop();
    while (scopeIt.nextPtr()) |sc| {
        if (sc.instances.getPtr(class)) |insts| {
            var instIt = insts.iterator();
            while (instIt.next()) |inst| {
                try foundInsts.put(inst.key_ptr.*, inst.value_ptr.*);
            }
        }
    }

    return foundInsts;
}

fn solveAvailableConstraints(self: *Self) !void {
    var hadChanges = true; // true, because we need to enter the loop at least once.
    while (hadChanges) {
        hadChanges = false;

        // copy array, so that modifications won't affect it.
        const currentAssocs = try self.arena.alloc(Association, self.associations.items.len);
        defer self.arena.free(currentAssocs); // noop with arena. but reminds me of currentAssocs's lifetime.
        @memcpy(currentAssocs, self.associations.items);

        var i: usize = 0; // for future me: we are modifying i inside the loop, so we can't make a for(,) zip thing.
        for (currentAssocs) |assoc| {
            defer i +%= 1;
            switch (self.typeContext.getType(assoc.from)) {
                .Con => |con| {
                    if (assoc.instances.get(con.type)) |inst| {
                        // NOTE: modifying self.associations while iterating assocs.
                        const fun: *AST.Function = b: {
                            for (inst.instFuns) |instFun| {
                                if (instFun.classFunId == assoc.classFun.uid) {
                                    const fun = instFun.fun;
                                    break :b fun;
                                }
                            }

                            unreachable;
                        };
                        const funTyAndMatch = try self.instantiateFunction(fun);
                        const funTy = funTyAndMatch.t;
                        try self.typeContext.unify(assoc.to, funTy);

                        assoc.ref.* = .{ .InstFun = .{ .fun = fun, .m = funTyAndMatch.m } };

                        _ = self.associations.orderedRemove(i); // TODO: not very efficient with normal ArrayList.
                        i -%= 1; // make sure to adjust index.
                    } else {
                        // error
                        try self.errors.append(.{ .CouldNotFindInstanceForType = .{
                            .data = con.type,
                            .class = assoc.classFun.class,
                        } });
                        assoc.ref.* = null;
                        _ = self.associations.orderedRemove(i);
                    }
                    hadChanges = true;
                },
                .Anon => unreachable, // ??? i dunno
                .Fun => unreachable, // error!
                .TVar => |tv| { // what should i do here?
                    // const targetClass = assoc.classFun.class;
                    // // for (tv.classes.items) |class| {
                    // //     if (targetClass == class) break;
                    // // } else {
                    // // }

                    // if (currentFunction) |funId| {
                    //     if (std.meta.eql(tv.binding, AST.TVar.Binding{ .Function = funId })) {
                    //         continue;
                    //     }
                    // }
                    _ = tv;

                    // unreachable; // should this happen?

                    // assoc.ref.* = null;
                    // _ = self.associations.orderedRemove(i);
                    // try self.errors.append(.{ .TVarDoesNotImplementClass = .{ .tv = tv, .class = targetClass } });
                    // NOTE: don't assign to hadChanges, because this doesn't impact anything.
                },
                .TyVar => {},
            }
        }
    }
}

// TYPES
// (requires US to generate a new unique.)
fn newData(self: *@This(), data: *AST.Data) !void {
    // add type
    try self.scope.currentScope().types.put(data.name, .{ .Data = data });

    // add constructors
    switch (data.stuff) {
        .cons => |cons| {
            for (cons) |*con| {
                try self.scope.currentScope().cons.put(con.name, con);
            }
        },

        .recs => {
            // do nothing :)
        },
    }
}

fn newClass(self: *Self, class: *AST.Class) !void {
    try self.scope.currentScope().types.put(
        class.name,
        .{ .Class = class },
    );

    for (class.classFuns) |classFun| {
        try self.scope.currentScope().vars.put(classFun.name.name, .{ .ClassFun = classFun });
    }
}

const DataInst = struct {
    t: AST.Type,
    tyArgs: []AST.Type,
    match: *AST.Match(AST.Type),
};
fn instantiateData(self: *Self, data: *AST.Data) !DataInst {
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

fn instantiateType(self: *Self, tyTok: Token) !DataShit {
    const typename = tyTok.literal(self.lexer.source);
    if (self.maybeLookupType(typename)) |dataOrClass| {
        switch (dataOrClass) {
            .Data => |data| {
                const dt = try self.instantiateData(data);
                return .{
                    .data = data,
                    .t = dt.t,
                    .tyArgs = dt.tyArgs,
                    .match = dt.match,
                };
            },

            .Class => unreachable,
        }
    } else {
        return try self.newPlaceholderType(typename, .{
            .from = tyTok.from,
            .to = tyTok.to,
            .source = self.lexer.source,
        });
    }
}

const DataShit = struct {
    data: *AST.Data,
    t: AST.Type,
    tyArgs: []AST.Type,
    match: *AST.Match(AST.Type),
};
fn newPlaceholderType(self: *Self, typename: Str, location: Common.Location) !DataShit {
    const placeholderType = try Common.allocOne(self.arena, AST.Data{
        .name = typename,
        .uid = self.gen.vars.newUnique(),
        .stuff = .{ .cons = &.{} },
        .scheme = AST.Scheme.empty(),
    });
    try self.errors.append(.{
        .UndefinedType = .{ .typename = typename, .loc = location },
    });

    const match = try Common.allocOne(self.arena, AST.Match(AST.Type).empty(placeholderType.scheme));
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

fn maybeLookupType(self: *Self, typename: Str) ?Module.DataOrClass {
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
fn newTVar(self: *@This(), tvname: Str, binding: ?AST.TVar.Binding) !AST.TVar {
    const tv: AST.TVar = .{
        .uid = self.gen.tvars.newUnique(),
        .name = tvname,
        .binding = binding,
        .inferred = false,
        .fields = &.{},
    };
    try self.scope.currentScope().tvars.put(tvname, tv);
    return tv;
}

// basically, sometimes when we look up tvars in declarations, we want to define them. slightly hacky, but makes stuff easier.
fn lookupTVar(self: *Self, tvTok: Token, binding: ?AST.TVar.Binding) !AST.TVar {
    const tvname = tvTok.literal(self.lexer.source);
    var lastScopes = self.scope.scopes.iterateFromTop();
    while (lastScopes.next()) |cursc| {
        if (cursc.tvars.get(tvname)) |tv| {
            return tv;
        }
    } else {
        // create a new var then.
        if (binding == null) {
            try self.errors.append(.{ .UndefinedTVar = .{
                .tvname = tvname,
                .loc = .{
                    .from = tvTok.from,
                    .to = tvTok.to,
                    .source = self.lexer.source,
                },
            } });
        }
        return try self.newTVar(tvTok.literal(self.lexer.source), binding);
    }
}

// CNS
fn newCon(self: *@This(), con: *AST.Con) !void {
    try self.scope.currentScope().cons.put(con.name, con);
}

fn instantiateCon(self: *@This(), modpath: Module.Path, conTok: Token) !struct {
    con: *AST.Con,
    t: AST.Type,
    tys: []AST.Type,
} {
    const conName = conTok.literal(self.lexer.source);
    const con = if (modpath.len == 0) b: {
        var lastVars = self.scope.scopes.iterateFromTop();
        while (lastVars.next()) |cursc| {
            if (cursc.cons.get(conName)) |con|
                break :b con;
        } else {
            const data = try self.arena.create(AST.Data);
            data.uid = self.gen.types.newUnique();
            data.name = conName;
            data.stuff = .{ .cons = try self.arena.alloc(AST.Con, 1) };
            data.stuff.cons[0] = .{
                .uid = self.gen.cons.newUnique(),
                .name = conName,
                .tys = &.{},
                .data = data,
                .tagValue = 0,
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
            return .{ .con = &data.stuff.cons[0], .t = try self.typeContext.fresh(), .tys = &.{} };
        }
    } else b: {
        if (self.importedModules.get(modpath)) |mmod| {
            if (mmod) |mod| {
                if (mod.lookupCon(conName)) |con| {
                    break :b con;
                } else {
                    unreachable; // TODO ERROR: could not find constructor.
                }
            } else {
                unreachable; // this might be a circular dependency?
            }
        } else {
            unreachable; // TODO ERROR: Module not imported.
        }
    };

    const dt = try self.instantiateData(con.data);

    // found con. now we instantiate it.
    if (con.tys.len == 0) {
        return .{ .con = con, .t = dt.t, .tys = &.{} };
    } else {
        // NOTE: function type making moved to .Con case in expression()
        var args = std.ArrayList(AST.Type).init(self.arena);
        for (con.tys) |ty| {
            try args.append(try self.typeContext.mapType(dt.match, ty));
        }
        return .{
            .con = con,
            .t = dt.t,
            .tys = args.items,
        };
    }
}

// SCHEMES
fn instantiateScheme(self: *Self, scheme: AST.Scheme) !*AST.Match(AST.Type) {
    const tvars = try self.arena.alloc(AST.Type, scheme.tvars.len);
    for (scheme.tvars, 0..) |_, i| {
        tvars[i] = try self.typeContext.fresh();
    }

    const envVars = try self.arena.alloc(AST.EnvRef, scheme.envVars.len);
    for (scheme.envVars, 0..) |_, i| {
        envVars[i] = try self.typeContext.newEnv(null);
    }

    const assocs = try self.arena.alloc(?AST.Match(AST.Type).AssocRef, scheme.associations.len);
    for (assocs) |*a| {
        a.* = null; // default VALUE YO
    }

    const tvarMatch = AST.Match(AST.Type){
        .tvars = tvars,
        .envVars = envVars,
        .assocs = assocs,
        .scheme = scheme,
    };

    // should prolly add assocs to "Match", but we don't need em yet.
    for (scheme.associations, assocs) |assoc, *ref| {
        try self.addAssociation(.{
            .classFun = assoc.classFun,
            .from = try self.typeContext.mapType(&tvarMatch, try self.typeContext.newType(
                .{ .TVar = assoc.depends },
            )),
            .to = try self.typeContext.mapType(&tvarMatch, assoc.to),
            .instances = try self.getInstancesForClass(assoc.classFun.class),
            .ref = ref,
        });
    }

    // now members fields
    for (scheme.tvars, tvars) |tv, tyv| {
        for (tv.fields) |field| {
            const fieldTy = try self.typeContext.field(tyv, field.field);
            try self.typeContext.unify(fieldTy, try self.typeContext.mapType(&tvarMatch, field.t));
        }
    }

    return try Common.allocOne(self.arena, tvarMatch);
}

const FTVs = struct {
    const TyVars = Set(FTV, struct {
        pub fn eql(ctx: @This(), a: FTV, b: FTV) bool {
            _ = ctx;
            return a.tyv.uid == b.tyv.uid;
        }

        pub fn hash(ctx: @This(), k: FTV) u64 {
            _ = ctx;
            // return @truncate(k.tyv);
            return k.tyv.uid;
        }
    });
    const Envs = Set(AST.EnvRef, struct {
        pub fn eql(ctx: @This(), a: AST.EnvRef, b: AST.EnvRef) bool {
            _ = ctx;
            return a.id == b.id;
        }

        pub fn hash(ctx: @This(), k: AST.EnvRef) u64 {
            _ = ctx;
            // return @truncate(k.tyv);
            return k.id;
        }
    });
    tyvars: TyVars,

    envs: Envs,

    fn init(al: std.mem.Allocator) @This() {
        return .{
            .tyvars = TyVars.init(al),
            .envs = Envs.init(al),
        };
    }

    fn difference(self: *@This(), diff: *const @This()) void {
        self.tyvars.difference(&diff.tyvars);
        self.envs.difference(&diff.envs);
    }

    fn deinit(self: *@This()) void {
        self.tyvars.deinit();
        self.envs.deinit();
    }
};
const FTV = struct { tyv: AST.TyVar, t: AST.Type };
fn mkSchemeforFunction(self: *Self, alreadyDefinedTVars: *const std.StringHashMap(AST.TVar), params: []*AST.Decon, ret: AST.Type, env: AST.Env, functionId: Unique, constraints_: *const Constraints) !AST.Scheme {
    const expectedBinding = AST.TVar.Binding{
        .Function = functionId,
    };

    // Function local stuff.
    var funftvs = FTVs.init(self.arena);
    try self.ftvs(&funftvs, ret);
    for (params) |p| {
        try self.ftvs(&funftvs, p.t);
    }

    // environment stuff.
    var envftvs = FTVs.init(self.arena);
    for (env) |inst| {
        // TODO: this is incorrect. For functions, I must extract ftvs from UNINSTANTIATED types.
        switch (inst.v) {
            .Var => try self.ftvs(&envftvs, inst.t),
            .Fun => |fun| {
                for (fun.params) |p| {
                    try self.ftvs(&envftvs, p.t);
                }

                try self.ftvs(&envftvs, fun.ret);
            },

            .ClassFun => |vv| {
                const cfun = vv.cfun;
                for (cfun.params) |p| {
                    try self.ftvs(&envftvs, p.t);
                }

                try self.ftvs(&envftvs, cfun.ret);
            },
        }
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

    var it = funftvs.tyvars.iterator();
    while (it.next()) |e| {
        const name = try std.fmt.allocPrint(self.arena, "'{}", .{e.tyv.uid});
        const tv = AST.TVar{
            .name = name,
            .uid = self.gen.tvars.newUnique(),
            .binding = expectedBinding,
            .inferred = true,
            .fields = self.typeContext.getFieldsForTVar(e.tyv) orelse &.{},
        };
        try tvars.append(tv);
        const tvt = try self.typeContext.newType(.{ .TVar = tv });
        try self.typeContext.unify(e.t, tvt);
    }

    var envs = std.ArrayList(AST.EnvRef).init(self.arena);
    var envIt = funftvs.envs.iterator();
    while (envIt.next()) |e| {
        try envs.append(e.*);
    }

    // also, make sure to gather assocs
    var assocs = std.ArrayList(AST.Association).init(self.arena);
    var assocsChanged = true;
    while (assocsChanged) {
        assocsChanged = false;
        const currentAssocs = try self.arena.alloc(Association, self.associations.items.len);
        defer self.arena.free(currentAssocs); // noop with arena. but reminds me of currentAssocs's lifetime.
        @memcpy(currentAssocs, self.associations.items);
        var i: usize = 0;
        for (currentAssocs) |assoc| {
            defer i +%= 1;
            switch (self.typeContext.getType(assoc.from)) {
                .TVar => |assocTV| { // TODO: associate tvars with places they are declared. this can be a tvar of an outside function.
                    if (!std.meta.eql(assocTV.binding, expectedBinding)) unreachable; // Since we added errors for TVars (in constraint solving), this should be unreachable.

                    if (!assocTV.inferred) b: {
                        const assocClass = assoc.classFun.class;
                        if (constraints_.get(assocTV)) |constrs| {
                            for (constrs.items) |class| {
                                if (class.uid == assocClass.uid) {
                                    // OKAY!
                                    break :b;
                                }
                            }
                        }

                        // here, it's "bruh"
                        try self.errors.append(.{ .TVarDoesNotImplementClass = .{ .class = assocClass, .tv = assocTV } });
                    }

                    // mkae sure to check it's actually bound to a function.
                    var assocFTVs = FTVs.init(self.arena); // TODO: this is kinda fugly. I should reuse the general ftvs.
                    defer assocFTVs.deinit();
                    try self.ftvs(&assocFTVs, assoc.to);

                    var assocFTVIt = assocFTVs.tyvars.iterator();
                    while (assocFTVIt.next()) |tyv| {
                        const name = try std.fmt.allocPrint(self.arena, "'{}", .{tyv.tyv.uid});
                        const tv = AST.TVar{
                            .name = name,
                            .uid = self.gen.tvars.newUnique(),
                            .binding = .{ .Function = functionId },
                            .inferred = true,
                            .fields = self.typeContext.getFieldsForTVar(tyv.tyv) orelse &.{},
                        };
                        try tvars.append(tv);
                        const tvt = try self.typeContext.newType(.{ .TVar = tv });
                        try self.typeContext.unify(tyv.t, tvt);
                    }

                    var assocEnvIt = assocFTVs.envs.iterator();
                    while (assocEnvIt.next()) |e| {
                        try envs.append(e.*);
                    }

                    // here we are adding an existing association to a scheme.
                    // remember to create a uid and pointer-write it to the previous match's association.
                    const assocID = self.gen.assocs.newUnique();
                    assoc.ref.* = .{ .Id = assocID };
                    try assocs.append(.{
                        .depends = assocTV,
                        .to = assoc.to,
                        .classFun = assoc.classFun,
                        .uid = assocID,
                    });

                    // also, make sure to later add tvars to them
                    _ = self.associations.orderedRemove(i);
                    i -%= 1;
                    assocsChanged = true;
                },
                .Anon => unreachable, // ???
                .TyVar => {},
                .Con => unreachable, // should be handled earlier
                .Fun => unreachable, // -//-
            }
        }
    }

    return .{
        .tvars = tvars.items,
        .envVars = envs.items,
        .associations = assocs.items,
    };
}

fn ftvs(self: *Self, store: *FTVs, tref: AST.Type) !void {
    const t = self.typeContext.getType(tref);
    switch (t) {
        .Anon => |fields| {
            for (fields) |field| {
                try self.ftvs(store, field.t);
            }
        },
        .TyVar => |tyv| {
            try store.tyvars.insert(.{ .tyv = tyv, .t = tref });
            if (self.typeContext.getFieldsForTVar(tyv)) |fields| {
                for (fields) |field| {
                    try self.ftvs(store, field.t);
                }
            }
        },
        .Con => |con| {
            for (con.application.tvars) |mt| {
                try self.ftvs(store, mt);
            }
        },
        .Fun => |fun| {
            for (fun.args) |arg| {
                try self.ftvs(store, arg);
            }

            const env = self.typeContext.getEnv(fun.env);
            if (env.env == null) {
                try store.envs.insert(env.base);
            }

            try self.ftvs(store, fun.ret);
        },
        .TVar => {},
    }
}

// ASSOCIATION
const Association = struct {
    from: AST.Type,
    to: AST.Type,

    classFun: *AST.ClassFun,
    ref: *?AST.Match(AST.Type).AssocRef,

    instances: Module.DataInstance,
};

fn addAssociation(self: *Self, assoc: Association) !void {
    try self.associations.append(assoc);
}

fn addInstance(self: *Self, instance: *AST.Instance) !void {
    const getOrPutResult = try self.scope.currentScope().instances.getOrPut(instance.class);
    if (!getOrPutResult.found_existing) {
        getOrPutResult.value_ptr.* = Module.DataInstance.init(self.arena);
    }

    const dataInsts = getOrPutResult.value_ptr;
    try dataInsts.put(instance.data, instance);
}

const Scope = struct {
    al: std.mem.Allocator,
    // TODO: instead of this stack, we should just use the program stack!
    //   But data locality is then scuffed...?
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
    vars: std.StringHashMap(Module.VarOrFun),
    types: std.StringHashMap(Module.DataOrClass),
    cons: std.StringHashMap(*AST.Con),
    tvars: std.StringHashMap(AST.TVar),
    instances: std.AutoHashMap(*AST.Class, Module.DataInstance),

    env: ?*Env,

    fn init(al: std.mem.Allocator, env: ?*Env) @This() {
        return .{
            .vars = std.StringHashMap(Module.VarOrFun).init(al),
            .types = std.StringHashMap(Module.DataOrClass).init(al),
            .cons = std.StringHashMap(*AST.Con).init(al),
            .tvars = std.StringHashMap(AST.TVar).init(al),
            .instances = std.AutoHashMap(*AST.Class, Module.DataInstance).init(al),
            .env = env,
        };
    }

    // in the future - scopes are actually safe to deallocate.
    fn deinit() void {}
};

const Env = std.ArrayList(AST.VarInst);

// typechecking zone
fn getReturnType(self: *Self) !AST.Type {
    return self.returnType orelse try self.definedType(.Int);
}

fn definedType(self: *Self, predefinedType: Prelude.PremadeType) !AST.Type {
    return (try self.defined(predefinedType)).dataInst.t;
}

fn defined(self: *Self, predefinedType: Prelude.PremadeType) !struct {
    dataInst: DataInst,
    data: *AST.Data,
} {
    return if (self.prelude) |prelude| {
        const data = prelude.defined(predefinedType);
        return .{
            .dataInst = try self.instantiateData(data),
            .data = data,
        };
    } else b: {
        const data = switch (self.maybeLookupType(Prelude.PremadeTypeName.get(predefinedType)) orelse break :b error.PreludeError) {
            .Data => |data| data,
            .Class => |_| break :b error.PreludeError,
        };

        return .{
            .dataInst = try self.instantiateData(data),
            .data = data,
        };
    };
}

// parser zone
fn foldFromHere(self: *Self) ParsingMode {
    const old = self.mode;
    self.mode = .{ .CountIndent = 0 };
    return old;
}

fn finishFold(self: *Self, mode: ParsingMode) !void {
    switch (self.mode) {
        .Normal => unreachable,
        .CountIndent => |i| {
            if (i == 1) {
                try self.devour(.DEDENT); // maybe make not consuming it `unreachable`? since this might not even be possible.
            } else if (i == 0) {
                try self.endStmt();
            }
        },
    }
    self.mode = mode;
}

fn expect(self: *Self, tt: TokenType) !Token {
    return self.consume(tt) orelse return self.err(Token, "Expect {}", .{tt});
}

fn devour(self: *Self, tt: TokenType) !void {
    _ = try self.expect(tt);
}

fn check(self: *Self, tt: TokenType) bool {
    return self.consume(tt) != null;
}

// PARSING PRIMITIVES
fn peek(self: *Self) Token {
    switch (self.mode) {
        .Normal => {},
        .CountIndent => |*ind| while (true) {
            if (!self.currentToken.isWhitespace()) break;
            if (self.currentToken.type == .STMT_SEP and ind.* == 0) break;
            if (self.currentToken.type == .INDENT) ind.* += 1;
            if (self.currentToken.type == .DEDENT) {
                if (ind.* <= 1) break;
                ind.* -= 1;
            }
            self.skip();
        },
    }
    return self.currentToken;
}

fn consume(self: *Self, tt: TokenType) ?Token {
    var tok = self.currentToken;
    switch (self.mode) {
        .Normal => {},
        .CountIndent => |*ind| while (tok.isWhitespace()) {
            if (tok.type == .STMT_SEP and ind.* == 0) break;
            if (tok.type == .INDENT) ind.* += 1;
            if (tok.type == .DEDENT) {
                if (ind.* <= 1) break;
                ind.* -= 1;
            }
            self.skip();
            tok = self.currentToken;
        },
    }

    if (tok.type == tt) {
        self.skip();
        return tok;
    } else {
        return null;
    }
}

// IMPORTANT: DON'T MODIFY SKIP, SINCE AFTER PEEK/CONSUME THE MODE SHOULD BE CHANGED. LET PEEK / CONSUME CONSUME ALL THE WHITESPACE BEFOREHAND.
// ALSO, THEY ALL DEPEND ON SKIP.
fn skip(self: *Self) void {
    self.currentToken = self.lexer.nextToken();
}

const ParsingMode = union(enum) {
    Normal,
    CountIndent: u32,
};

// NOTE: later, we don't have to specify a return value. Just always follow it with "return unreachable".
fn err(self: *Self, comptime t: type, comptime fmt: []const u8, args: anytype) !t {
    std.debug.print(fmt ++ " at {}\n", args ++ .{self.currentToken});
    std.debug.print("{s}\n", .{self.lexer.source[self.currentToken.from -% 5 .. @min(self.lexer.source.len, self.currentToken.to +% 5)]});
    return error.ParseError;
}

// this might be in the tokenizer.
fn sync_to_next_toplevel() void {}

const ParseError = error{ParseError};
const ParserError = error{
    ParseError,
    PreludeError,
    OutOfMemory,
    TempError,
}; // full error set when it cannot be inferred.

// TEMP
const Fold = *u32;
