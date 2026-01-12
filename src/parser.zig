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
        .mode = .{ .Simple = .Normal },

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
            std.debug.print("Err {s}.\n", .{self.name});
            return e;
        };

        // consume statement separators
        self.consumeSeps();

        if (dec != null) try decs.append(dec.?);
    }

    try self.solveAvailableConstraintsAndApplyDefaultsIfPossible();

    if (self.associations.items.len > 0) {
        try self.reportError(.{
            .ConstraintsLeft = self.associations.items,
        });
    }

    // std.debug.print("parsing success\n", .{});

    return .{
        .ast = AST{ .toplevel = decs.items },
        .exports = self.scopeToExports(),
    };
}

pub fn addExports(self: *Self, exports: *const Module.Exports) !void {
    try addToHash(&self.scope.currentScope().vars, &exports.vars);
    try addToHash(&self.scope.currentScope().cons, &exports.cons);
    try addToHash(&self.scope.currentScope().types, &exports.types);
    try self.addAllInstances(exports);
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

fn dataDef(self: *Self, typename: Token, extraTVar: ?Token, annotations: []AST.Annotation) !void {
    const uid = self.gen.types.newUnique();
    self.scope.beginScope(null);
    const data = b: {
        defer self.scope.endScope();

        // tvars
        var tvars = std.ArrayList(AST.TVarOrNum).init(self.arena);
        if (extraTVar) |tvname| {
            const tv = try self.newTVar(tvname.literal(self.lexer.source), .{ .Data = uid });
            try tvars.append(.{ .TVar = tv });
        }

        while (true) {
            if (self.consume(.NUMTYNAME)) |numtyTok| {
                const numtv = try self.newTNum(numtyTok.literal(self.lexer.source)[1..], .{ .Data = uid });
                try tvars.append(.{ .TNum = numtv });
            } else if (self.consume(.IDENTIFIER)) |tvname| {
                const tv = try self.newTVar(tvname.literal(self.lexer.source), .{ .Data = uid });
                try tvars.append(.{ .TVar = tv });
            } else {
                break;
            }
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
                .annotations = annotations,
            });
        }

        const data = try Common.allocOne(self.arena, AST.Data{
            .uid = self.gen.types.newUnique(),
            .name = typename.literal(self.lexer.source),
            .scheme = undefined, // NOTE: currently leave it undefined, but recursive structures might have a problem.
            // .scheme = AST.Scheme{
            //     .tvars = tvars.items,
            //     .envVars = &.{}, // TEMP
            //     .associations = &.{},
            // }, // TODO: check for repeating tvars and such.
            .annotations = annotations,
            .stuff = undefined,
        });

        var cons = std.ArrayList(AST.Con).init(self.arena);
        var recs = std.ArrayList(AST.Record).init(self.arena);
        var tag: u32 = 0;
        var assocs = std.ArrayList(AST.Association).init(self.arena);
        const tyconstr = Type.Constrain{ .Data = .{ .uid = data.uid, .assocs = &assocs } };
        while (!self.check(.DEDENT)) {
            if (self.consume(.IDENTIFIER)) |recname| {
                // record
                const t = try Type.init(self, tyconstr).sepTyo();
                try recs.append(.{
                    .field = recname.literal(self.lexer.source),
                    .t = t.e,
                });
                try self.endStmt();
            } else if (self.consume(.TYPE)) |conName| {
                // constructor
                var tys = std.ArrayList(AST.Type).init(self.arena);
                while (!(self.check(.STMT_SEP) or (self.peek().type == .DEDENT))) { // we must not consume the last DEDENT, as it's used to terminate the whole type declaration.
                    // TODO: for now, no complicated types!
                    const ty = try Type.init(self, tyconstr).typ();
                    try tys.append(ty.e);
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
            } else {
                unreachable; // TODO: ERROR!
            }
        }

        // don't forget to add associations at the end!!!!
        for (assocs.items) |assoc| {
            try tvars.append(.{ .TVar = assoc.depends });
        }
        data.scheme = .{
            .tvars = tvars.items,
            .envVars = &.{},
            .associations = assocs.items,
        };

        if (cons.items.len > 0 and recs.items.len > 0) {
            try self.reportError(.{ .RecordsAndConstructorsPresent = .{} });
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

fn function(self: *Self, fun: *AST.Function, nameLoc: Loc) !*AST.Function {
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
    const tyconstr = Type.Constrain{ .Function = .{ .uid = fun.name.uid } };
    if (!self.check(.RIGHT_PAREN)) {
        while (true) {
            const decon = try self.deconstruction();
            const nextTok = self.peek().type;
            if (nextTok != .COMMA and nextTok != .RIGHT_PAREN) {
                const pt = try Type.init(
                    self,
                    tyconstr,
                ).sepTyo();
                try self.typeContext.unify(decon.t, pt.e, &.{ .l = decon.l, .r = pt.l });
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
        const retTy = try Type.init(self, tyconstr).sepTyo();
        try self.typeContext.unify(ret, retTy.e, null);
    }

    const constraints_ = try self.constraints();

    // NOTE: make sure everything what's needed is assigned in case of recursive calls.
    fun.* = AST.Function{
        .name = fun.name,
        .params = params.items,
        .ret = ret,
        .scheme = AST.Scheme.empty(), // in recursive calls, the scheme should be empty
        .temp__isRecursive = true,
        .env = &.{},
        .body = undefined,
    };

    const fnBody = if (self.check(.COLON)) b: {
        const pm = self.foldFromHere();
        const expr = try self.expression();
        try self.finishFold(pm);

        const stmts = try self.arena.alloc(*AST.Stmt, 1);
        stmts[0] = try Common.allocOne(self.arena, AST.Stmt{
            .Return = expr,
        });
        try self.typeContext.unify(ret, expr.t, &.{ .l = expr.l });
        break :b stmts;
    } else b: {
        // set return and parse body
        const oldReturnType = self.returnType;
        self.returnType = ret;
        const fnBodyAndReturnStatus = try self.body();
        var fnBody = fnBodyAndReturnStatus.stmts;
        const returnStatus = fnBodyAndReturnStatus.returnStatus;

        try self.finishBodyAndInferReturnType(&fnBody, returnStatus, nameLoc);

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
        .temp__isRecursive = false,
    };

    return fun;
}

fn finishBodyAndInferReturnType(self: *Self, fnBody: *std.ArrayList(*AST.Stmt), returnStatus: ReturnStatus, l: Loc) !void {
    // TODO: factor it out!
    if (!self.triedReturningAtAll) {
        try fnBody.append(try Common.allocOne(self.arena, try self.unitReturn(l))); // TODO: add special location type for "midlines"
        // eg.
        //      askjdklasjdk
        //  |-> ----------
        //  |   aksdlkasjdkj
        //  L some footnote

    } else if (returnStatus == .Nah) retcheck: {
        // We know we returned somewhere, but for this main program branch we did not for some reason.
        // If the return type is Unit, we can just insert a return.
        //  Otherwise, error obv.
        switch (self.typeContext.getType(self.returnType.?)) {
            .Con => |c| if (c.type.uid == (try self.defined(.Unit)).data.uid) {
                try fnBody.append(try Common.allocOne(self.arena, try self.unitReturn(l)));
                break :retcheck;
            },
            else => {
                // otherwise
                try self.reportError(.{ .MissingReturn = .{} });
            },
        }
    }
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
    return self.statement_() catch |e| switch (e) {
        error.ParseError => {
            // maybe extract it to a new function.
            // sync to end of line (naive n simple)
            while (!self.isEndStmt() and self.peek().type != .INDENT) {
                self.skip();
            }

            // what to do in case of sudden indent??
            // maybe nothing? like, handle all indenting statemenets separately (if, case, etc.)

            switch (self.mode) {
                .Simple => |nmode| switch (nmode) {
                    .Normal => try self.endStmt(),
                    .CountIndent => try self.finishFold(.{ .Simple = .Normal }),
                },

                else => unreachable,
            }
            return null;
        },
        else => return e,
    };
}

fn statement_(self: *Self) ParserError!?*AST.Stmt {
    if (self.returned == .Returned) {
        try self.reportError(.{ .UnreachableCode = .{} });
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
        else if (self.consume(.RETURN)) |retTok| {
            var l = self.loc(retTok);
            const expr = if (!self.isEndStmt()) bb: {
                const pm = self.foldFromHere();
                const e = try self.expression();
                l = l.between(e.l);
                try self.finishFold(pm);
                break :bb e;
            } else bb: {
                try self.endStmt();
                const t = try self.definedType(.Unit);
                const con = &self.typeContext.getType(t).Con.type.stuff.cons[0]; // WARNING: funny casts
                break :bb try self.allocExpr(.{
                    .t = t,
                    .l = self.loc(retTok),
                    .e = .{ .Con = con },
                });
            };

            self.triedReturningAtAll = true;
            if (self.returned != .Errored) self.returned = .Returned;

            try self.typeContext.unify(expr.t, try self.getReturnType(), &.{ .l = l });

            break :b .{ .Return = expr };
        } // return
        else if (self.check(.BREAK)) {
            try self.endStmt();
            break :b .{ .Break = .{} };
            //
        } // break
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
                                try self.reportError(.{ .ModuleDoesNotExportThing = .{
                                    .moduleName = modpath.items,
                                    .thing = varName,
                                    .l = self.loc(v),
                                } });
                            }
                        }
                    } else if (self.consume(.TYPE)) |tt| {
                        const typeName = tt.literal(self.lexer.source);
                        try self.endStmt();
                        const dataOrClass: ?Module.DataOrClass = bb: {
                            if (mmodule) |mod| {
                                const doc = mod.lookupData(typeName) orelse {
                                    try self.reportError(.{ .UndefinedType = .{ .typename = typeName, .loc = self.loc(tt) } });
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
                                                    try self.reportError(.{ .ClassDoesNotExportThing = .{} });
                                                }
                                            },
                                            .Data => try self.reportError(.{ .ModuleDoesNotExportThing = .{
                                                .moduleName = modpath.items,
                                                .thing = vname,
                                                .l = self.loc(vt),
                                            } }),
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
                                                    try self.reportError(.{ .DataDoesNotExportThing = .{} });
                                                }
                                            },
                                            .Class => try self.reportError(.{ .ModuleDoesNotExportThing = .{
                                                .moduleName = modpath.items,
                                                .thing = cname,
                                                .l = self.loc(ct),
                                            } }),
                                        }
                                    }
                                } else {
                                    return try self.errorExpect("imported stuff");
                                }

                                if (self.check(.RIGHT_PAREN)) break;
                                try self.devour(.COMMA);
                            };
                        }
                    } else {
                        return try self.errorExpect("import");
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
            const fun = try self.function(funptr, self.loc(v));
            const fndec = AST.Stmt{ .Function = fun };
            break :b fndec;
        } // function
        else if (self.consume(.IDENTIFIER)) |v| {
            // here, choose between identifier and call
            if (self.check(.EQUALS)) {
                const pm = self.foldFromHere();
                const expr = try self.expression();
                const vt = try self.newVar(v);
                try self.typeContext.unify(vt.t, expr.t, &.{ .l = self.loc(v), .r = expr.l });

                try self.finishFold(pm);

                break :b .{ .VarDec = .{
                    .varDef = vt.v,
                    .varValue = expr,
                } };
            } else if (self.check(.LTEQ)) { // basic mutation (different token.)
                // COPYPASTA
                const pm = self.foldFromHere();
                const e = try self.expression();
                try self.finishFold(pm);

                const vtsc = try self.lookupVar(&.{}, v);
                const vv = switch (vtsc.vorf) {
                    .Var => |vt| vt,
                    else => bb: {
                        try self.reportError(.{ .TryingToMutateNonVar = .{} });
                        break :bb try self.newVar(v);
                    },
                };

                try self.typeContext.unify(vv.t, e.t, &.{ .l = self.loc(v), .r = e.l });
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
                        try self.reportError(.{ .TryingToMutateNonVar = .{} });
                        break :bb try self.newVar(v);
                    },
                };

                var innerTy = vv.t;
                var accessors = std.ArrayList(AST.Stmt.Accessor).init(self.arena);
                while (true) {
                    if (self.consume(.REF)) |reftok| {
                        try accessors.append(.{ .tBefore = innerTy, .acc = .Deref });

                        const ptr = (try self.defined(.Ptr)).dataInst;

                        try self.typeContext.unify(innerTy, ptr.t, &.{ .l = self.loc(v), .r = self.loc(reftok) });
                        innerTy = ptr.tyArgs[0].Type;
                    } else if (self.consume(.DOT)) |dottok| {
                        const name = try self.expect(.IDENTIFIER);
                        const field = name.literal(self.lexer.source);
                        try accessors.append(.{
                            .tBefore = innerTy,
                            .acc = .{ .Access = field },
                        });

                        const ft = try self.typeContext.field(innerTy, field, &.{
                            .l = self.loc(v),
                            .r = self.loc(dottok).between(self.loc(name)),
                        });
                        innerTy = ft;
                    } else break;
                }

                try self.devour(.EQUALS);

                const pm = self.foldFromHere();
                const e = try self.expression();

                try self.typeContext.unify(innerTy, e.t, &.{ .l = self.loc(v), .r = e.l });

                // TEMP
                // if (vtsc.sc != self.scope.currentScope()) {
                //     try self.reportError(.{ .CannotDirectlyMutateVarFromEnv = .{} });
                // }

                try self.finishFold(pm);
                break :b .{ .VarMut = .{
                    .varRef = vv.v,
                    .accessors = accessors.items,
                    .varValue = e,
                } };
            } // mutation
            else {
                // try parsing expression yo as a variable n shiii
                const pm = self.foldFromHere();
                const vv = try self.instantiateVar(&.{}, v);
                const e = try self.finishExpression(try self.allocExpr(.{
                    .t = vv.t,
                    .e = .{ .Var = .{ .v = vv.v, .match = vv.m } },
                    .l = self.loc(v),
                }));
                try self.finishFold(pm);
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
            if (self.peek().type == .DOT) { // DON'T CONSUME!
                const pm = self.foldFromHere();
                const qe = try self.qualified(typename);
                const e = try self.finishExpression(qe);
                try self.finishFold(pm);
                break :b .{ .Expr = e };
            }

            // can be a tvar or a postfix call.
            const lexState = self.saveLexingState();
            if (self.consume(.IDENTIFIER)) |mtv| {
                if (self.peek().type == .LEFT_PAREN) {
                    // postfix call.
                    self.loadLexingState(lexState); // AHH AHSDH FUCK I DID IT, NO!!
                    // ITS OBVIOUS I SHOULD USE A `data` KEYWORD OR SOMETHING LIKE THIS BRUHHHH.
                    // BUT MUH QT SYNTAX :OOOOOOOO
                    const pm = self.foldFromHere();
                    const ce = try self.constructorExpression(&.{}, typename);
                    try self.finishFold(pm);
                    break :b .{ .Expr = try self.finishExpression(ce) };
                }

                try self.dataDef(typename, mtv, annotations);
                break :b null;
            }

            // BRITTLE AS HELL.
            if (self.peek().type == .INDENT or self.peek().type == .STMT_SEP or self.peek().type == .NUMTYNAME) {
                // TODO: parse non-qualified postfix expression alls.
                try self.dataDef(typename, null, annotations);
                break :b null;
            }

            // actually, this is probably an expression, so parse it as one.

            const pm = self.foldFromHere();
            const ce = try self.constructorExpression(&.{}, typename);
            const e = try self.finishExpression(ce);
            try self.finishFold(pm);
            break :b .{ .Expr = e };
        } // type
        else if (self.check(.IF)) {
            const cond = try self.expression();
            try self.typeContext.unify(cond.t, try self.definedType(.Bool), &.{ .l = cond.l });
            const bTrueBod = try self.body();
            const bTrue = bTrueBod.stmts.items;
            var returnStatus = bTrueBod.returnStatus;

            var elifs = std.ArrayList(AST.Stmt.Elif).init(self.arena);
            while (self.check(.ELIF)) {
                const elifCond = try self.expression();
                try self.typeContext.unify(elifCond.t, try self.definedType(.Bool), &.{ .l = elifCond.l });
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
            try self.typeContext.unify(cond.t, boolTy, &.{ .l = cond.l });
            const bod = try self.body();
            self.returned = bod.returnStatus;
            break :b .{ .While = .{
                .cond = cond,
                .body = bod.stmts.items,
            } };
        } // while
        else if (self.check(.FOR)) {
            const decon = try self.deconstruction();
            try self.devour(.IN);
            const itexpr = try self.expression();

            // do the unifications!
            // TODO: bruh, type and code generation is so annoying bruv. I should make it look nicer (and simpler repr will cause less bugs.)
            //  Maybe I should make some convenience functions?
            const intoIterClass = try self.definedClass(.IntoIter);
            const intoIterFun = intoIterClass.classFuns[0];
            const intoIterFunInst = try self.instantiateClassFunction(intoIterFun, itexpr.l);

            const iterType = try self.typeContext.fresh();
            try self.typeContext.unify(intoIterFunInst.t, try self.makeType(.{ .Fun = .{
                .args = [_]AST.Type{itexpr.t},
                .ret = iterType,
            } }), &.{ .l = itexpr.l });

            const iterClass = try self.definedClass(.Iter);
            const nextFun = iterClass.classFuns[0];
            const nextFunInst = try self.instantiateClassFunction(nextFun, itexpr.l);

            const elemType = decon.t;
            const maybeElem = (try self.defined(.Maybe)).dataInst;
            try self.typeContext.unify(maybeElem.tyArgs[0].Type, elemType, &.{ .l = itexpr.l, .r = decon.l });

            const ptrType = (try self.defined(.Ptr)).dataInst;
            try self.typeContext.unify(ptrType.tyArgs[0].Type, iterType, &.{ .l = itexpr.l });

            try self.typeContext.unify(nextFunInst.t, try self.makeType(.{ .Fun = .{
                .args = [_]AST.Type{ptrType.t},
                .ret = maybeElem.t,
            } }), &.{ .l = itexpr.l, .r = decon.l });

            const bod = try self.body();
            self.returned = bod.returnStatus;
            break :b .{ .For = .{
                .decon = decon,
                .iter = itexpr,
                .intoIterFun = intoIterFunInst.ref,
                .nextFun = nextFunInst.ref,
                .body = bod.stmts.items,
            } };
        } // for
        else if (self.check(.CASE)) {
            const switchOn = try self.expression();

            var returnStatus = ReturnStatus.Returned; // mempty-like
            var cases = std.ArrayList(AST.Case).init(self.arena);
            try self.devour(.INDENT);
            self.scope.beginScope(null);
            while (!self.check(.DEDENT)) {
                const decon = try self.deconstruction();
                try self.typeContext.unify(switchOn.t, decon.t, &.{ .l = switchOn.l, .r = decon.l });
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
            class.* = AST.Class{
                .uid = uid,
                .name = className.literal(self.lexer.source),
                .selfType = selfVar,

                .classFuns = undefined,
                .default = null,
            };

            if (self.check(.COLON)) {
                const qtypeName = try self.parseQualifiedType(try self.expect(.TYPE));
                const data = (try self.findQualifiedDataOrClass(qtypeName.modpath, qtypeName.name, qtypeName.loc) orelse unreachable).Data;
                class.default = data;
            }

            var classFuns = std.ArrayList(*AST.ClassFun).init(self.arena);
            try self.devour(.INDENT);
            while (!self.check(.DEDENT)) {
                const classFun = try self.classFunction(.{ .tvar = selfVar, .t = selfType }, class);
                self.consumeSeps();
                try classFuns.append(classFun);
            }

            self.selfType = oldSelf;

            class.classFuns = classFuns.items;

            try self.newClass(class);

            return null;
        } // class
        else if (self.check(.INST)) {
            const qclassName = try self.parseQualifiedType(try self.expect(.TYPE));
            const class = (try self.findQualifiedDataOrClass(qclassName.modpath, qclassName.name, qclassName.loc) orelse unreachable).Class;
            const qtypeName = try self.parseQualifiedType(try self.expect(.TYPE));
            const data = (try self.findQualifiedDataOrClass(qtypeName.modpath, qtypeName.name, qtypeName.loc) orelse unreachable).Data;

            const oldSelf = self.selfType;
            const instantiatedSelfType = try self.typeContext.newType(.{
                .Con = .{
                    .type = data,
                    .application = try self.instantiateScheme(data.scheme, qtypeName.loc),
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
                const fun = try self.function(funptr, self.loc(funName));

                // now, "associate it" with a class function
                for (class.classFuns) |classFun| {
                    if (!Common.streq(classFun.name.name, fun.name.name)) continue;

                    // class function found. unify types.
                    // todo
                    try instFuns.append(.{ .fun = fun, .classFunId = classFun.uid });
                    break;
                } else {
                    // error that instance function is not found.
                    // TODO: I might need to add a placeholder function (based on the class declaration), which is a lot of work, so whatever.
                    try self.errorExpect("could not find instance of function (IMPLEMENT THIS PART BRUH!!)");
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
    const tyconstr = Type.Constrain{ .ExternalFunction = .{ .uid = uid } };
    if (!self.check(.RIGHT_PAREN)) {
        while (true) {
            const pname = try self.expect(.IDENTIFIER);
            const v = try self.newVar(pname); // pointless fresh.
            const t = try Type.init(self, tyconstr).sepTyo();
            try params.append(.{ .pn = v.v, .pt = t.e });

            if (self.check(.RIGHT_PAREN)) {
                break;
            }
            try self.devour(.COMMA);
        }
    }

    try self.devour(.RIGHT_ARROW);
    const ret = try Type.init(self, tyconstr).sepTyo();
    try self.endStmt();

    // TODO: Technically, we should be able to pass buffers. But we should not in general allow type integers.
    var definedTVars = std.ArrayList(AST.TVarOrNum).init(self.arena);
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
        .ret = ret.e,
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

fn unitReturn(self: *Self, culprit: Loc) !AST.Stmt {
    const t = try self.definedType(.Unit);
    const con = &self.typeContext.getType(t).Con.type.stuff.cons[0]; // WARNING: funny casts
    try self.typeContext.unify(t, self.returnType.?, &.{ .l = culprit });
    return .{ .Return = try self.allocExpr(.{
        .t = t,
        .e = .{ .Con = con },
        .l = culprit,
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
    return tt == .DEDENT or tt == .STMT_SEP or tt == .EOF; // NOTE: check for EOF just in case!
}

fn consumeSeps(self: *Self) void {
    while (self.check(.STMT_SEP)) {}
}

fn classFunction(self: *Self, classSelf: struct { tvar: AST.TVar, t: AST.Type }, class: *AST.Class) !*AST.ClassFun {
    const funName = try self.expect(.IDENTIFIER);
    const uid = self.gen.classFuns.newUnique();

    self.scope.beginScope(null); // let's capture all tvars.
    var params = std.ArrayList(AST.ClassFun.Param).init(self.arena);
    var assocs = std.ArrayList(AST.Association).init(self.arena);
    const tyconstr = Type.Constrain{ .ClassFunction = .{ .uid = uid, .assocs = &assocs } };
    try self.devour(.LEFT_PAREN);
    if (!self.check(.RIGHT_PAREN)) while (true) {
        // consume identifier if possible.
        if (self.check(.IDENTIFIER)) {}

        try params.append(.{ .t = (try Type.init(self, tyconstr).sepTyo()).e });

        if (self.check(.RIGHT_PAREN)) {
            break;
        }

        try self.devour(.COMMA);
    };

    // another new eye candy - default Unit
    const ret = if (self.check(.RIGHT_ARROW)) (try Type.init(self, tyconstr).sepTyo()).e else try self.definedType(.Unit);

    // constraints
    const constraints_ = try self.constraints();

    try self.endStmt();
    const tvarsMap = self.scope.currentScope().tvars;
    self.scope.endScope();

    // make a scheme from deze vars yo.
    var tvars = std.ArrayList(AST.TVarOrNum).init(self.arena);
    try tvars.append(.{ .TVar = classSelf.tvar });
    var tvit = tvarsMap.valueIterator();
    while (tvit.next()) |tvar| {
        try tvars.append(tvar.*);
    }

    // also append implicit tvars from inner class definitions.
    for (assocs.items) |ass| {
        try tvars.append(.{ .TVar = ass.depends });
    }

    // make sure to add the implicit tvars before this!
    try self.addConstraintsToAssocs(&assocs, &constraints_);

    const scheme = AST.Scheme{
        .tvars = tvars.items,
        .envVars = &.{}, // TEMP
        .associations = assocs.items, // NOTE: this will change when class constraints are allowed on functions. EDIT: Or not??? We have no constraints to specify, because these constraints are not based on class function calls. Right now, I'll leave it alone because of our "broken" type system.
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
                        const tvarOrNum = self.scope.currentScope().tvars.get(tvarName) orelse {
                            // NOTE: we're looking only at current scope, so we won't find any named tvars from other functions.
                            try self.reportError(.{ .ConstrainedNonExistentTVar = .{ .tvname = tvarName } });
                            break :b;
                        };

                        const tvar = switch (tvarOrNum) {
                            .TVar => |tv| tv,
                            .TNum => unreachable, // TODO: error
                        };

                        const e = try constraints_.getOrPutValue(tvar, std.ArrayList(*AST.Class).init(self.arena)); // NOTE: source says it's not allocating anything until an element is inserted.

                        try e.value_ptr.append(class);
                    },
                    .Data => unreachable, // TODO: error.
                }
            } else {
                try self.reportError(.{ .UndefinedClass = .{
                    .className = classTok.literal(self.lexer.source),
                } });
            }

            if (!self.check(.COMMA)) break;
        }
    }

    return constraints_;
}

fn addConstraintsToAssocs(self: *Self, assocs: *std.ArrayList(AST.Association), constrs: *const Constraints) !void {
    // also add defined constraints! (but it's all bad thoooo)
    var constrIt = constrs.iterator();
    while (constrIt.next()) |kv| {
        for (kv.value_ptr.items) |class| {
            try assocs.append(.{
                .depends = kv.key_ptr.*,
                .uid = self.gen.assocs.newUnique(),
                .class = class,
                .concrete = null,
            });
        }
    }
}

fn deconstruction(self: *Self) !*AST.Decon {
    const decon: AST.Decon = if (self.consume(.IDENTIFIER)) |vn| b: {
        const v = try self.newVar(vn);
        break :b .{
            .t = v.t,
            .l = self.loc(vn),
            .d = .{ .Var = v.v },
        };
    } // var
    else if (self.consume(.UNDERSCORE)) |ut| b: {
        break :b .{
            .t = try self.typeContext.fresh(),
            .l = self.loc(ut),
            .d = .{ .None = .{} },
        };
    } // ignore var
    else if (self.consume(.INTEGER)) |numTok| b: {
        break :b .{
            .t = try self.definedType(.Int),
            .l = self.loc(numTok),
            .d = .{ .Num = self.parseInt(numTok) },
        };
    } // number
    else if (self.consume(.TYPE)) |cn| b: {
        const con = bb: {
            if (self.check(.DOT)) {
                var modpath = std.ArrayList(Str).init(self.arena);
                try modpath.append(cn.literal(self.lexer.source));
                // TODO: oof. what is this? I need to check it if it's duplicate code.
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

        const conLocation = self.loc(cn); // TODO: incorrect in case of qualified types

        var decons: []*AST.Decon = &.{};
        var args: []AST.Type = &.{};
        var tysLoc: ?Loc = null;
        if (self.check(.LEFT_PAREN)) {
            var ds = std.ArrayList(*AST.Decon).init(self.arena);
            var tys = std.ArrayList(AST.Type).init(self.arena);

            while (true) { // while1
                const d = try self.deconstruction();
                try ds.append(d);
                try tys.append(d.t);
                tysLoc = if (tysLoc) |l| l.between(d.l) else d.l;

                if (self.check(.RIGHT_PAREN)) break;

                try self.devour(.COMMA);
            }

            decons = ds.items;
            args = tys.items;
        }

        try self.typeContext.unifyParams(con.tys, args, &.{
            .l = conLocation,
            .r = tysLoc,
        }, &.{
            .lfull = con.t,
            .rfull = null,
        });
        break :b .{
            .t = con.t,
            .l = conLocation.between(tysLoc),
            .d = .{
                .Con = .{
                    .con = con.con,
                    .decons = decons,
                },
            },
        };
    } // con decon
    else if (self.consume(.LEFT_BRACE)) |leftBraceTok| b: {
        const t = try self.typeContext.fresh();

        var fields = std.ArrayList(AST.Decon.Field).init(self.arena);
        while (true) {
            const fieldTok = try self.expect(.IDENTIFIER);
            const fieldName = fieldTok.literal(self.lexer.source);

            const fieldTy = try self.typeContext.field(t, fieldName, null);

            if (self.check(.COLON)) {
                const decon = try self.deconstruction();
                try self.typeContext.unify(fieldTy, decon.t, null);
                try fields.append(.{
                    .field = fieldName,
                    .decon = decon,
                });
            } else {
                const vnt = try self.newVar(fieldTok);
                try self.typeContext.unify(fieldTy, vnt.t, null);
                try fields.append(.{
                    .field = fieldName,
                    .decon = try Common.allocOne(self.arena, AST.Decon{
                        .t = fieldTy,
                        .d = .{ .Var = vnt.v },
                        .l = self.loc(fieldTok),
                    }),
                });
            }
            if (!self.check(.COMMA)) break;
        }
        const rightBraceTok = try self.expect(.RIGHT_BRACE);
        const dloc = self.loc(leftBraceTok).between(self.loc(rightBraceTok));

        // TODO: right now we only care that the deconstructed struct has all the fields defined. basically { <whatever we write>, ... }
        // later expect the user to write `...` to ignore extra fields.
        break :b .{
            .t = t,
            .d = .{ .Record = fields.items },
            .l = dloc,
        };
    } // record deccon
    else if (self.consume(.LEFT_SQBR)) |ltok| b: {
        var left = std.ArrayList(*AST.Decon).init(self.arena);
        var right = std.ArrayList(*AST.Decon).init(self.arena);
        const listTy = try self.typeContext.fresh();
        const elemTy = try self.typeContext.fresh();
        const spreadTy = try self.typeContext.fresh();
        const spreadInnerTy = try self.typeContext.fresh();

        // for now, parse an easy version of this.
        var decons = &left;
        var spreadVar: ?AST.Var = null;
        var hadSpread = false;
        const dloc = if (self.consume(.RIGHT_SQBR)) |rtok| bb: {
            break :bb self.loc(ltok).between(self.loc(rtok));
        } else bb: {
            while (true) {
                if (!hadSpread and self.check(.DOT)) {
                    // scuffed spread xddddd
                    try self.devour(.DOT);
                    try self.devour(.DOT);

                    if (self.consume(.IDENTIFIER)) |svtok| {
                        const sv = try self.newVar(svtok);
                        try self.typeContext.unify(sv.t, spreadInnerTy, &.{ .l = self.loc(svtok) });
                        spreadVar = sv.v;
                    }

                    hadSpread = true;
                    decons = &right;
                } else {
                    const decon = try self.deconstruction();
                    try self.typeContext.unify(decon.t, elemTy, &.{ .l = decon.l });
                    try decons.append(decon);
                }

                if (self.consume(.RIGHT_SQBR)) |rtok| {
                    break :bb self.loc(ltok).between(self.loc(rtok));
                }
                try self.devour(.COMMA);
            }
        };

        const class: *AST.Class = try self.definedClass(.ListDecon); // NOTE: assumes, that we won't be doing any deconstructing of lists in prelude (a fair assumption)
        const cfun = class.classFuns[0]; // assume only one function! no need create another enum or search by string!

        const ifn = try self.instantiateClassFunction(cfun, dloc);

        // ====== Construct fun ty from here. ======
        const params = try self.arena.alloc(AST.Type, 6);

        params[0] = listTy;

        const elemPtr = (try self.defined(.Ptr)).dataInst; // pointer to actual elements
        const elemPtrPtr = (try self.defined(.Ptr)).dataInst; // ptr to ptr which switches on real data or the premade list.
        // this is to allow modification, while allowing types which don't have a stable pointer to any element.
        try self.typeContext.unify(elemPtr.tyArgs[0].Type, elemTy, null); // TODO: nulls here, we'll see if this place can error out.
        try self.typeContext.unify(elemPtrPtr.tyArgs[0].Type, elemPtr.t, null);

        try self.typeContext.unify(params[1], elemPtrPtr.t, null);
        try self.typeContext.unify(params[4], elemPtrPtr.t, null);

        const spread = (try self.defined(.ListSpread)).dataInst;
        try self.typeContext.unify(spread.tyArgs[0].Type, spreadInnerTy, null);
        try self.typeContext.unify(params[3], spread.t, null);
        try self.typeContext.unify(spread.t, spreadTy, null);

        const funTy = try self.typeContext.newType(.{ .Fun = .{
            .args = params,
            .ret = try self.definedType(.Bool),
            .env = try self.typeContext.newEnv(null),
        } });

        try self.typeContext.unify(ifn.t, funTy, &.{ .l = dloc });

        break :b .{
            .t = listTy,
            .l = dloc,
            .d = .{ .List = .{
                .l = left.items,
                .r = if (hadSpread) .{
                    .spreadVar = if (spreadVar) |v| .{ .v = v, .t = spreadInnerTy } else null,
                    .r = right.items,
                } else null,
                .assocRef = ifn.ref,

                .elemTy = elemTy,
                .spreadTy = spreadTy,
            } },
        };
        //
    } // arr decon [...]
    else {
        return try self.errorExpect("decon");
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
    var optok = self.peek();

    // FUNNY! Handle multiline lambdas.
    if (optok.type == .INDENT and self.mode == .MultilineLambda) {
        try self.multilineLambda(self.loc(optok));
        optok = self.peek();
    }

    var binop = getBinOp(self.peek()) orelse return left;
    const nextPrec = binOpPrecedence(binop);

    if (nextPrec <= minPrec) {
        return left;
    } else {
        self.skip(); // if accepted, consume

        if (binop == .Call) {
            var params = std.ArrayList(*AST.Expr).init(self.arena);
            const leftLoc = self.loc(optok);
            const rightLoc = if (self.consume(.RIGHT_PAREN)) |rightTok| b: {
                break :b self.loc(rightTok);
            } else b: {
                while (true) {
                    try params.append(try self.expression());
                    if (!self.check(.COMMA)) break;
                }

                break :b self.loc(try self.expect(.RIGHT_PAREN));
            };

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

            try self.typeContext.unify(left.t, callType, &.{
                .l = left.l,
                .r = leftLoc.between(rightLoc),
            });

            return self.allocExpr(.{
                .t = retType,
                .e = .{ .Call = .{
                    .callee = left,
                    .args = params.items,
                } },
                .l = left.l.between(rightLoc),
            });
        }

        if (binop == .PostfixCall) {
            const funt: *AST.Expr = try self.qualified(optok);

            var params = std.ArrayList(*AST.Expr).init(self.arena);
            try params.append(left);
            _ = try self.devour(.LEFT_PAREN);
            const rightLoc = if (self.consume(.RIGHT_PAREN)) |rightTok| b: {
                break :b self.loc(rightTok);
            } else b: {
                while (true) {
                    try params.append(try self.expression());
                    if (!self.check(.COMMA)) break;
                }

                break :b self.loc(try self.expect(.RIGHT_PAREN));
            };

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

            try self.typeContext.unify(callType, funt.t, &.{
                .l = funt.l,
                .r = left.l.between(rightLoc),
            });

            return self.allocExpr(.{
                .t = retType,
                .e = .{ .Call = .{
                    .callee = funt,
                    .args = params.items,
                } },
                .l = left.l.between(rightLoc),
            });
        }

        if (binop == .Deref) {
            const ptr = (try self.defined(.Ptr)).dataInst;
            const dl = self.loc(optok);
            try self.typeContext.unify(ptr.t, left.t, &.{
                .l = left.l,
                .r = dl,
            });
            return self.allocExpr(.{
                .t = ptr.tyArgs[0].Type,
                .e = .{
                    .UnOp = .{ .op = .Deref, .e = left },
                },
                .l = left.l.between(dl),
            });
        }

        if (binop == .RecordAccess) {
            const mem = try self.expect(.IDENTIFIER);
            const fieldLoc = self.loc(mem);
            const t = try self.typeContext.field(left.t, mem.literal(self.lexer.source), &.{ .l = left.l, .r = fieldLoc });
            return self.allocExpr(.{
                .t = t,
                .e = .{ .UnOp = .{
                    .e = left,
                    .op = .{
                        .Access = mem.literal(self.lexer.source),
                    },
                } },
                .l = left.l.between(fieldLoc),
            });
        }

        if (binop == .As) {
            const t = try Type.init(self, null).sepTyo();
            try self.typeContext.unify(t.e, left.t, &.{
                .l = left.l,
                .r = t.l,
            });
            return self.allocExpr(.{
                .t = t.e,
                // NOTE: we generate a new node only for error reporting. we don't really need it otherwise.
                .e = .{ .UnOp = .{
                    .e = left,
                    .op = .{ .As = t.e },
                } },
                .l = left.l.between(t.l),
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
                try self.typeContext.unify(left.t, intTy, &.{
                    .l = left.l,
                });
                try self.typeContext.unify(right.t, intTy, &.{
                    .l = right.l,
                });
                break :b intTy;
            },

            .GreaterThan,
            .LessThan,
            .GreaterEqualThan,
            .LessEqualThan,
            => b: {
                const intTy = try self.definedType(.Int);
                try self.typeContext.unify(left.t, intTy, &.{
                    .l = left.l,
                });
                try self.typeContext.unify(right.t, intTy, &.{
                    .l = right.l,
                });
                const boolTy = try self.definedType(.Bool);
                break :b boolTy;
            },

            .Or,
            .And,
            => b: {
                const boolTy = try self.definedType(.Bool);
                try self.typeContext.unify(left.t, boolTy, &.{
                    .l = left.l,
                });
                try self.typeContext.unify(right.t, boolTy, &.{
                    .l = right.l,
                });
                break :b boolTy;
            },

            .Equals, .NotEquals => b: {
                const eqClass = try self.definedClass(.Eq);
                const eqFun = eqClass.classFuns[0];
                const l = left.l.between(right.l);
                const ifn = try self.instantiateClassFunction(eqFun, l);

                try self.typeContext.unify(left.t, right.t, &.{
                    .l = left.l,
                    .r = right.l,
                });

                const boolTy = try self.definedType(.Bool);
                const funTy = try self.makeType(.{ .Fun = .{
                    .args = [_]AST.Type{ left.t, right.t },
                    .ret = boolTy,
                } });

                try self.typeContext.unify(ifn.t, funTy, &.{ .l = l });

                // TODO: FUNNY!
                if (binop == .Equals) binop = .{ .Equals = ifn.ref };
                if (binop == .NotEquals) binop = .{ .NotEquals = ifn.ref };

                break :b boolTy;
            },

            else => unreachable,
        };

        return self.allocExpr(.{
            .t = exprType,
            .e = .{ .BinOp = .{ .op = binop, .l = left, .r = right } },
            .l = left.l.between(right.l),
        });
    }
}

fn term(self: *Self, minPrec: u32) !*AST.Expr {
    // parse unary prefix.
    // I'm not sure if this is good, but getting a reference of something is not really done outside of function calls / constructors.
    // If *nothing* has been parsed, you may get a reference.
    if (minPrec == 0) if (self.consume(.REF)) |tokref| {
        const n = try self.expression();
        const ptr = (try self.defined(.Ptr)).dataInst;
        const l = self.loc(tokref).between(n.l);
        try self.typeContext.unify(ptr.tyArgs[0].Type, n.t, &.{ .l = l });
        return self.allocExpr(.{
            .e = .{ .UnOp = .{ .op = .Ref, .e = n } },
            .t = ptr.t,
            .l = l,
        });
    };

    // not
    if (minPrec <= comptime binOpPrecedence(.And)) if (self.consume(.NOT)) |nottok| {
        const n = try self.precedenceExpression(binOpPrecedence(.And) + 1); // higher than and
        const boolTy = try self.definedType(.Bool);
        try self.typeContext.unify(n.t, boolTy, &.{ .l = n.l });
        const l = self.loc(nottok).between(n.l);
        return self.allocExpr(.{
            .e = .{ .UnOp = .{ .op = .Not, .e = n } },
            .t = boolTy,
            .l = l,
        });
    };

    // negation (-)
    if (minPrec <= comptime binOpPrecedence(.Divide)) if (self.consume(.MINUS)) |mintok| {
        const n = try self.precedenceExpression(binOpPrecedence(.Divide) + 1); // higher than and
        const intTy = try self.definedType(.Int);
        try self.typeContext.unify(n.t, intTy, &.{ .l = n.l });
        return self.allocExpr(.{
            .e = .{ .UnOp = .{ .op = .Negate, .e = n } },
            .t = intTy,
            .l = self.loc(mintok).between(n.l),
        });
    };

    // TODO: maybe make some function to automatically allocate memory when expr succeeds?
    if (self.consume(.FN)) |tokfun| { // smol hack to allow quick empty lambdas.
        var params = std.ArrayList(*AST.Decon).init(self.arena);

        const env = try Common.allocOne(self.arena, Env.init(self.arena));
        self.scope.beginScope(env);

        var needsBody = false;
        var l = self.loc(tokfun);

        // WARNING: (): whatever gets parsed as a lambda with no args. this might be incorrect behavior when we add tuples.
        if (self.check(.LEFT_PAREN)) {
            if (self.consume(.RIGHT_PAREN)) |rparen| {
                l = l.between(self.loc(rparen));
            } else {
                while (true) {
                    const decon = try self.deconstruction();
                    try params.append(decon);

                    if (self.consume(.RIGHT_PAREN)) |rparen| {
                        l = l.between(self.loc(rparen));
                        break;
                    }
                    try self.devour(.COMMA);
                }
            }

            if (!self.check(.COLON)) {
                // multiline lambda bruh.
                needsBody = true; // basically, I want the body thing to happen later.
            }
        } else if (self.consume(.COLON)) |colonTok| {
            // no params
            // NOTE: current syntax allows a lambda to start with a colon only.
            _ = colonTok;
        } else {
            // single param
            const decon = try self.deconstruction();
            try params.append(decon);
            try self.devour(.COLON);
        }

        // do the types for function type.
        const argTys = try self.arena.alloc(AST.Type, params.items.len);
        for (params.items, 0..) |p, i| {
            argTys[i] = p.t;
        }

        if (needsBody) {
            const lamscopeSave = self.scope.currentScope().*;
            self.scope.endScope();

            const lamExpr = try self.allocExpr(.{
                .t = try self.typeContext.newType(.{
                    .Fun = .{
                        .args = argTys,
                        .env = try self.typeContext.newEnv(null),
                        .ret = try self.typeContext.fresh(),
                    },
                }),
                .e = .{
                    .Lam = .{
                        .params = params.items,
                        .body = .{ .Body = &.{} }, // temporary empty list!
                        .env = &.{},
                    },
                },

                .l = l,
            });

            const oldMode = switch (self.mode) {
                .Simple => |simp| simp,
                .MultilineLambda => |ml| b: {
                    // This means we are trying to define two multiline lambdas on the same line. This is BRUH!
                    try self.reportError(.{
                        .TriedDefiningSecondMultilineLambdaOnSameLine = .{ .loc = l },
                    });

                    break :b ml.prev;
                },
            };
            self.mode = .{ .MultilineLambda = .{
                .prev = oldMode,
                .lamExpr = lamExpr,
                .scope = lamscopeSave,
            } };

            return lamExpr;
        } else {
            const expr = try self.expression();
            self.scope.endScope();

            l = l.between(expr.l);

            return self.allocExpr(.{
                .t = try self.typeContext.newType(.{
                    .Fun = .{
                        .args = argTys,
                        .env = try self.typeContext.newEnv(env.items),
                        .ret = expr.t,
                    },
                }),
                .e = .{
                    .Lam = .{
                        .params = params.items,
                        .body = .{ .Expr = expr },
                        .env = env.items,
                    },
                },

                .l = l,
            });
        }
    } // lambda
    else if (self.consume(.IDENTIFIER)) |v| {
        const dv = try self.instantiateVar(&.{}, v);
        return self.allocExpr(.{
            .t = dv.t,
            .e = .{ .Var = .{ .v = dv.v, .match = dv.m } },
            .l = self.loc(v),
        });
    } // var
    else if (self.consume(.INTRINSIC)) |intrTok| {
        var l = self.loc(intrTok);
        // for intrinsics, we must IMMEDIATELY parse the call - we don't want to deal with them being passed around.
        const fullIntr = intrTok.literal(self.lexer.source);
        const intrName = fullIntr[1..];
        if (Intrinsic.findByName(intrName)) |intr| {
            // parse any required arguments brah.
            var args = std.ArrayList(*AST.Expr).init(self.arena);
            if (intr.args > 0) {
                try self.devour(.LEFT_PAREN);
                for (0..intr.args) |i| {
                    try args.append(try self.expression());
                    if (i != intr.args - 1) {
                        try self.devour(.COMMA);
                    } else {
                        const lastTok = try self.expect(.RIGHT_PAREN);
                        l = l.between(self.loc(lastTok));
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
                    try self.typeContext.unify(ptr.t, args.items[0].t, &.{ .l = args.items[0].l });

                    // arg 2
                    try self.typeContext.unify(try self.definedType(.Int), args.items[1].t, &.{ .l = args.items[1].l });

                    break :b ptr.t;
                },

                .argc => try self.definedType(.Int),
                .argv => b: {
                    const ptr = (try self.defined(.Ptr)).dataInst;
                    try self.typeContext.unify(ptr.tyArgs[0].Type, try self.definedType(.ConstStr), null);
                    break :b ptr.t;
                },

                .memeq => b: {
                    try self.typeContext.unify(args.items[0].t, args.items[1].t, &.{
                        .l = l,
                    });
                    break :b try self.definedType(.Bool);
                },

                .errno => try self.definedType(.Int),
            };

            return self.allocExpr(.{
                .t = t,
                .e = .{
                    .Intrinsic = .{
                        .intr = intr,
                        .args = args.items,
                    },
                },
                .l = l,
            });
        } else {
            // NOTE: after this error there are bound to be shitty errors about trying to call some intrinsic type.
            // Most likely, the intrinsic has args, BUT it's possible we are calling `@undefined` for example, but we end up consuming the call.
            // TODO: think about it, I should probably implement that skipping.
            try self.reportError(.{ .UndefinedIntrinsic = .{
                .name = fullIntr,
                .loc = l,
            } });

            return self.allocExpr(.{
                .t = try self.typeContext.fresh(),
                .e = .{
                    .Intrinsic = .{ // placeholder
                        .intr = .{
                            .ty = .undefined,
                            .args = 0,
                        },
                        .args = &.{},
                    },
                },
                .l = l,
            });
        }
    } // intrinsic
    else if (self.consume(.TYPE)) |con| {
        return try self.qualified(con);
    } // con
    else if (self.consume(.INTEGER)) |i| {
        return self.allocExpr(.{
            .t = try self.definedType(.Int),
            .e = .{ .Int = self.parseInt(i) },
            .l = self.loc(i),
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
    else if (self.consume(.LEFT_BRACE)) |leftTok| {
        const definitionsAndLoc = try self.someRecordDefinition();
        const definitions = definitionsAndLoc.fields;
        const rightLoc = definitionsAndLoc.rightLoc;

        // TODO: deduplicate (and, in this case, error out)
        const typeFields = try self.arena.alloc(AST.TypeF(AST.Type).Field, definitions.len);
        for (definitions, 0..) |def, i| {
            typeFields[i] = .{ .t = def.value.t, .field = def.field };
        }
        const t = try self.typeContext.newType(.{
            .Anon = typeFields,
        });

        return self.allocExpr(.{
            .e = .{ .AnonymousRecord = definitions },
            .t = t,
            .l = self.loc(leftTok).between(rightLoc),
        });
    } // anonymous struct.
    else if (self.consume(.LEFT_SQBR)) |ltok| {
        var listLikeThing = std.ArrayList(*AST.Expr).init(self.arena);
        const elemTy = try self.typeContext.fresh();
        const l: Loc = if (self.consume(.RIGHT_SQBR)) |rtok| b: {
            break :b self.loc(ltok).between(self.loc(rtok));
        } else b: {
            while (true) {
                const expr = try self.expression();
                try self.typeContext.unify(expr.t, elemTy, &.{ .l = expr.l }); // TODO: unify all of them AFTER. Then, you can use the location of the whole list to use as .{ .r } to stand for elemTy.
                try listLikeThing.append(expr);
                if (self.consume(.RIGHT_SQBR)) |rtok| {
                    break :b self.loc(ltok).between(self.loc(rtok));
                }
                try self.devour(.COMMA);
            }
        };

        const class = try self.definedClass(.ListLike);
        const cfun = class.classFuns[0];
        const ifn = try self.instantiateClassFunction(cfun, l);
        const selfType = try self.typeContext.fresh();

        const arrInst = try self.defined(.Array);
        try self.typeContext.unifyNum(arrInst.dataInst.tyArgs[0].Num, try self.typeContext.newNum(.{
            .Literal = @intCast(listLikeThing.items.len),
        }), &.{ .l = l }, &.{ .lfull = arrInst.dataInst.t, .rfull = null });
        try self.typeContext.unify(arrInst.dataInst.tyArgs[1].Type, elemTy, &.{ .l = l });

        const funTy = try self.makeType(.{ .Fun = .{
            .args = [_]AST.Type{arrInst.dataInst.t},
            .ret = selfType,
        } });

        try self.typeContext.unify(ifn.t, funTy, &.{ .l = l });

        const arg = try self.allocExpr(.{
            .e = .{
                .StaticArray = listLikeThing.items,
            },
            .t = arrInst.dataInst.t,
            .l = l,
        });

        const args = try self.arena.alloc(*AST.Expr, 1);
        args[0] = arg;

        const callee = try self.allocExpr(.{
            .e = .{
                .Var = .{
                    .v = .{ .ClassFun = .{
                        .cfun = cfun,
                        .ref = ifn.ref,
                    } },
                    .match = ifn.m,
                },
            },
            .t = funTy,
            .l = l,
        });

        return try self.allocExpr(.{
            .e = .{ .Call = .{
                .callee = callee,
                .args = args,
            } },
            .t = selfType,
            .l = l,
        });
    } // list-like thing
    else {
        return try self.errorExpect("term");
    }
}

// isolate this in a function, because its long AND it shares stuff with the normal function()
fn multilineLambda(self: *Self, tempLoc: Loc) !void {
    const lamMode = self.mode.MultilineLambda;
    self.scope.restoreScope(lamMode.scope);

    // CRAP CODE!!!
    const ret = try self.typeContext.fresh();
    try self.typeContext.unify(self.typeContext.getType(lamMode.lamExpr.t).Fun.ret, ret, null);
    const oldReturnType = self.returnType;
    defer self.returnType = oldReturnType;
    self.returnType = ret;

    // COPYPASTA, but needs defer, so its okay? I might group these statements together in a function?
    const oldTriedReturningAtAll = self.triedReturningAtAll;
    const oldReturned = self.returned;
    defer {
        self.triedReturningAtAll = oldTriedReturningAtAll;
        self.returned = oldReturned;
    }
    self.triedReturningAtAll = false;
    self.returned = .Nah;

    const bod = try self.body();
    var stmts = bod.stmts;
    self.scope.endScope();

    try self.finishBodyAndInferReturnType(&stmts, bod.returnStatus, tempLoc); // TEMP. I should return the location of the last statement (but I don''t have locations in statements yet.')

    lamMode.lamExpr.e.Lam.body.Body = stmts.items;
    lamMode.lamExpr.e.Lam.env = lamMode.scope.env.?.items;

    self.mode = .{ .Simple = lamMode.prev };
}

fn qualified(self: *Self, first: Token) !*AST.Expr {
    if (first.type == .IDENTIFIER) {
        const dv = try self.instantiateVar(&.{}, first);
        return self.allocExpr(.{
            .t = dv.t,
            .e = .{ .Var = .{ .v = dv.v, .match = dv.m } },
            .l = self.loc(first),
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

    var l = self.loc(first);
    loop: while (true) {
        if (self.consume(.TYPE)) |possibleCon| {
            if (self.check(.DOT)) {
                try modpath.append(possibleCon.literal(self.lexer.source));
                l = l.between(self.loc(possibleCon));
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
                .l = l,
            });
        } else {
            return try self.errorExpect("rest of qualification");
        }
    }
}

fn namedRecordDefinition(self: *Self, modpath: Module.Path, name: Token) !*AST.Expr {
    const definitionsAndLoc = try self.someRecordDefinition();
    const fieldsLoc = definitionsAndLoc.rightLoc;
    const definitions = definitionsAndLoc.fields;

    // instantiate it.
    const mDataOrClass = try self.findQualifiedDataOrClass(modpath, name.literal(self.lexer.source), self.loc(name));
    if (mDataOrClass) |dataOrClass| {
        switch (dataOrClass) {
            .Data => |data| {
                switch (data.stuff) {
                    .recs => |dataFields| {
                        const dataInst = try self.instantiateData(data, self.loc(name));
                        const match = dataInst.match;

                        // check if all fields were defined
                        for (dataFields) |dataField| {
                            for (definitions) |def| {
                                if (Common.streq(dataField.field, def.field)) {
                                    try self.typeContext.unify(def.value.t, try self.typeContext.mapType(match, dataField.t), &.{ .l = self.loc(name) });
                                    break;
                                }
                            } else {
                                // when a field is not defined.
                                try self.reportError(.{ .DidNotDefineField = .{
                                    .field = dataField.field,
                                    .loc = fieldsLoc,
                                } });
                            }
                        }

                        return self.allocExpr(.{
                            .t = dataInst.t,
                            .e = .{
                                .NamedRecord = .{
                                    .data = data,
                                    .fields = definitions,
                                },
                            },
                            .l = self.loc(name),
                        });
                    },
                    .cons => {
                        try self.reportError(.{ .DataIsNotARecord = .{ .data = data } });
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

    // PLACEHOLDER EXPR.
    std.debug.assert(self.errors.items.len > 0);
    return try self.allocExpr(.{
        .e = .{ .AnonymousRecord = &.{} },
        .t = try self.typeContext.fresh(),
        .l = self.loc(name),
    });
}

// either anonymous or normal :)
// checks for duplicates.
fn someRecordDefinition(self: *Self) !struct { fields: []AST.Expr.Field, rightLoc: Loc } {
    var definitions = std.ArrayList(AST.Expr.Field).init(self.arena);
    while (true) {
        const fieldTok = try self.expect(.IDENTIFIER);
        const fieldName = fieldTok.literal(self.lexer.source);
        const expr = if (self.check(.COLON)) b: {
            break :b try self.expression();
        } else b: {
            const varInst = try self.instantiateVar(&.{}, fieldTok); // { x } => { x: x }  TODO: maybe disallow anything except simple var definitions. Even more, maybe allow only current scope / env?
            break :b try self.allocExpr(.{
                .t = varInst.t,
                .e = .{ .Var = .{ .v = varInst.v, .match = varInst.m } },
                .l = self.loc(fieldTok),
            });
        };

        // check for duplication.
        for (definitions.items) |field| {
            if (Common.streq(field.field, fieldName)) {
                try self.reportError(.{
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
    const rightLoc = try self.expect(.RIGHT_BRACE);

    return .{ .fields = definitions.items, .rightLoc = self.loc(rightLoc) };
}

fn parseQualifiedType(self: *Self, first: Token) !struct { modpath: Module.Path, name: Str, loc: Common.Location } {
    if (self.peek().type != .DOT) {
        return .{ .modpath = &.{}, .name = first.literal(self.lexer.source), .loc = self.loc(first) };
    }

    var fullPath = try std.ArrayList(Str).initCapacity(self.arena, 1);
    try fullPath.append(first.literal(self.lexer.source));
    var qloc = self.loc(first);
    while (self.check(.DOT)) {
        const tt = try self.expect(.TYPE);
        try fullPath.append(tt.literal(self.lexer.source));
        qloc = qloc.between(self.loc(tt));
    }

    const modpath = fullPath.items[0 .. fullPath.items.len - 1];
    const tyname = fullPath.getLast();

    return .{ .modpath = modpath, .name = tyname, .loc = qloc };
}

// right now only used for records. In the future, will be used for qualifying types themselves.
// ALSO, TODO we might abstract away the stuff about getting the module, because it's annoying and it's not immediately obvious how I should handle that error. So, a fn (modpath) -> ?Module (but also throw error when module == null)
fn findQualifiedDataOrClass(self: *Self, modpath: Module.Path, name: Str, dloc: Common.Location) !?Module.DataOrClass {
    if (modpath.len == 0) {
        if (self.maybeLookupType(name)) |dataOrClass| {
            return dataOrClass;
        } else {
            try self.reportError(.{ .UndefinedType = .{
                .typename = name,
                .loc = dloc,
            } });
            return null;
        }
    } else {
        if (try self.loadModuleFromPath(modpath)) |mod| {
            if (mod.lookupData(name)) |data| {
                return data;
            } else {
                try self.reportError(.{
                    .UndefinedType = .{
                        .typename = name,
                        .loc = dloc,
                    },
                });
                return null;
            }
        } else {
            // circular dep??
            // return null;
            unreachable;
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
        .l = self.loc(name),
    });
}

// TODO: handle errors in literals
// Also, make it legible.
fn stringLiteral(self: *Self, st: Token) !*AST.Expr {
    const og = st.literal(self.lexer.source); // this includes single quotes

    // handling chars
    // currently, a string of 1 character will always be a char.
    // this is obviously bad, since we sometimes want one-char strings.
    // The thing is: kc code should be polymorphic enough to support it,
    // but we sometimes (incorrectly, but out of laziness) rely on ConstStr == const char*, which will be bad and will force us to allocate.
    // if (og.len == 3) { // 1 + two `'`
    //     return try self.allocExpr(.{
    //         .e = .{ .Char = og[1] },
    //         .l = self.loc(st),
    //         .t = try self.definedType(.Char),
    //     });
    // }

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

                    if (last != ci) {
                        const se = try self.constStr(try s.toOwnedSlice(), .{
                            .from = st.from + last, // this is probably incorrect.
                            .to = st.from + ci,
                            .line = self.lexer.line, // should be correct... right?
                            .module = .{
                                .source = self.lexer.source,
                                .name = self.name,
                            },
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

                    // BAD BAD BAD BAD
                    while (og[i] != ')' and og[i] != '.' and og[i] != '&') i += 1;
                    const v = try self.instantiateVar(&.{}, .{
                        .from = st.from + start,
                        .to = st.from + i,
                        .type = .IDENTIFIER,
                        .line = st.line,
                    });
                    var varExpr = try self.allocExpr(.{
                        .e = .{ .Var = .{
                            .v = v.v,
                            .match = v.m,
                        } },
                        .t = v.t,
                        .l = .{
                            .from = st.from + start,
                            .to = st.from + i,
                            .line = st.line,
                            .module = .{
                                .source = self.lexer.source,
                                .name = self.name,
                            },
                        },
                    });

                    while (true) {
                        switch (og[i]) {
                            '.' => {
                                i += 1;
                                const lastLast = i;
                                while (og[i] != ')' and og[i] != '.' and og[i] != '&') i += 1;
                                const field = og[lastLast..i];
                                const fieldLoc = Loc{
                                    .from = st.from + lastLast,
                                    .to = st.from + i,
                                    .line = st.line,
                                    .module = .{
                                        .name = self.name,
                                        .source = self.lexer.source,
                                    },
                                };

                                const t = try self.typeContext.field(
                                    varExpr.t,
                                    field,
                                    &.{
                                        .l = varExpr.l,
                                        .r = fieldLoc,
                                    },
                                );
                                varExpr = try self.allocExpr(.{
                                    .t = t,
                                    .e = .{ .UnOp = .{
                                        .e = varExpr,
                                        .op = .{
                                            .Access = field,
                                        },
                                    } },
                                    .l = varExpr.l.between(fieldLoc),
                                });
                            },
                            '&' => {
                                i += 1;
                                const ptr = (try self.defined(.Ptr)).dataInst;
                                const l = Loc{
                                    .from = st.from + i - 1,
                                    .to = st.from + i,
                                    .line = st.line,
                                    .module = .{
                                        .name = self.name,
                                        .source = self.lexer.source,
                                    },
                                };
                                try self.typeContext.unify(
                                    ptr.t,
                                    varExpr.t,
                                    &.{
                                        .l = varExpr.l,
                                        .r = l,
                                    },
                                );
                                varExpr = try self.allocExpr(.{
                                    .t = ptr.tyArgs[0].Type,
                                    .e = .{
                                        .UnOp = .{ .op = .Deref, .e = varExpr },
                                    },
                                    .l = l,
                                });
                            },
                            ')' => {
                                i += 1;
                                break;
                            },
                            else => unreachable, // error
                        }
                    }

                    if (e) |ee| {
                        e = try self.strConcat(ee, varExpr);
                    } else {
                        e = varExpr;
                    }
                    last = i;
                },
                't' => try s.append('\t'),
                'n' => try s.append('\n'),
                '\\' => try s.append('\\'),
                '0' => try s.append(0),
                else => unreachable, // TODO handle errors
            }
        } else {
            try s.append(c);
            i += 1;
        }
    }

    if (last != i) {
        const se = try self.constStr(try s.toOwnedSlice(), .{
            // NOTE: same problem as the loc definition for string in the beginning.
            .from = st.from + last,
            .to = st.from + i,
            .line = self.lexer.line,
            .module = .{
                .source = self.lexer.source,
                .name = self.name,
            },
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
    return e orelse try self.constStr(&.{}, self.loc(st));
}

fn constStr(self: *Self, s: Str, l: Loc) !*AST.Expr {
    if (s.len != 1) {
        return try self.allocExpr(.{
            .e = .{
                .Str = s,
            },
            .t = try self.definedType(.ConstStr),
            .l = l,
        });
    } else {
        const retTy = try self.typeContext.fresh();
        const class = try self.definedClass(.FromChar);
        const cfun = class.classFuns[0];
        const ifn = try self.instantiateClassFunction(cfun, l);
        const funTy = try self.makeType(.{ .Fun = .{
            .args = [_]AST.Type{try self.definedType(.ConstStr)},
            .ret = retTy,
        } });
        try self.typeContext.unify(ifn.t, funTy, &.{ .l = l });

        const arg = try self.allocExpr(.{
            .e = .{
                .Str = s,
            },
            .t = try self.definedType(.ConstStr),
            .l = l,
        });

        const args = try self.arena.alloc(*AST.Expr, 1);
        args[0] = arg;

        const callee = try self.allocExpr(.{
            .e = .{
                .Var = .{
                    .v = .{ .ClassFun = .{
                        .cfun = cfun,
                        .ref = ifn.ref,
                    } },
                    .match = ifn.m,
                },
            },
            .t = funTy,
            .l = l,
        });

        return try self.allocExpr(.{
            .e = .{ .Call = .{
                .callee = callee,
                .args = args,
            } },
            .t = retTy,
            .l = l,
        });
    }
}

fn strConcat(self: *Self, l: *AST.Expr, r: *AST.Expr) !*AST.Expr {
    const sc = try self.defined(.StrConcat);
    const sci = sc.dataInst;
    try self.typeContext.unify(sci.tyArgs[0].Type, l.t, &.{ .l = l.l });
    try self.typeContext.unify(sci.tyArgs[1].Type, r.t, &.{ .l = r.l });
    const args = try self.arena.alloc(*AST.Expr, 2);
    args[0] = l;
    args[1] = r;
    const tyArgs = try self.arena.alloc(AST.Type, 2);
    tyArgs[0] = args[0].t;
    tyArgs[1] = args[1].t;
    return self.allocExpr(.{
        .t = sci.t,
        .e = .{ .Call = .{
            .callee = try self.allocExpr(.{
                .t = try self.typeContext.newType(.{ .Fun = .{
                    .ret = sci.t,
                    .args = tyArgs,
                    .env = try self.typeContext.newEnv(&.{}),
                } }),
                .e = .{ .Con = &sc.data.stuff.cons[0] },
                .l = l.l.between(r.l),
            }),
            .args = args,
        } },
        .l = l.l.between(r.l),
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
        .SLASH => .Divide,

        // TODO: I think in the future, match on TokenTypes and only after map them. This is very iffy bruh.
        .EQEQ => .{ .Equals = undefined },
        .NOTEQ => .{ .NotEquals = undefined },
        .LT => .LessThan,
        .LTEQ => .LessEqualThan,
        .GT => .GreaterThan,
        .GTEQ => .GreaterEqualThan,
        .OR => .Or,
        .AND => .And,

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

        .Or => 2,
        .And => 3,
        // .Not => 4,

        .Equals => 8,
        .NotEquals => 8,
        .LessThan => 8,
        .LessEqualThan => 8,
        .GreaterThan => 8,
        .GreaterEqualThan => 8,

        .Plus => 12,
        .Minus => 12,
        .Times => 14,
        .Divide => 14,

        // .Negation => 16,

        .Call => 18,
        .RecordAccess => 18,
        .Deref => 18,
        .PostfixCall => 18,
        // else => unreachable,
    };
}

const Type = struct {
    const Constrain = union(enum) {
        Data: WithAssocs,
        ClassFunction: WithAssocs,
        Function: struct { uid: Unique },
        ExternalFunction: struct { uid: Unique },

        const WithAssocs = struct { uid: Unique, assocs: *std.ArrayList(AST.Association) };
    };
    constrain: ?Constrain,
    parser: *Self,

    fn init(self: *Self, constrain: ?Constrain) @This() {
        return .{ .constrain = constrain, .parser = self };
    }

    fn binding(this: *const @This()) ?AST.Binding {
        return if (this.constrain) |c| switch (c) {
            .Data => |e| .{ .Data = e.uid },
            .ClassFunction => |e| .{ .ClassFunction = e.uid },
            .Function => |e| .{ .Function = e.uid },
            .ExternalFunction => |e| .{ .Function = e.uid },
        } else null;
    }

    // type-o
    fn typ(this: *const @This()) ParserError!LocdIn(AST.Type) {
        const self = this.parser;
        // temp
        if (self.consume(.TYPE)) |ty| {
            const ity = try this.qualifiedType(ty);
            if (ity.tyArgs.len != 0) {
                try self.reportError(.{ .MismatchingKind = .{ .data = ity.dataExclusiveShit.?.data, .expect = ity.tyArgs.len, .actual = 0 } });
            }
            return .{ .e = ity.t, .l = self.loc(ty) };
        } else if (self.consume(.IDENTIFIER)) |tv| { // TVAR
            return .{
                .e = try self.typeContext.newType(.{
                    .TVar = try self.lookupTVar(tv, this.binding()),
                }),
                .l = self.loc(tv),
            };
        } else if (self.check(.LEFT_PAREN)) {
            const ty = try this.sepTyo();
            try self.devour(.RIGHT_PAREN);
            return ty;
        } else if (self.consume(.UNDERSCORE)) |tok| {
            if (self.selfType) |t| {
                return .{ .e = t, .l = self.loc(tok) };
            } else {
                // TODO: signal error
                unreachable;
            }
        } else if (self.consume(.LEFT_BRACE)) |leftTok| {
            var fields = std.ArrayList(AST.TypeF(AST.Type).Field).init(self.arena);
            while (true) {
                const field = try self.expect(.IDENTIFIER);
                try self.devour(.COLON);
                const t = try this.sepTyo();
                try fields.append(.{
                    .t = t.e,
                    .field = field.literal(self.lexer.source),
                });

                if (!self.check(.COMMA)) break;
            }
            const rightTok = try self.expect(.RIGHT_BRACE);
            const l = self.loc(leftTok).between(self.loc(rightTok));

            return .{
                .e = try self.typeContext.newType(.{ .Anon = fields.items }),
                .l = l,
            };
        } else {
            try self.errorExpect("type");
        }
        unreachable;
    }

    fn sepTyo(this: *const @This()) !LocdIn(AST.Type) {
        const self = this.parser;
        if (self.consume(.TYPE)) |tyName| {
            const ty = try this.qualifiedType(tyName);

            var tyArgs = std.ArrayList(AST.TypeOrNum).init(self.arena);
            var l = self.loc(tyName);
            var i: usize = 0; // bruh
            while (true) {
                defer i += 1; // BRUH
                const tokType = self.peek().type;
                if (!(tokType == .LEFT_PAREN or tokType == .TYPE or tokType == .IDENTIFIER or tokType == .UNDERSCORE or tokType == .NUMTYNAME or tokType == .INTEGER)) { // bad but works
                    break;
                }

                if (i < ty.tyArgs.len and ty.tyArgs[i].isNum()) { // BRUHHHH
                    const numTy: AST.TypeOrNum = b: {
                        if (self.consume(.NUMTYNAME)) |numtyTok| { // we also accept carets to make sure we are using a number type.
                            const tnum = try self.lookupTNum(numtyTok, this.binding());
                            break :b .{ .Num = try self.typeContext.newNum(.{ .TNum = tnum }) };
                        } else if (self.consume(.IDENTIFIER)) |numtyTok| {
                            const tnum = try self.lookupTNum(numtyTok, this.binding());
                            break :b .{ .Num = try self.typeContext.newNum(.{ .TNum = tnum }) };
                        } else if (self.consume(.INTEGER)) |intTok| {
                            const num = self.parseInt(intTok);
                            break :b .{ .Num = try self.typeContext.newNum(.{ .Literal = num }) };
                        } else if (self.consume(.TYPE)) |_| {
                            unreachable; // TODO: assert that it's a numeric type and get stored value.
                        } else {
                            unreachable; // TODO: error (or just quit and let unification throw an error.)
                        }
                    };
                    try tyArgs.append(numTy);
                    continue;
                }
                const lt = try this.typ();
                l = l.between(lt.l);

                try tyArgs.append(.{ .Type = lt.e });
            }

            // simply check arity.
            if (ty.dataExclusiveShit != null) {
                try self.typeContext.unifyParamsWithTNums(ty.tyArgs, tyArgs.items, &.{ .l = l }, &.{
                    .lfull = ty.t,
                    .rfull = null,
                });
            } else if (tyArgs.items.len > 0) {
                unreachable; // TODO: error that says you cannot apply parameters to class.
            }

            // there's a possibility it's a function!
            if (self.check(.RIGHT_ARROW)) {
                const args = try self.arena.alloc(AST.Type, 1);
                args[0] = ty.t;
                const ret = try this.sepTyo();
                return .{
                    .e = try self.typeContext.newType(.{
                        .Fun = .{
                            .args = args,
                            .ret = ret.e,
                            .env = if (this.binding() != null)
                                // in general case
                                try self.typeContext.newEnv(null)
                            else
                                // in external functions, assume no environment.
                                try self.typeContext.newEnv(&.{}),
                        },
                    }),
                    .l = l.between(ret.l),
                };
            } else {
                return .{ .e = ty.t, .l = l };
            }
        } else if (self.consume(.LEFT_PAREN)) |ltok| {
            // try parse function (but it can also be an extra paren!)
            var l = self.loc(ltok);
            var args = std.ArrayList(AST.Type).init(self.arena);
            if (self.consume(.RIGHT_PAREN)) |rtok| {
                l = l.between(self.loc(rtok));
            } else {
                while (true) {
                    const t = try this.sepTyo();
                    l = l.between(t.l);
                    try args.append(t.e);

                    if (!self.check(.COMMA)) {
                        break;
                    }
                }
                const rtok = try self.expect(.RIGHT_PAREN);
                l = l.between(self.loc(rtok));
            }

            if (self.check(.RIGHT_ARROW)) {
                const ret = try this.typ();
                return .{
                    .e = try self.typeContext.newType(.{ .Fun = .{
                        .ret = ret.e,
                        .args = args.items,
                        .env = try self.typeContext.newEnv(null),
                    } }),
                    .l = l.between(ret.l),
                };
            } else if (args.items.len == 1) { // just parens!
                return .{ .e = args.items[0], .l = l };
            } else if (args.items.len == 0) {
                // only Unit tuple is supported.
                return .{ .e = try self.definedType(.Unit), .l = l };
            } else { // this LOOKS like a tuple, but we don't support tuples yet!
                try self.reportError(.{ .TuplesNotYetSupported = .{} });
                return .{ .e = try self.typeContext.fresh(), .l = l };
            }
        } else if (self.consume(.IDENTIFIER)) |tv| {
            const tvt = try self.typeContext.newType(.{
                .TVar = try self.lookupTVar(tv, this.binding()),
            });
            if (self.consume(.RIGHT_ARROW)) |rtok| {
                const args = try self.arena.alloc(AST.Type, 1);
                args[0] = tvt;
                const ret = try this.sepTyo();
                return .{
                    .e = try self.typeContext.newType(.{ .Fun = .{
                        .args = args,
                        .ret = ret.e,
                        .env = try self.typeContext.newEnv(null),
                    } }),
                    .l = self.loc(tv).between(self.loc(rtok)),
                };
            } else {
                return .{ .e = tvt, .l = self.loc(tv) };
            }
        } else {
            return try this.typ();
        }
        unreachable;
    }

    fn qualifiedType(this: *const @This(), first: Token) !DataOrClassShit {
        const self = this.parser;
        const stuff = try self.parseQualifiedType(first);
        const modpath = stuff.modpath;
        const tyname = stuff.name;
        const qloc = stuff.loc;

        if (try self.findQualifiedDataOrClass(modpath, tyname, qloc)) |dataOrClass| {
            switch (dataOrClass) {
                .Data => |data| {
                    const dt = try self.instantiateData(data, qloc);
                    return .{
                        .t = dt.t,
                        .tyArgs = dt.tyArgs,
                        .dataExclusiveShit = .{
                            .data = data,
                            .match = dt.match,
                        },
                    };
                },

                .Class => |class| {
                    if (this.constrain) |constr| {
                        switch (constr) {
                            .Data, .ClassFunction => |data| {
                                const tv: AST.TVar = .{
                                    .uid = self.gen.tvars.newUnique(),
                                    .name = "miau:3",
                                    .binding = this.binding(),
                                    .inferred = false,
                                    .fields = &.{},
                                };
                                try data.assocs.append(.{
                                    .depends = tv,
                                    .uid = self.gen.assocs.newUnique(),
                                    .class = class,
                                    .concrete = null,
                                });

                                return .{
                                    .t = try self.typeContext.newType(.{ .TVar = tv }),
                                    .tyArgs = &.{},
                                    .dataExclusiveShit = null,
                                };
                            },
                            .Function => {
                                const t = try self.typeContext.fresh();
                                try self.addAssociation(.{
                                    .from = t,
                                    .loc = qloc, // TODO!
                                    .class = class,
                                    .instances = try self.getInstancesForClass(class),
                                    .default = class.default,
                                    .concrete = null,
                                });

                                return .{
                                    .t = t,
                                    .tyArgs = &.{},
                                    .dataExclusiveShit = null,
                                };
                            },
                            .ExternalFunction => unreachable, // TODO: error
                        }

                        unreachable;
                    } else {
                        const t = try self.typeContext.fresh();
                        try self.addAssociation(.{
                            .from = t,
                            .loc = qloc, // TODO!
                            .class = class,
                            .instances = try self.getInstancesForClass(class),
                            .default = class.default,
                            .concrete = null,
                        });

                        return .{
                            .t = t,
                            .tyArgs = &.{},
                            .dataExclusiveShit = null,
                        };
                    }

                    unreachable;
                },
            }
        } else {
            return try self.newPlaceholderType(
                tyname,
                self.loc(first),
            );
        }
    }
};

// resolver zone
fn loadModuleFromPath(self: *Self, path: Module.Path) !?Module {
    if (self.importedModules.get(path)) |mmod| {
        return mmod;
    }

    const mmod = try self.modules.loadModule(.{ .ByModulePath = .{ .base = self.base, .path = path } }, .{});
    try self.importedModules.put(path, mmod);

    // automatically add instances (like muh haskells)
    if (mmod) |mod| {
        try self.addAllInstances(&mod.exports);
    }
    return mmod;
}

fn addAllInstances(self: *Self, exports: *const Module.Exports) !void {
    var it = exports.instances.iterator();
    while (it.next()) |insts| {
        var iit = insts.value_ptr.iterator();
        while (iit.next()) |inst| {
            try self.addInstance(inst.value_ptr.*);
        }
    }
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
    funPtr.* = .{ // TODO: is this all really needed? The scheme is assigned empty in the function() anyways. I should inline all this code in the future.
        .name = thisVar,
        .scheme = AST.Scheme.empty(),
        .env = &.{},
        .params = undefined,
        .ret = undefined,
        .body = undefined,
        .temp__isRecursive = true,
    };
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
        if (try self.loadModuleFromPath(modpath)) |mod| {
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
            // RE: it happens when I try to import the module I'm currently in. (this can be handled before tho, to give the appropriate error)
        }
    }
    const placeholderVar = AST.Var{
        .name = varName,
        .uid = self.gen.vars.newUnique(),
    };
    try self.reportError(.{
        .UndefinedVariable = .{ .varname = placeholderVar, .loc = self.loc(varTok) },
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
    m: *AST.Match,
} {
    const vorfAndScope = try self.lookupVar(modpath, varTok);
    const vorf = vorfAndScope.vorf;
    const cursc = vorfAndScope.sc;
    const varInst: AST.VarInst = switch (vorf) {
        .TNum => |tnum| .{
            .v = .{ .TNum = tnum },
            .t = try self.definedType(.Int),
            .m = try Common.allocOne(self.arena, AST.Match.empty(AST.Scheme.empty())),
        },
        .Var => |vt| .{
            .v = .{ .Var = vt.v },
            .t = vt.t,
            .m = try Common.allocOne(self.arena, AST.Match.empty(AST.Scheme.empty())),
        },
        .Fun => |fun| b: {
            const funTyAndMatch = try self.instantiateFunction(fun, self.loc(varTok));
            break :b .{
                .v = .{ .Fun = fun },
                .t = funTyAndMatch.t,
                .m = funTyAndMatch.m,
            };
        },

        .ClassFun => |cfun| b: {
            const ifn = try self.instantiateClassFunction(cfun, self.loc(varTok));

            const varInst = AST.VarInst{
                .v = .{
                    .ClassFun = .{
                        .cfun = cfun,
                        .ref = ifn.ref,
                    },
                },
                .t = ifn.t,
                .m = ifn.m,
            };

            break :b varInst;
        },

        .Extern => |extfun| {
            const match = try self.instantiateScheme(extfun.scheme, self.loc(varTok));

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

    const isRecursive = switch (varInst.v) {
        .Fun => |fun| fun.temp__isRecursive,
        else => false,
    };

    if (!isRecursive) {
        // TODO: I should make a separate function, but I'm still not sure about the interface.
        var lastScope = self.scope.scopes.iterateFromTop();
        while (lastScope.nextPtr()) |sc| {
            if (sc == cursc) break; // we are in the scope the var was defined in, so don't add it to its env.

            if (sc.env) |env| {
                try env.append(varInst);
            }
        }
    }
    return .{
        .v = switch (varInst.v) {
            .TNum => |tnum| .{ .TNum = tnum },
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

fn instantiateClassFunction(self: *Self, cfun: *AST.ClassFun, l: Loc) !struct {
    ref: AST.InstFunInst,
    t: AST.Type,
    m: *AST.Match,
} {
    const match = try self.instantiateScheme(cfun.scheme, l);

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

    const ref: *?AST.Match.AssocRef = try Common.allocOne(self.arena, @as(?AST.Match.AssocRef, null));

    try self.addAssociation(.{
        .from = classSelf,
        .class = cfun.class,
        .instances = instances,
        .loc = l,
        .default = cfun.class.default,

        .concrete = .{
            .classFun = cfun,
            .ref = ref,
            .to = funTy,
        },
    });

    return .{
        .ref = ref,
        .t = funTy,
        .m = match,
    };
}

fn instantiateFunction(self: *Self, fun: *AST.Function, l: ?Loc) !struct { t: AST.Type, m: *AST.Match } {
    const match = try self.instantiateScheme(fun.scheme, l);

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

// the function name bruh
fn solveAvailableConstraintsAndApplyDefaultsIfPossible(self: *Self) !void {
    while (true) {
        try self.solveAvailableConstraints();

        var appliedAnyDefaults = false;
        if (self.associations.items.len > 0) {
            // try solving defaults.
            for (self.associations.items) |assoc| {
                appliedAnyDefaults = appliedAnyDefaults or try self.maybeApplyDefault(&assoc);
            }
        }

        if (!appliedAnyDefaults) {
            break;
        }
    }
}

// TODO: this should not really be a function thooo
fn maybeApplyDefault(self: *Self, assoc: *const Association) !bool {
    if (assoc.default) |def| {
        const data = try self.instantiateData(def, assoc.loc);
        try self.typeContext.unify(assoc.from, data.t, &.{ .l = assoc.loc.? });
        return true;
    }

    return false;
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
                        if (assoc.concrete) |conc| {
                            // NOTE: modifying self.associations while iterating assocs.
                            const fun: *AST.Function = b: {
                                for (inst.instFuns) |instFun| {
                                    if (instFun.classFunId == conc.classFun.uid) {
                                        const fun = instFun.fun;
                                        break :b fun;
                                    }
                                }

                                // TODO: compiler error: could not find instance function.
                                //  This case should be checked when parsing the selected instance.
                                //  Then here, we would return some placeholder (or the placeholder will be provided then)
                                unreachable;
                            };
                            const funTyAndMatch = try self.instantiateFunction(fun, assoc.loc);
                            const funTy = funTyAndMatch.t;

                            try self.typeContext.unify(conc.to, funTy, if (assoc.loc) |l| &.{ .l = l } else null);

                            conc.ref.* = .{ .InstFun = .{ .fun = fun, .m = funTyAndMatch.m } };
                        } else {
                            // nothing. it's good.
                        }
                    } else {
                        // error
                        try self.reportError(.{
                            .CouldNotFindInstanceForType = .{
                                .data = con.type,
                                .class = assoc.class,
                                .possibilities = assoc.instances,
                                .loc = assoc.loc.?, // TODO: is this safe?
                            },
                        });

                        if (assoc.concrete) |*conc| {
                            conc.ref.* = null;
                        }
                    }

                    _ = self.associations.orderedRemove(i); // TODO: not very efficient with normal ArrayList.
                    i -%= 1; // make sure to adjust index.
                    hadChanges = true;
                },
                .Anon => unreachable, // ??? i dunno
                .Fun => unreachable, // error!

                // BUG?(tvar-in-solving-constraints)
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
                    // try self.reportError(.{ .TVarDoesNotImplementClass = .{ .tv = tv, .class = targetClass } });
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
    tyArgs: []AST.TypeOrNum,
    match: *AST.Match,
};
fn instantiateData(self: *Self, data: *AST.Data, l: ?Loc) !DataInst {
    const match = try self.instantiateScheme(data.scheme, l);

    return .{
        .t = try self.typeContext.newType(.{ .Con = .{
            .type = data,
            .application = match,
        } }),
        .tyArgs = match.tvars,
        .match = match,
    };
}

const DataOrClassShit = struct {
    t: AST.Type,
    tyArgs: []AST.TypeOrNum,

    // it's a class here bruv:
    dataExclusiveShit: ?struct {
        data: *AST.Data,
        match: *AST.Match,
    },
};
fn newPlaceholderType(self: *Self, typename: Str, location: Common.Location) !DataOrClassShit {
    const placeholderType = try Common.allocOne(self.arena, AST.Data{
        .name = typename,
        .uid = self.gen.vars.newUnique(),
        .stuff = .{ .cons = &.{} },
        .scheme = AST.Scheme.empty(),
        .annotations = &.{},
    });
    try self.reportError(.{
        .UndefinedType = .{ .typename = typename, .loc = location },
    });

    const match = try Common.allocOne(self.arena, AST.Match.empty(placeholderType.scheme));
    return .{
        .t = try self.typeContext.newType(.{ .Con = .{
            .type = placeholderType,
            .application = match,
        } }),
        .tyArgs = &.{},

        .dataExclusiveShit = .{
            .data = placeholderType,
            .match = match,
        },
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
fn newTVar(self: *@This(), tvname: Str, binding: ?AST.Binding) !AST.TVar {
    const tv: AST.TVar = .{
        .uid = self.gen.tvars.newUnique(),
        .name = tvname,
        .binding = binding,
        .inferred = false,
        .fields = &.{},
    };
    try self.scope.currentScope().tvars.put(tvname, .{ .TVar = tv });
    return tv;
}

// basically, sometimes when we look up tvars in declarations, we want to define them. slightly hacky, but makes stuff easier.
fn lookupTVar(self: *Self, tvTok: Token, binding: ?AST.Binding) !AST.TVar {
    const tvname = tvTok.literal(self.lexer.source);
    var lastScopes = self.scope.scopes.iterateFromTop();
    while (lastScopes.next()) |cursc| {
        if (cursc.tvars.get(tvname)) |tvOrNum| {
            switch (tvOrNum) {
                .TVar => |tv| {
                    return tv;
                },
                .TNum => {
                    unreachable; // TODO: error
                },
            }
        }
    } else {
        // create a new var then.
        if (binding == null) {
            try self.reportError(.{ .UndefinedTVar = .{
                .tvname = tvname,
                .loc = self.loc(tvTok),
            } });
        }
        return try self.newTVar(tvTok.literal(self.lexer.source), binding);
    }
}

// Num TVar
fn newTNum(self: *Self, name: Str, binding: ?AST.Binding) !AST.TNum {
    const tnum = AST.TNum{
        .uid = self.gen.vars.newUnique(),
        .name = name,
        .binding = binding,
    };
    try self.scope.currentScope().tvars.put(name, .{ .TNum = tnum });
    try self.scope.currentScope().vars.put(name, .{ .TNum = tnum });
    return tnum;
}

fn lookupTNum(self: *Self, tnumTok: Token, binding: ?AST.Binding) !AST.TNum {
    var tvname = tnumTok.literal(self.lexer.source);
    if (tvname[0] == '^') tvname = tvname[1..];

    var lastScopes = self.scope.scopes.iterateFromTop();
    while (lastScopes.next()) |cursc| {
        if (cursc.tvars.get(tvname)) |tvOrNum| {
            switch (tvOrNum) {
                .TNum => |tv| {
                    return tv;
                },
                .TVar => {
                    unreachable; // TODO: error
                },
            }
        }
    } else {
        // create a new var then.
        if (binding == null) {
            try self.reportError(.{ .UndefinedTNum = .{
                .tvname = tvname,
                .loc = self.loc(tnumTok),
            } });
        }
        return try self.newTNum(tvname, binding);
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
            try self.reportError(.{
                .UndefinedCon = .{
                    .conname = conName,
                    .loc = self.loc(conTok),
                },
            });

            // return placeholder var after an error.
            return .{ .con = &data.stuff.cons[0], .t = try self.typeContext.fresh(), .tys = &.{} };
        }
    } else b: {
        if (try self.loadModuleFromPath(modpath)) |mod| {
            if (mod.lookupCon(conName)) |con| {
                break :b con;
            } else {
                unreachable; // TODO ERROR: could not find constructor.
            }
        } else {
            unreachable; // this might be a circular dependency?
        }
    };

    const dt = try self.instantiateData(con.data, self.loc(conTok));

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
fn instantiateScheme(self: *Self, scheme: AST.Scheme, l: ?Loc) !*AST.Match {
    const tvars = try self.arena.alloc(AST.TypeOrNum, scheme.tvars.len);
    for (scheme.tvars, 0..) |tvOrNum, i| {
        tvars[i] = switch (tvOrNum) {
            .TVar => .{ .Type = try self.typeContext.fresh() },
            .TNum => .{ .Num = try self.typeContext.newNum(.Unknown) },
        };
    }

    const envVars = try self.arena.alloc(AST.EnvRef, scheme.envVars.len);
    for (scheme.envVars, 0..) |_, i| {
        envVars[i] = try self.typeContext.newEnv(null);
    }

    const assocs = try self.arena.alloc(?AST.Match.AssocRef, scheme.associations.len);
    for (assocs) |*a| {
        a.* = null; // default VALUE YO
    }

    const tvarMatch = AST.Match{
        .tvars = tvars,
        .envVars = envVars,
        .assocs = assocs,
        .scheme = scheme,
    };

    // should prolly add assocs to "Match", but we don't need em yet.
    for (scheme.associations, assocs) |assoc, *ref| {
        try self.addAssociation(.{
            .from = try self.typeContext.mapType(&tvarMatch, try self.typeContext.newType(
                .{ .TVar = assoc.depends },
            )),
            .instances = try self.getInstancesForClass(assoc.class),
            .loc = l,
            .class = assoc.class,

            .concrete = if (assoc.concrete) |conc| .{
                .to = try self.typeContext.mapType(&tvarMatch, conc.to),

                .classFun = conc.classFun,
                .ref = ref,
            } else null,
        });
    }

    // now members fields
    for (scheme.tvars, tvars) |tvOrNum, tyv| {
        switch (tvOrNum) {
            .TVar => |tv| {
                for (tv.fields) |field| {
                    const fieldTy = try self.typeContext.field(tyv.Type, field.field, null);
                    try self.typeContext.unify(fieldTy, try self.typeContext.mapType(&tvarMatch, field.t), null);
                }
            },

            .TNum => {
                // no fields to instantiate
            },
        }
    }

    return try Common.allocOne(self.arena, tvarMatch);
}

fn mkSchemeforFunction(self: *Self, alreadyDefinedTVars: *const std.StringHashMap(AST.TVarOrNum), params: []*AST.Decon, ret: AST.Type, env: AST.Env, functionId: Unique, constraints_: *const Constraints) !AST.Scheme {
    const expectedBinding = AST.Binding{
        .Function = functionId,
    };

    // Function local stuff.
    var funftvs = TypeContext.FTVs.init(self.arena);
    try self.typeContext.ftvs(&funftvs, ret);
    for (params) |p| {
        try self.typeContext.ftvs(&funftvs, p.t);
    }

    // environment stuff.
    var envftvs = TypeContext.FTVs.init(self.arena);
    for (env) |inst| {
        // TODO: this is incorrect. For functions, I must extract ftvs from UNINSTANTIATED types.
        switch (inst.v) {
            .TNum => {},
            .Var => try self.typeContext.ftvs(&envftvs, inst.t),
            .Fun => |fun| {
                for (fun.params) |p| {
                    try self.typeContext.ftvs(&envftvs, p.t);
                }

                try self.typeContext.ftvs(&envftvs, fun.ret);
            },

            .ClassFun => |vv| {
                const cfun = vv.cfun;
                for (cfun.params) |p| {
                    try self.typeContext.ftvs(&envftvs, p.t);
                }

                try self.typeContext.ftvs(&envftvs, cfun.ret);
            },
        }
    }

    // TEMP: detect free type variables and apply defaults in case they are not from outside.
    // NOTE(invalidate-entries): we can invalidate entries.
    while (true) {
        var defaultsApplied = false;
        for (self.associations.items) |assoc| {
            const from = self.typeContext.getType(assoc.from);
            switch (from) {
                .TyVar => {},
                else => continue, // NOTE: for some reason, it's possible for a non-tyvar to appear here (I guess after applying a default?) If so, continue and let this constraint be solved later.
            }
            if (assoc.default != null and !funftvs.tyvars.contains(.{ .t = assoc.from, .tyv = from.TyVar }) and !envftvs.contains(assoc.from, from.TyVar)) {
                defaultsApplied = defaultsApplied or try self.maybeApplyDefault(&assoc);
            }
        }

        if (!defaultsApplied) {
            break;
        }

        // TODO: THIS SHOULD NOT BE HERE. BAD DESIGN (need better free variables detection)
        try self.solveAvailableConstraints();
    }

    // now, remove the tyvars from env here.
    funftvs.difference(&envftvs);

    // make tvars out of them
    // TODO: assign pretty names ('a, 'b, etc.).
    var tvars = std.ArrayList(AST.TVarOrNum).init(self.arena);

    // add defined tvars in this function.
    var tvit = alreadyDefinedTVars.valueIterator();
    while (tvit.next()) |tvar| {
        try tvars.append(tvar.*);
    }

    var it = funftvs.tyvars.iterator();
    while (it.next()) |e| {
        // NOTE(invalidate-entries): filter in case it was invalidated.
        switch (self.typeContext.getType(e.t)) {
            .TyVar => {},
            else => continue,
        }
        const name = try std.fmt.allocPrint(self.arena, "'{}", .{e.tyv.uid});
        const tv = AST.TVar{
            .name = name,
            .uid = self.gen.tvars.newUnique(),
            .binding = expectedBinding,
            .inferred = true,
            .fields = self.typeContext.getFieldsForTVar(e.tyv) orelse &.{},
        };
        try tvars.append(.{ .TVar = tv });
        const tvt = try self.typeContext.newType(.{ .TVar = tv });
        try self.typeContext.unify(e.t, tvt, null);
    }

    var envs = std.ArrayList(AST.EnvRef).init(self.arena);
    var envIt = funftvs.envs.iterator();
    while (envIt.next()) |e| {

        // NOTE(invalidate-entries): filter in case it was invalidated.
        if (self.typeContext.getEnv(e.*).env != null) {
            continue;
        }
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
                        const assocClass = assoc.class;
                        if (constraints_.get(assocTV)) |constrs| {
                            for (constrs.items) |class| {
                                if (class.uid == assocClass.uid) {
                                    // OKAY!
                                    break :b;
                                }
                            }
                        }

                        // here, it's "bruh"
                        try self.reportError(.{ .TVarDoesNotImplementClass = .{ .class = assocClass, .tv = assocTV } });
                    }

                    const assocID = self.gen.assocs.newUnique();
                    if (assoc.concrete) |conc| {

                        // make sure to check it's actually bound to a function.
                        var assocFTVs = TypeContext.FTVs.init(self.arena); // TODO: this is kinda fugly. I should reuse the general ftvs.
                        defer assocFTVs.deinit();
                        try self.typeContext.ftvs(&assocFTVs, conc.to);

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
                            try tvars.append(.{ .TVar = tv });
                            const tvt = try self.typeContext.newType(.{ .TVar = tv });
                            try self.typeContext.unify(tyv.t, tvt, null);
                        }

                        var assocEnvIt = assocFTVs.envs.iterator();
                        while (assocEnvIt.next()) |e| {
                            try envs.append(e.*);
                        }

                        // here we are adding an existing association to a scheme.
                        // remember to create a uid and pointer-write it to the previous match's association.
                        conc.ref.* = .{ .Id = assocID };
                        try assocs.append(.{
                            .depends = assocTV,
                            .class = assoc.class,
                            .uid = assocID,

                            .concrete = .{
                                .classFun = conc.classFun,
                                .to = conc.to,
                            },
                        });
                    } else {
                        try assocs.append(.{
                            .depends = assocTV,
                            .class = assoc.class,
                            .uid = assocID,
                            .concrete = null,
                        });
                    }

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

    // also add defined constraints! (but it's all bad thoooo)
    try self.addConstraintsToAssocs(&assocs, constraints_);

    return .{
        .tvars = tvars.items,
        .envVars = envs.items,
        .associations = assocs.items,
    };
}

// ASSOCIATION
// NOTE: kind of crappy constraint thing. It should work tho?
pub const Association = struct {
    from: AST.Type,
    loc: ?Loc,
    class: *AST.Class,
    instances: Module.DataInstance,
    default: ?*AST.Data = null, // for now, when generalizing, we DON'T keep defaults. I'm thinking of applying defaults before generalizing, so that returning strings works correctly. Or make a different default type: `late` and `eager`

    // when it's null, it's just a `constraint` and not based on a class function call.
    // when it's a value, it's an actual association with an associated function call.
    concrete: ?struct {
        classFun: *AST.ClassFun,
        ref: *?AST.Match.AssocRef,

        to: AST.Type,
    },
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

    fn restoreScope(self: *@This(), scope: CurrentScope) void {
        self.scopes.push(scope);
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
    tvars: std.StringHashMap(AST.TVarOrNum),
    instances: std.AutoHashMap(*AST.Class, Module.DataInstance),

    env: ?*Env,

    fn init(al: std.mem.Allocator, env: ?*Env) @This() {
        return .{
            .vars = std.StringHashMap(Module.VarOrFun).init(al),
            .types = std.StringHashMap(Module.DataOrClass).init(al),
            .cons = std.StringHashMap(*AST.Con).init(al),
            .tvars = std.StringHashMap(AST.TVarOrNum).init(al),
            .instances = std.AutoHashMap(*AST.Class, Module.DataInstance).init(al),
            .env = env,
        };
    }

    // in the future - scopes are actually safe to deallocate.
    fn deinit() void {}
};

const Env = std.ArrayList(AST.VarInst);

// typechecking zone

// TEST ZIG'S BIG BEAN BURRITO.
fn makeType(self: *Self, t: anytype) !AST.Type {
    if (@hasField(@TypeOf(t), "Fun")) {
        const fun = @field(t, "Fun");
        if (@hasField(@TypeOf(fun), "env")) @compileError("todo");

        const ret = @field(fun, "ret");

        const args = @field(fun, "args");
        const params = try self.arena.alloc(AST.Type, args.len);
        for (args, 0..) |a, i| {
            params[i] = a;
        }

        return try self.typeContext.newType(.{ .Fun = .{
            .args = params,
            .ret = ret,
            .env = try self.typeContext.newEnv(null),
        } });
    } else {
        @compileError("trying to make unknown type brub");
    }
}

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
            .dataInst = try self.instantiateData(data, null), // no location, because it should not happen?
            .data = data,
        };
    } else b: {
        const data = switch (self.maybeLookupType(Prelude.PremadeTypeName.get(predefinedType)) orelse break :b error.PreludeError) {
            .Data => |data| data,
            .Class => |_| break :b error.PreludeError,
        };

        return .{
            .dataInst = try self.instantiateData(data, null), // -||-
            .data = data,
        };
    };
}

fn definedClass(self: *Self, predefinedType: Prelude.PremadeClass) !*AST.Class {
    return if (self.prelude) |prelude| {
        return prelude.definedClass(predefinedType);
    } else b: {
        return switch (self.maybeLookupType(Prelude.PremadeClassName.get(predefinedType)) orelse break :b error.PreludeError) {
            .Data => |_| error.PreludeError,
            .Class => |c| c,
        };
    };
}

// parser zone
fn foldFromHere(self: *Self) ParsingMode {
    const old = self.mode;
    self.mode = .{ .Simple = .{ .CountIndent = 0 } };
    return old;
}

fn finishFold(self: *Self, mode: ParsingMode) !void {
    switch (self.mode) {
        .Simple => |*nmode| switch (nmode.*) {
            .Normal => unreachable,
            .CountIndent => |i| {
                if (i == 1) {
                    try self.devour(.DEDENT); // maybe make not consuming it `unreachable`? since this might not even be possible.
                } else if (i == 0) {
                    try self.endStmt();
                }
            },
        },
        else => unreachable,
    }
    self.mode = mode;
}

fn expect(self: *Self, tt: TokenType) !Token {
    return self.consume(tt) orelse try self.parseError(.{ .UnexpectedToken = .{
        .got = self.currentToken,
        .expected = tt,
    } });
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
        .Simple => |*nmode| switch (nmode.*) {
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
        },
        .MultilineLambda => {},
    }
    return self.currentToken;
}

fn consume(self: *Self, tt: TokenType) ?Token {
    var tok = self.currentToken;
    switch (self.mode) {
        .Simple => |*nmode| switch (nmode.*) {
            .Normal => {},
            .CountIndent => |*ind| while (tok.isWhitespace()) {
                // TODO: COPYPASTA!
                if (tok.type == .STMT_SEP and ind.* == 0) break;
                if (tok.type == .INDENT) ind.* += 1;
                if (tok.type == .DEDENT) {
                    if (ind.* <= 1) break;
                    ind.* -= 1;
                }
                self.skip();
                tok = self.currentToken;
            },
        },

        .MultilineLambda => {},
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
    const Simple = union(enum) {
        Normal,
        CountIndent: u32,
    };
    Simple: Simple,
    MultilineLambda: struct {
        prev: Simple,
        lamExpr: *AST.Expr,
        scope: CurrentScope,
    }, // <- old parsing mode. we can't have two multi line lambdas on the same line.
};

const LexingState = struct {
    lexer: Lexer,
    currentToken: Token,
    parsingMode: ParsingMode,
};

// backtracking!!
fn saveLexingState(self: *const Self) LexingState {
    return .{
        .lexer = self.lexer,
        .currentToken = self.currentToken,
        .parsingMode = self.mode,
    };
}

fn loadLexingState(self: *Self, state: LexingState) void {
    self.lexer = state.lexer;
    self.currentToken = state.currentToken;
    self.mode = state.parsingMode;
}

// NOTE: later, we don't have to specify a return value. Just always follow it with "return unreachable".
fn errorExpect(self: *Self, exp: Str) !noreturn {
    try self.parseError(.{ .UnexpectedThing = .{
        .at = self.currentToken,
        .expected = exp,
    } });
}
fn parseError(self: *Self, err: Error) !noreturn {
    try self.reportError(err);
    return error.ParseError;
}

// fn err(self: *Self, comptime t: type, comptime fmt: []const u8, args: anytype) !t {
//     std.debug.print(fmt ++ " at {} ({s})\n", args ++ .{ self.currentToken, self.name });
//     std.debug.print("{s}\n", .{self.lexer.source[self.currentToken.from -% 5 .. @min(self.lexer.source.len, self.currentToken.to +% 5)]});
//     return error.ParseError;
// }

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

// assume integer is well formed because lexer guarantees it.
fn parseInt(self: *const Self, tok: Token) i64 {
    std.debug.assert(tok.type == .INTEGER);
    return std.fmt.parseInt(i64, tok.literal(self.lexer.source), 10) catch unreachable;
}

fn reportError(self: *const Self, ierr: Error) !void {
    try self.errors.append(.{ .err = ierr, .module = .{
        .name = self.name,
        .source = self.lexer.source,
    } });
}

fn LocdIn(t: type) type {
    return struct {
        l: Loc,
        e: t,
    };
}

fn loc(self: *const Self, t: Token) Loc {
    return t.toLocation(self.lexer.source, self.name);
}
