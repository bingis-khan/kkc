const common = @import("common.zig");
const std = @import("std");
const ast = @import("ast.zig");
const Prelude = @import("Prelude.zig");
const TypeContext = @import("TypeContext.zig");
const Set = @import("Set.zig").Set;
const Module = @import("Module.zig");
const TypeMap = @import("TypeMap.zig").TypeMap;
const Gen = @import("UniqueGen.zig");

// TODO: later generalize it to make backends from. Right now, just generate C and see what happens.
// selfType: type,
// genFunction: fn () GenError!void,
// genEnvMod: fn () GenError!void,
// genStatement: fn (*ast.Stmt) GenError!void,

const GenError = error{OutOfMemory};
ctx: ast.Ctx,
typeContext: *const TypeContext,
match: *ast.Match,
backend: CGen,
useScope: *InstUses,

const InstUses = struct {
    const Use = struct { uses: u32, defined: bool };
    const UseMap = std.HashMap(ast.Function.FunApp, Use, ast.Function.FunApp.Comparator, std.hash_map.default_max_load_percentage);
    uses: UseMap,
    prev: ?*const InstUses,

    fn init(al: std.mem.Allocator, tc: *const TypeContext, prev: ?*const InstUses) @This() {
        return .{
            .uses = UseMap.initContext(al, .{ .typeContext = tc }),
            .prev = prev,
        };
    }

    fn findUses(self: *@This(), fa: ast.Function.FunApp) !*Use {
        if (self.uses.getPtr(fa)) |uses| {
            return uses;
        } else {
            var mcur = self.prev;
            while (mcur) |cur| {
                defer mcur = cur.prev;

                if (cur.uses.get(fa)) |uses| {
                    const cp = try self.uses.getOrPut(fa); // copy it to current level! (make sure to copy it!)
                    std.debug.assert(!cp.found_existing);
                    cp.value_ptr.* = uses;
                    return cp.value_ptr; // return pointer to current scope!
                }
            }

            ast.Ctx.pp(self.uses.ctx.typeContext, .{
                fa.fun.name,
                " >> ",
                fa.m,
            });
            unreachable;
        }
    }
};
const Self = @This();

pub fn mono(modules: []ast, roots: []ast.Function.Use, prelude: Prelude, typeContext: *TypeContext, al: std.mem.Allocator) !void {
    const cgStartTime = try std.time.Instant.now();

    _ = prelude; // autofix
    var hadNewline = false;
    var emptyMatch = ast.Match.empty(ast.Scheme.empty());
    const monoStuff = try findFullFunctionEnvs(typeContext, al, roots);
    _ = monoStuff;
    var firstUseScope = InstUses.init(al, typeContext, null);
    const cgTime = std.time.Instant.since(try std.time.Instant.now(), cgStartTime) / std.time.ns_per_ms;
    std.debug.print("=== mono call graph: {}ms ===\n", .{cgTime});

    const monoStartTime = try std.time.Instant.now();
    var self = Self{
        .ctx = ast.Ctx.init(&hadNewline, typeContext),
        .typeContext = typeContext,
        .match = &emptyMatch,
        .backend = CGen.init(al),
        .useScope = &firstUseScope,
    };
    for (modules) |module| {
        try self.monoScope(module.toplevel);
    }

    const monoTime = std.time.Instant.since(try std.time.Instant.now(), monoStartTime) / std.time.ns_per_ms;
    std.debug.print("=== mono compilation: {}ms ===\n", .{monoTime});
}

pub fn monoScope(self: *Self, stmts: []*ast.Stmt) !void {
    for (stmts) |stmt| {
        switch (stmt.*) {
            .Function => |fun| {
                var it = self.getEnvUses(fun);
                while (it.next()) |kv| {
                    const m = kv.key_ptr.*;
                    const funapp = ast.Function.FunApp{ .fun = fun, .m = m };
                    try self.useScope.uses.put(funapp, .{ .uses = kv.value_ptr.uses, .defined = true });
                    try self.genFunction(fun, m);
                    // try self.genEnv(&fun.env.monoInsts, m, null);
                }
            },
            .Instance => |inst| {
                for (inst.instFuns) |instFun| {
                    // TODO: toposort here to reduce completions.
                    // {
                    //     var iit = self.getEnvUses(instFun.fun);
                    //     self.ctx.print("ALL: ");
                    //     while (iit.next()) |x| {
                    //         self.ctx.print(.{ x.key_ptr.*, " AAAAAAA " });
                    //     }
                    //     self.ctx.print("\n");
                    // }
                    var it = self.getEnvUses(instFun.fun);
                    { // make sure to add them before, but mark them as not defined (we don't know the order!)
                        var iit = it;
                        while (iit.next()) |e| {
                            try self.useScope.uses.put(
                                .{
                                    .fun = instFun.fun,
                                    .m = e.key_ptr.*,
                                },
                                .{
                                    .uses = e.value_ptr.uses,
                                    .defined = false,
                                },
                            );
                        }
                    }
                    while (it.next()) |kv| {
                        const m = kv.key_ptr.*;
                        const funapp = ast.Function.FunApp{ .fun = instFun.fun, .m = m };

                        // mark that the env is going to be generated shortly
                        self.useScope.uses.getPtr(funapp).?.defined = true;

                        self.ctx.print(.{ funapp.fun.name, " => ", kv.value_ptr.uses, "\n" });
                        try self.genFunction(instFun.fun, m);
                        // try self.genEnv(&instFun.fun.env.monoInsts, m, null);

                        const use = try self.useScope.findUses(.{ .fun = instFun.fun, .m = m });
                        try self.tryCompleteEnv(.{ .fun = instFun.fun, .m = m }, kv.value_ptr, use);
                    }
                }
            },
            else => try self.genStmt(stmt),
        }
    }
}

fn tryCompleteEnv(self: *Self, fun: ast.Function.FunApp, v: *const ast.Function.MonoMatchStuff, uses: *const InstUses.Use) !void {
    if (uses.uses == 0) {
        var ecIt = v.completes.iterator();
        while (ecIt.next()) |app| {
            try self.genEnvCompletion(app.*, fun);
            const iv = app.fun.temp__mono.matches.getPtr(app.m).?;
            const iu = try self.useScope.findUses(app.*);
            iu.uses -= 1;

            if (iu.defined) {
                try self.tryCompleteEnv(
                    app.*,
                    iv,
                    iu,
                );
            }
        }
    }
}

// Q: can a function's uses depend on the type of the enclosing function?
// A: actually, no. instances can "conditionally" call functions depending on if they are selected,
//    but when a tyvar gets generalized, the instance resolution happens at callsite of the function that got generalized.
//    So, we can use only the function to find instances
//    So, the environment is the same for all monos of a function BAR the ones in scheme (obv.)
// fn generateFunctions(self: *Self, fun: *ast.Function) !void {
//     _ = self;
//     _ = fun;
//     unreachable;
// }

// this should generate functions if needed.
fn getEnvUses(self: *Self, fun: *ast.Function) ast.Function.MonoMatches.Iterator {
    _ = self;
    return fun.temp__mono.matches.iterator();
}

// fn gatherTopLevels(modules: []ast, prelude: Prelude, typeContext: *const TypeContext, al: std.mem.Allocator) ![]ast.EnvVar {}

// const Uses = std.HashMap(MonoFunction, comptime V: type, comptime Context: type, comptime max_load_percentage: u64);
const MonoFunction = struct { fun: *ast.Function, m: *ast.Match };
const MonoEnv = struct {
    env: *ast.Env,
    m: *ast.Env,
};

// roots -> call graph
// or really?
const MonoStuff = struct {
    al: std.mem.Allocator,
    typeContext: *TypeContext,
    match: *const ast.Match,
    // typeMap: ?*const TypeMap,
    // TEMP: for now, we are using the monoenv in *Env.
    // functionEnvs: Envs,

    // const Envs = std.AutoHashMap(*ast.Function, *ast.Env);
    const Error = error{OutOfMemory};

    fn init(tc: *TypeContext, al: std.mem.Allocator) !@This() {
        return .{
            .typeContext = tc,
            .match = try common.allocOne(al, ast.Match.empty(ast.Scheme.empty())),
            .al = al,
        };
    }

    fn expandRoot(self: *@This(), root: ast.Function.Use) Error!void {
        const ifn = self.usePair(root);
        try self.expandFunction(ifn.fun, ifn.m);
    }

    fn usePair(self: *@This(), use: ast.Function.Use) struct {
        fun: *ast.Function,
        m: *const ast.Match,
    } {
        return switch (use) {
            .ClassFun => |cfun| switch (cfun.ref.*.?) {
                .InstFun => |ifn| .{ .fun = ifn.fun, .m = ifn.m },
                .Id => |iid| {
                    // ast.Ctx.pp(self.typeContext, .{ cfun.cfun.name, ": ", self.match.* });
                    const ifn = self.match.tryGetFunctionByID(iid).?;
                    return .{ .fun = ifn.fun, .m = ifn.m };
                },
            },
            .Fun => |fun| .{ .fun = fun.fun, .m = fun.m },
        };
    }

    fn expandFunction(self: *@This(), fun: *ast.Function, um: *const ast.Match) Error!void {
        const oldMatch = self.match;
        // TODO: i think storing a match only is gonna fail for nested functions which uses variables and assocs from outer function.
        self.match = try self.typeContext.mapMatch(self.match, um); //try self.
        defer self.match = oldMatch;

        // TODO: cut off early if Match matches. Problem is if matches of outer functions match. How to solve it?
        const env = fun.env;
        const envfun = ast.EnvFun{
            .fun = fun,
            .env = env,
        };

        // ast.Ctx.pp(self.typeContext, .{ "MIAU(", fun.name, "): ", self.match });
        const vp = try fun.temp__mono.matches.getOrPut(self.match);
        if (!vp.found_existing) {
            for (fun.env.insts.items) |inst| {
                switch (inst.v) {
                    .TNum => {
                        unreachable; // TODO
                        // basically, should inner functions make that tnum their env parameter or scheme parameter?
                        // right now it's env parameter, but I might/should change it.
                    },

                    // in current algorithm ignore, since we'll go through all Fun and ClassFuns. (we can actually add them here and don't bother doing it later, but whatever.)
                    .Fun => {
                        try self.addToEnv(envfun, inst);
                    },
                    .ClassFun => |cfun| {
                        const funn = self.usePair(.{ .ClassFun = .{ .cfun = cfun.cfun, .ref = cfun.ref } });
                        try self.addToEnv(envfun, .{
                            .v = .{ .Fun = funn.fun },
                            .m = funn.m,
                            .t = inst.t,
                            .l = inst.l,
                        });
                    },

                    .Var => {
                        try self.addToEnv(envfun, inst);
                    },
                }
            }

            std.debug.assert(!vp.found_existing);
            vp.value_ptr.* = .{
                .uses = 0,
                .completes = ast.Function.EnvCompletes.initContext(
                    self.al,
                    .{ .typeContext = self.typeContext },
                ),
            };

            for (fun.temp__mono.uses.items) |use| {
                try self.expandRoot(use);
            }

            // vp might have changed. find it again!!
            const avp = fun.temp__mono.matches.getPtr(self.match).?;

            // now, we must check which parts of the environment need to be completed later.
            // (only needed for insts thooo)
            for (self.match.assocs, 0..) |massoc, i| {
                const afun = b: {
                    if (massoc) |assoc| {
                        break :b assoc.InstFun;
                    } else {
                        // it's null either when it's a constraint not associated with a function or it's a bug.
                        // here, we want to check if it's actually a bug.
                        if (fun.scheme.associations[i].concrete == null) continue; // good, it's a non-actionable constraint.

                        // here, it's a bug, so
                        unreachable;
                    }
                };
                if (afun.fun.isDefinedAfterOrAt(fun)) {
                    try afun.fun.temp__mono.matches.getPtr(afun.m).?.completes.insert(.{ .fun = fun, .m = self.match });
                    avp.uses += 1;
                }
            }
        } else {
            //     var envIt = fun.env.monoInsts.iterator();
            //     while (envIt.next()) |inst| {
            //         switch (inst.v) {
            //             .ClassFun => |cfun| {
            //                 try self.expandRoot(.{
            //                     .ClassFun = .{
            //                         .cfun = cfun.cfun,
            //                         .ref = cfun.ref,
            //                     },
            //                 });
            //             },
            //             .Fun => |ifun| {
            //                 try self.expandRoot(.{
            //                     .Fun = .{
            //                         .fun = ifun,
            //                         .m = inst.m,
            //                     },
            //                 });
            //             },
            //             .Var => try self.addToEnv(env.outer, inst.*),
            //             .TNum => unreachable,
            //         }
            //     }
        }
    }

    fn addToEnv(self: *@This(), startEnv: ?ast.EnvFun, umInst: ast.EnvVar) !void {
        var inst = umInst;
        inst.m = try self.typeContext.mapMatch(self.match, umInst.m);

        var curenv: ?ast.EnvFun = startEnv;
        while (curenv) |env| {
            if (env.env.level >= inst.l) {
                try env.env.monoInsts.insert(inst);
            } else {
                break;
            }
            curenv = env.env.outer;
        }
    }
};

fn findFullFunctionEnvs(tc: *TypeContext, al: std.mem.Allocator, roots: []ast.Function.Use) !MonoStuff {
    var monoStuff = try MonoStuff.init(tc, al);
    for (roots) |root| {
        try monoStuff.expandRoot(root);
    }

    return monoStuff;
}

fn newUseScope(self: *@This()) InstUses {
    return InstUses.init(self.useScope.uses.allocator, self.typeContext, self.useScope);
}

//////////////// BACKEND

const CGen = struct {
    gen: Gen,

    main: Buf,

    fn init(al: std.mem.Allocator) @This() {
        return .{
            .gen = Gen.init(),
            .main = Buf.init(al),
        };
    }
};
const Buf = std.ArrayList(u8);

fn genFunction(self: *Self, fun: *ast.Function, m: *const ast.Match) GenError!void { // TODO: params
    // const funId = self.backend.gen.newUnique();
    self.ctx.print(.{ fun.name, " ", m });

    const oldM = self.ctx.mapTypes;
    self.ctx.mapTypes = m;
    defer self.ctx.mapTypes = oldM;

    const c = self.ctx;
    c.print(.{" ("});
    c.sepBy(fun.params, ", ");
    c.s(")");
    fun.env.print(c);
    // c.encloseSepBy(fun.temp__calls.items, ", ", "[", "]");
    c.s(" -> ");
    fun.ret.print(c);
    c.s(" ## ");
    fun.scheme.print(c);
    c.s("\n");

    {
        self.ctx.indent += 1;
        defer self.ctx.indent -= 1;

        const oldUseScope = self.useScope;
        defer self.useScope = oldUseScope;
        var curUseScope = self.newUseScope();
        self.useScope = &curUseScope;

        try self.monoScope(fun.body);
    }
    self.ctx.print(.{ "ENV: [", ast.Ctx.iter(fun.env.monoInsts.iterator(), ", "), "] ", m, "\n" });
}

// this is the "public api" part
fn genStmt(self: *Self, stmt: *ast.Stmt) !void {
    const b = &self.backend;
    _ = b;
    switch (stmt.*) {
        // .VarDec => |vd| {
        //     // vd.varDef
        //     _ = vd;
        // },
        // .Expr => |expr| {
        //     try self.genExpr(expr);
        //     self.ctx.print("\n");
        // },

        // gets eliminated beforehand.
        .Function => unreachable,
        .Instance => unreachable,

        else => {
            self.ctx.print(stmt.*);
        }, //TEMP
    }
}

fn genExpr(self: *Self, expr: *ast.Expr) !void {
    _ = self;
    switch (expr.e) {
        else => unreachable,
    }
}

// currently generating env in genFunction
// fn genEnv(self: *Self, env: *const ast.Env.Mono, m: *const ast.Match, exclusions: ?struct {}) !void { // TODO: exclusions
//     _ = exclusions;
//     self.ctx.print(.{ "ENV: [", ast.Ctx.iter(env.iterator(), ", "), "] ", m, "\n" });
// }

fn genEnvCompletion(self: *Self, incompleteEnv: ast.Function.FunApp, completedEnv: ast.Function.FunApp) !void {
    self.ctx.print(.{ "||", incompleteEnv.fun.name, " ", incompleteEnv.m, "\n^^", completedEnv.fun.name, " ", completedEnv.m, "\n" });
}
