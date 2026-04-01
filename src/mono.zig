const common = @import("common.zig");
const Str = common.Str;
const std = @import("std");
const ast = @import("ast.zig");
const Prelude = @import("Prelude.zig");
const TypeContext = @import("TypeContext.zig");
const Set = @import("Set.zig").Set;
const Module = @import("Module.zig");
const TypeMap = @import("TypeMap.zig").TypeMap;
const Gen = @import("UniqueGen.zig");
const sizer = @import("sizer.zig");
const TypeSize = sizer.TypeSize;

pub const Debug = false;

pub const GenError = anyerror;
pub fn Mono(Back: type) type {
    return struct {
        // TODO: later generalize it to make backends from. Right now, just generate C and see what happens.
        // selfType: type,
        // genFunction: fn () GenError!void,
        // genEnvMod: fn () GenError!void,
        // genStatement: fn (*ast.Stmt) GenError!void,

        ctx: ast.Ctx,
        typeContext: *TypeContext,
        tymap: *const TypeMap, // IS THIS NEEDED?
        backend: *Backend,
        useScope: *InstUses,
        prelude: *const Prelude,

        pub const Backend = Back;

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

                    if (Debug) {
                        ast.Ctx.pp(self.uses.ctx.typeContext, .{
                            fa.fun.name,
                            " >> ",
                            fa.m,
                        });
                    }
                    unreachable;
                }
            }
        };
        const Self = @This();

        pub fn mono(modules: []ast, roots: []ast.Function.Use, prelude: *const Prelude, typeContext: *TypeContext, backend: *Backend, al: std.mem.Allocator) !void {
            const cgStartTime = try std.time.Instant.now();

            var hadNewline = false;
            const monoStuff = try findFullFunctionEnvs(typeContext, al, roots);

            if (Debug) {
                modules[modules.len - 1].print(ast.Ctx.init(&hadNewline, typeContext));
            }

            _ = monoStuff;
            var firstUseScope = InstUses.init(al, typeContext, null);
            const cgTime = std.time.Instant.since(try std.time.Instant.now(), cgStartTime) / std.time.ns_per_ms;
            std.debug.print("=== mono call graph: {}ms ===\n", .{cgTime});

            const monoStartTime = try std.time.Instant.now();
            var self = Self{
                .ctx = ast.Ctx.init(&hadNewline, typeContext),
                .typeContext = typeContext,
                .tymap = &TypeMap.Empty,
                .backend = backend,
                .useScope = &firstUseScope,
                .prelude = prelude,
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
                            const mm = try self.typeContext.mapMatch(self.tymap, m);
                            const funapp = ast.Function.FunApp{ .fun = fun, .m = mm };
                            if (Debug) {
                                self.ctx.print(.{ "gen fun: ", fun.name, " ", fun.env, " ", mm, "\n" });
                            }
                            try self.useScope.uses.put(funapp, .{ .uses = kv.value_ptr.uses, .defined = true });
                            {
                                const oldM = self.tymap;
                                defer self.tymap = oldM;
                                self.tymap = &TypeMap.init(mm, oldM);

                                try Backend.genFunction(self, fun);
                                // try self.genEnv(&fun.env.monoInsts, m, null);
                            }
                        }
                    },
                    .Instance => |inst| {
                        for (inst.instFuns) |instFun| {
                            // TODO: toposort here to reduce completions.
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
                                const mm = try self.typeContext.mapMatch(self.tymap, m);
                                const funapp = ast.Function.FunApp{ .fun = instFun.fun, .m = mm };

                                // mark that the env is going to be generated shortly
                                self.useScope.uses.getPtr(funapp).?.defined = true;

                                if (Debug) {
                                    self.ctx.print(.{ funapp.fun.name, " => ", kv.value_ptr.uses, "\n" });
                                }
                                {
                                    const oldM = self.tymap;
                                    defer self.tymap = oldM;
                                    self.tymap = &TypeMap.init(mm, oldM);

                                    try Backend.genFunction(self, instFun.fun);

                                    const use = try self.useScope.findUses(.{ .fun = instFun.fun, .m = mm });
                                    try self.tryCompleteEnv(.{ .fun = instFun.fun, .m = mm }, kv.value_ptr, use);
                                }
                            }
                        }
                    },
                    else => try Backend.genStmt(self, stmt),
                }
            }
        }

        fn tryCompleteEnv(self: *Self, fun: ast.Function.FunApp, v: *const ast.Function.MonoMatchStuff, uses: *const InstUses.Use) !void {
            if (uses.uses == 0) {
                var ecIt = v.completes.iterator();
                while (ecIt.next()) |app| {
                    try Backend.genEnvCompletion(self, app.*, fun);
                    const iv = app.fun.temp__mono.matches.getPtr(app.m).?;
                    const iu = try self.useScope.findUses(app.*);
                    iu.uses -= 1;

                    if (iu.defined) {
                        const oldM = self.tymap;
                        defer self.tymap = oldM;
                        const mm = try self.typeContext.mapMatch(self.tymap, app.m);
                        self.tymap = &TypeMap.init(mm, oldM);

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
                    .tymap = TypeMap.init(&ast.Match.Empty, null),
                    .al = al,
                };
            }

            // fn expandRoot(self: *@This(), root: ast.Function.Use) Error!void {
            //     const ifn = self.usePair(root);
            //     try self.expandFunction(ifn.fun, ifn.m);
            // }

            fn usePair(self: *@This(), use: ast.Function.Use) struct {
                fun: *ast.Function,
                m: *const ast.Match,
            } {
                return switch (use) {
                    .ClassFun => |cfun| switch (cfun.ref.*.?) {
                        .InstFun => |ifn| .{ .fun = ifn.fun, .m = ifn.m },
                        .Id => |iid| {
                            // ast.Ctx.pp(self.typeContext, .{ cfun.cfun.name, ": ", self.match.* });
                            const ifn = self.tymap.tryGetFunctionByID(iid).?;
                            return .{ .fun = ifn.fun, .m = ifn.m };
                        },
                    },
                    .Fun => |fun| .{ .fun = fun.fun, .m = fun.m },
                };
            }

            fn expandFunction(self: *@This(), fun: *ast.Function, um: *const ast.Match) Error!void {
                if (!fun.env.monoFinished) {
                    defer fun.env.monoFinished = true;

                    // add stuff from the environment here
                    // NOTE(env-escaping): check for envs from the inside.
                    var outerFTVs = TypeContext.FTVs.init(self.al);
                    defer outerFTVs.deinit();
                    for (fun.env.insts.items) |inst| {
                        // JUST IN CASE DON'T ADD INSTS HERE, BECAUSE MATCH CAN CHANGE.

                        // LAZINESS - I'll just add all the envs.
                        switch (inst.v) {
                            .Fun => {
                                try self.typeContext.ftvsFromMatch(&outerFTVs, inst.m);
                            },
                            .ClassFun => {
                                try self.typeContext.ftvsFromMatch(&outerFTVs, inst.m);
                            },

                            else => {},
                        }
                    }

                    // now, remove the function envs from scheme.
                    const outerEnvs = &outerFTVs.envs;
                    for (fun.scheme.envVars) |ev| {
                        // ev should be already "base"d, so no need to normalize it with getEnv().base
                        outerEnvs.delete(ev);
                    }

                    // NOTE(env-escaping) instead of looking for outer tvars/envs/assocs, we just extend the environment scheme with outer schemes, so it can be mapped.
                    var outerEnvIt = outerEnvs.iterator();
                    while (outerEnvIt.next()) |oe| {
                        const ee = self.typeContext.getEnv(oe.*);
                        if (ee.env.*) |e| {
                            var mef = e.env.outer;
                            var envmatch = e.match;
                            while (mef) |ef| {
                                defer mef = ef.env.outer;
                                if (ef.fun) |outerfun| {
                                    envmatch = try envmatch.joinScheme(&outerfun.scheme, self.typeContext, self.al);
                                }
                            }

                            // SUSSY
                            ee.env.*.?.match = envmatch;
                        }
                    }

                    for (fun.env.insts.items) |inst| {
                        try fun.env.monoInsts.insert(inst);
                    }

                    for (fun.scheme.associations) |assoc| {
                        if (assoc.concrete) |conc| {
                            try fun.env.monoInsts.insert(.{
                                .v = .{ .ClassFun = .{ .cfun = conc.classFun, .ref = conc.ref } },
                                .t = conc.to,
                                .m = conc.match,
                                .l = conc.classFun.class.level,
                            });
                        }
                    }

                    for (fun.temp__mono.uses.items) |use| {
                        switch (use) {
                            .Fun => |calledFun| {
                                if (calledFun.fun.env.level > fun.env.level) {
                                    try self.expandFunction(calledFun.fun, calledFun.m);
                                }
                            },
                            .ClassFun => |cfun| {
                                switch (cfun.ref.*.?) {
                                    .Id => {}, // don't do anything, since it's a function from outside.
                                    .InstFun => |calledFun| {
                                        if (calledFun.fun.env.level > fun.env.level) {
                                            try self.expandFunction(calledFun.fun, calledFun.m);
                                        }
                                    },
                                }
                            },
                        }
                    }
                } // end of function shit

                matchAlreadyRegistered: {
                    {
                        const gp = try fun.temp__mono.matches.getOrPut(um);
                        if (gp.found_existing) break :matchAlreadyRegistered;
                        gp.value_ptr.* = .{ .completes = ast.Function.EnvCompletes.initContext(self.al, .{ .typeContext = self.typeContext }), .uses = 0 };
                    }

                    // var hadNewline = false;
                    // var c = ast.Ctx.init(&hadNewline, self.typeContext);
                    // c.print(.{ "fun ", fun.name, " :: ", um, "\n" });

                    //
                    // vp might have changed. find it again!!

                    // expand with current match
                    var instIt = fun.env.monoInsts.iterator();
                    while (instIt.next()) |inst| {
                        var iinst = inst.*;
                        iinst.m = try self.typeContext.mapMatch(um, inst.m);

                        switch (inst.v) {
                            .Fun => |ifn| {
                                if (ifn.env.nextFunction() == fun.env.nextFunction()) { // if in the same scope.
                                    try self.expandFunction(ifn, iinst.m);
                                } else {
                                    try self.addToEnv(fun.env.outer, iinst);
                                }
                            },
                            .ClassFun => |cfun| {
                                switch (cfun.ref.*.?) {
                                    .Id => |id| {
                                        if (um.getFunctionOrIDByID(id)) |r| {
                                            switch (r) {
                                                .InstFun => |ipair| {
                                                    const ifn = ipair.fun;
                                                    if (ifn.env.nextFunction() == fun.env.nextFunction()) { // if in the same scope.
                                                        try self.expandFunction(ifn, ipair.m);
                                                    } else {
                                                        // try self.addToEnv(fun.env.outer, iinst);
                                                    }
                                                },

                                                .Id => {
                                                    // do nothing, i guess? it's already in this environment, and it should be added to the next env too?
                                                },
                                            }
                                        } else {
                                            try self.addToEnv(fun.env.outer, iinst);
                                        }
                                    },
                                    .InstFun => unreachable,
                                }
                            },
                            .Var => {
                                // TODO: technically we can do it once for variables, since we don't have to remap variables.
                                try self.addToEnv(fun.env.outer, iinst);
                            },
                            else => unreachable,
                        }
                    }

                    // now, we must check which parts of the environment need to be completed later.
                    // (only needed for insts thooo)
                    for (um.assocs, 0..) |massoc, i| {
                        const afun = b: {
                            if (massoc) |assoc| {
                                switch (assoc) {
                                    .InstFun => |ifun| break :b ifun,
                                    .Id => continue,
                                }
                            } else {
                                // it's null either when it's a constraint not associated with a function or it's a bug.
                                // here, we want to check if it's actually a bug.
                                if (fun.scheme.associations[i].concrete == null) continue; // good, it's a non-actionable constraint.

                                // here, it's a bug, so
                                unreachable;
                            }
                        };

                        if (afun.fun.env.nextFunction() == fun.env.nextFunction()) { // if in the same scope.
                            try self.expandFunction(afun.fun, afun.m);
                        } else {
                            // try self.addToEnv(fun.env.outer, .{
                            //     .v = .{ .Fun = afun.fun },
                            //     .m = afun.m,
                            //     .t = undefined,
                            //     .l = undefined,
                            // });
                            // do nothing. this function should already be added to the previous environment!
                        }

                        if (afun.fun.isDefinedAfterOrAt(fun)) {
                            // {
                            // var hadNewline = false;
                            // var ctx = ast.Ctx.init(&hadNewline, self.typeContext);
                            // ctx.print(.{ afun.m, "\n" });
                            // var it = afun.fun.temp__mono.matches.iterator();
                            // while (it.next()) |e| {
                            //     ctx.print(.{ "\t", e.key_ptr.*, "\n" });
                            // }
                            // ctx.print("\n");
                            // }
                            try afun.fun.temp__mono.matches.getPtr(afun.m).?.completes.insert(.{ .fun = fun, .m = um });
                            fun.temp__mono.matches.getPtr(um).?.uses += 1;
                        }
                    }
                }

                // if there was already a match, no need to expand.
            }

            fn addToEnv(self: *@This(), startEnv: ?ast.EnvFun, umInst: ast.EnvVar) !void {
                _ = self;
                // var inst = umInst;
                // inst.m = try self.typeContext.mapMatch(self.tymap, umInst.m);

                var curenv: ?ast.EnvFun = startEnv;
                while (curenv) |envfun| {
                    const env = envfun.env;
                    if (env.level <= umInst.l) {
                        return;
                    }

                    try envfun.env.monoInsts.insert(umInst);

                    if (envfun.fun) |_| {
                        return;
                    }

                    curenv = envfun.env.outer;
                }
            }
        };

        fn findFullFunctionEnvs(tc: *TypeContext, al: std.mem.Allocator, roots: []ast.Function.Use) !MonoStuff {
            var monoStuff = MonoStuff{
                .al = al,
                .match = &ast.Match.Empty,
                .typeContext = tc,
            };
            for (roots) |root| {
                switch (root) {
                    .ClassFun => |cfun| {
                        switch (cfun.ref.*.?) {
                            .InstFun => |fun| {
                                try monoStuff.expandFunction(fun.fun, fun.m);
                            },
                            .Id => unreachable, // actually unreachable cuz these are top level.
                        }
                    },
                    .Fun => |fun| try monoStuff.expandFunction(fun.fun, fun.m),
                }
            }

            return monoStuff;
        }

        pub fn newUseScope(self: *@This()) InstUses {
            return InstUses.init(self.useScope.uses.allocator, self.typeContext, self.useScope);
        }

        // TEMP until I figure out how to handle tymaps in the new sizer.
        fn ts(self: *const Self) TypeSize {
            return .{
                .tyc = self.typeContext,
                .match = self.tymap.match,
                .prelude = self.prelude,
            };
        }

        pub fn sizeOf(self: *const Self, t: ast.Type) sizer.Size {
            var tsz = self.ts();
            return tsz.sizeOf(t);
        }

        pub fn getFieldOffsetFromType(self: *Self, t: ast.Type, mem: Str) usize {
            var tsz = self.ts();
            return tsz.getFieldOffsetFromType(t, mem);
        }
    };
}
