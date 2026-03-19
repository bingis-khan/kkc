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

pub const GenError = error{OutOfMemory};
pub fn Mono(Back: type) type {
    return struct {
        // TODO: later generalize it to make backends from. Right now, just generate C and see what happens.
        // selfType: type,
        // genFunction: fn () GenError!void,
        // genEnvMod: fn () GenError!void,
        // genStatement: fn (*ast.Stmt) GenError!void,

        ctx: ast.Ctx,
        typeContext: *const TypeContext,
        match: *const ast.Match,
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

        pub fn mono(modules: []ast, roots: []ast.Function.Use, prelude: *const Prelude, typeContext: *TypeContext, backend: *Backend, al: std.mem.Allocator) !void {
            const cgStartTime = try std.time.Instant.now();

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
                            const funapp = ast.Function.FunApp{ .fun = fun, .m = m };
                            try self.useScope.uses.put(funapp, .{ .uses = kv.value_ptr.uses, .defined = true });
                            try Backend.genFunction(self, fun, m);
                            // try self.genEnv(&fun.env.monoInsts, m, null);
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
                                const funapp = ast.Function.FunApp{ .fun = instFun.fun, .m = m };

                                // mark that the env is going to be generated shortly
                                self.useScope.uses.getPtr(funapp).?.defined = true;

                                self.ctx.print(.{ funapp.fun.name, " => ", kv.value_ptr.uses, "\n" });
                                try Backend.genFunction(self, instFun.fun, m);
                                // try self.genEnv(&instFun.fun.env.monoInsts, m, null);

                                const use = try self.useScope.findUses(.{ .fun = instFun.fun, .m = m });
                                try self.tryCompleteEnv(.{ .fun = instFun.fun, .m = m }, kv.value_ptr, use);
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

        pub fn newUseScope(self: *@This()) InstUses {
            return InstUses.init(self.useScope.uses.allocator, self.typeContext, self.useScope);
        }

        // TEMP until I figure out how to handle tymaps in the new sizer.
        fn ts(self: *const Self) TypeSize {
            return .{
                .tyc = self.typeContext,
                .match = self.match,
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
