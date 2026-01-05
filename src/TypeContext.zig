const std = @import("std");
const ast = @import("ast.zig");
const UniqueGen = @import("UniqueGen.zig");
const Unique = UniqueGen.Unique;
const @"error" = @import("error.zig");
const Errors = @"error".Errors;
const Error = @"error".Error;
const common = @import("common.zig");
const Str = common.Str;
const Set = @import("Set.zig").Set;
const Loc = common.Location;

// Personal Note: bruh, I mean, an allocator for this would basically be the same.
// I guess this is more local.
const TyRef = ast.TyRef;
const TyStoreElem = union(enum) {
    Ref: TyRef,
    Type: ast.TypeF(TyRef), // not necessarily good, because it takes a lot of space. Ideally, it would be a second index to an array of actual immutable AST types.
};
const TyStore = std.ArrayList(TyStoreElem);

const EnvRef = ast.EnvRef;
const EnvStore = std.ArrayList(union(enum) {
    Ref: EnvRef,
    Env: ?ast.Env,
});

// instead of putting it in tyvars, make a separate map (as only a minority of tyvars will have any fields.)
const TyVarFields = std.HashMap(
    ast.TyVar,
    std.ArrayList(ast.Record),
    ast.TyVar.comparator(),
    std.hash_map.default_max_load_percentage,
);

context: TyStore,
envContext: EnvStore,
tyvarFields: TyVarFields,
gen: UniqueGen,
errors: *Errors, // pointer to the global error array (kinda bad design.)
arena: std.mem.Allocator, // for `mapType` functions

const Self = @This();
pub fn init(al: std.mem.Allocator, errors: *Errors) !Self {
    const context = TyStore.init(al);

    return .{
        .context = context,
        .envContext = EnvStore.init(al),
        .tyvarFields = TyVarFields.init(al),
        .gen = UniqueGen.init(),
        .errors = errors,
        .arena = al,
    };
}

pub fn fresh(self: *Self) !ast.Type {
    const tid = self.gen.newUnique();
    return self.newType(.{
        .TyVar = .{
            .uid = tid,
            // .fields = std.ArrayList(ast.Record).init(self.arena),
        },
    });
}

pub fn newType(self: *Self, t: ast.TypeF(TyRef)) !ast.Type {
    try self.context.append(.{ .Type = t });
    const tid = self.context.items.len - 1;
    return .{ .id = tid };
}

pub fn newEnv(self: *Self, e: ?ast.Env) !ast.EnvRef {
    try self.envContext.append(.{ .Env = e });
    const envid = self.envContext.items.len - 1;
    return .{ .id = envid };
}

const Locs = ?*const struct { l: Loc, r: ?Loc = null }; // NOTE: when null, then mismatch should not happen!
const Full = *const struct { lfull: ast.Type, rfull: ?ast.Type };
pub fn unify(self: *Self, t1: TyRef, t2: TyRef, locs: Locs) !void {
    try self.unify_(t1, t2, locs, &.{ .lfull = t1, .rfull = t2 });
}

fn unify_(self: *Self, t1: TyRef, t2: TyRef, locs: Locs, fullTys: Full) error{OutOfMemory}!void {
    const tt1 = self.getType(t1);

    // handle tyvars, cuz it's easier.
    // TODO: occurs check
    switch (tt1) {
        .TyVar => |tyv| {
            if (self.getFieldsForTVar(tyv)) |fields| {
                for (fields) |f| {
                    const t2f = try self.field(t2, f.field, locs);
                    try self.unify_(t2f, f.t, locs, fullTys);
                }
            }
            self.setType(t1, t2);
            return;
        },
        else => {
            const tt2 = self.getType(t2);
            switch (tt2) {
                .TyVar => |tyv| {
                    // COPYPASTA.
                    if (self.getFieldsForTVar(tyv)) |fields| {
                        for (fields) |f| {
                            const t1f = try self.field(t1, f.field, locs);
                            try self.unify_(t1f, f.t, locs, fullTys);
                        }
                    }
                    self.setType(t2, t1);
                    return;
                },
                else => {},
            }
        },
    }

    const tt2 = self.getType(t2);
    switch (tt1) {
        .TyVar => unreachable,
        .Anon => |fields1| {
            switch (tt2) {
                .Anon => |fields2| {
                    try self.matchFields(.{ .t = t1, .fields = fields1 }, .{ .t = t2, .fields = fields2 }, locs, fullTys);
                    self.setType(t1, t2);
                },
                .Con => |rcon| {
                    switch (rcon.type.stuff) {
                        .recs => |recs| {
                            try self.matchFields(.{ .t = t1, .fields = fields1 }, .{ .t = t2, .fields = recs }, locs, fullTys);
                            self.setType(t1, t2);
                        },
                        .cons => unreachable, // error! (make it more specialized, explain that this constructor does not have fields)
                    }
                },
                else => try self.errMismatch(t1, t2, locs, fullTys),
            }
        },
        .Con => |lcon| {
            switch (tt2) {
                .Con => |rcon| {
                    if (!lcon.type.eq(rcon.type)) {
                        try self.errMismatch(t1, t2, locs, fullTys);
                        return;
                    }

                    try self.unifyMatch(lcon.application, rcon.application, locs, fullTys);
                },

                .Anon => |fields2| {
                    switch (lcon.type.stuff) {
                        .recs => |recs| {
                            try self.matchFields(.{ .t = t1, .fields = recs }, .{ .t = t2, .fields = fields2 }, locs, fullTys);
                            self.setType(t2, t1);
                        },
                        .cons => unreachable, // explain
                    }
                },
                else => try self.errMismatch(t1, t2, locs, fullTys),
            }
        },
        .Fun => |lfun| {
            switch (tt2) {
                .Fun => |rfun| {
                    try self.unifyEnv(lfun.env, rfun.env, locs, fullTys);
                    try self.unify_(lfun.ret, rfun.ret, locs, fullTys);
                    try self.unifyParams(lfun.args, rfun.args, locs, fullTys);
                },
                else => try self.errMismatch(t1, t2, locs, fullTys),
            }
        },
        .TVar => |ltv| {
            switch (tt2) {
                .TVar => |rtv| {
                    if (!ltv.eq(rtv)) {
                        return try self.errMismatch(t1, t2, locs, fullTys);
                    }
                },

                else => try self.errMismatch(t1, t2, locs, fullTys),
            }
        }, // TODO
    }
}

// BUG: because we later unify the type, we report an error on a type that later gets unified. make it better.
// BUG: make the error better, make one type as "the one that overwrites" and report "extraenous field".
// NOTE: maybe make a union type (if dealing with two anons)
fn matchFields(self: *Self, rec1: struct { t: ast.Type, fields: []ast.Record }, rec2: struct { t: ast.Type, fields: []ast.Record }, locs: Locs, full: Full) !void {
    // TODO: unstupidify (look next)
    for (rec2.fields) |f2| {
        // assume deduplicated.
        for (rec1.fields) |f1| {
            if (common.streq(f2.field, f1.field)) {
                try self.unify_(f1.t, f2.t, locs, full);
                break;
            }
        } else {
            try self.typeDoesNotHaveField(rec1.t, f2.field, locs, full.lfull);
        }
    }

    // BRUH
    for (rec1.fields) |f1| {
        // assume deduplicated.
        for (rec2.fields) |f2| {
            if (common.streq(f2.field, f1.field)) {
                try self.unify_(f1.t, f2.t, locs, full);
                break;
            }
        } else {
            try self.typeDoesNotHaveField(rec2.t, f1.field, locs, full.rfull.?); // BUG TODO WHATEVER I DON'T KNOW IF THIS'LL HAPPEN
        }
    }
}

pub fn getFieldsForTVar(self: *const Self, tyv: ast.TyVar) ?[]ast.Record {
    return if (self.tyvarFields.get(tyv)) |fields|
        fields.items
    else
        null;
}

pub fn field(self: *Self, t: ast.Type, mem: Str, locs: Locs) !ast.Type {
    switch (self.getType(t)) {
        .Anon => |recs| {
            for (recs) |rec| {
                if (common.streq(rec.field, mem)) {
                    return rec.t;
                }
            } else {
                try self.typeDoesNotHaveField(t, mem, locs, t);
                return try self.fresh();
            }
        },
        .TVar => |tv| {
            for (tv.fields) |f| {
                if (common.streq(f.field, mem)) {
                    return f.t;
                }
            } else {
                try self.typeDoesNotHaveField(t, mem, locs, t);
                return try self.fresh();
            }
        },
        .TyVar => |tyv| {
            const gpr = try self.tyvarFields.getOrPut(tyv);
            if (!gpr.found_existing) {
                gpr.value_ptr.* = std.ArrayList(ast.Record).init(self.tyvarFields.allocator);
            }

            // try put result
            // if it exists, UNIFY!
            const fields = gpr.value_ptr;
            for (fields.items) |rec| {
                if (common.streq(rec.field, mem)) {
                    return rec.t;
                }
            } else {
                const ft = try self.fresh();
                try fields.append(.{ .field = mem, .t = ft });
                return ft;
            }
        },

        .Con => |con| {
            const data = con.type;
            switch (data.stuff) {
                .cons => {
                    try self.reportError(locs, .{ .TypeIsNotARecord = .{ .t = t, .field = mem } });
                    return try self.fresh();
                },
                .recs => |recs| {
                    for (recs) |rec| {
                        if (common.streq(rec.field, mem)) {
                            return try self.mapType(con.application, rec.t);
                        }
                    } else {
                        // TODO: this one has funny locations. associations should carry more location info, bruh.
                        try self.typeDoesNotHaveField(t, mem, locs, t);
                        return try self.fresh();
                    }
                },
            }
        },

        .Fun => unreachable, // error
    }
}

fn unifyEnv(self: *Self, lenvref: EnvRef, renvref: EnvRef, locs: Locs, full: Full) !void {
    _ = full; // use later
    // SLOW AND BAD! Structurally check if environments are the same (but it's BAD, because we are not deduplicating them!!)
    // Later (after we implement classes) we will probably have an id associated with it.
    // Then I can decide if I want structural equality.
    if (lenvref.id == renvref.id) return;
    const lenv = self.getEnv(lenvref).env orelse {
        // self.envContext.items[lenvref.id] = self.envContext.items[renvref.id];
        self.setEnvRef(lenvref, renvref);
        return;
    };
    const renv = self.getEnv(renvref).env orelse {
        // self.envContext.items[renvref.id] = self.envContext.items[lenvref.id];
        self.setEnvRef(renvref, lenvref);
        return;
    };

    if (lenv.len != renv.len) {
        try self.envMismatch(lenv, renv, locs);
        return;
    }

    for (lenv, renv) |lv, rv| {
        if (!std.meta.eql(lv, rv)) {
            try self.envMismatch(lenv, renv, locs);
            return;
        }
    }

    // both are equal here!
    // self.setEnvRef(lenvref, renvref); // hope it's correct! not necessary, but should speed up comparisions?
    // // TODO: we should also do something like this with types, because this might decrease unification times in the future.
}

pub fn getEnv(self: *const Self, envref: EnvRef) struct {
    env: ?ast.Env,
    base: EnvRef,
} {
    var curref = envref;
    while (true) {
        switch (self.envContext.items[curref.id]) {
            .Env => |env| return .{
                .base = curref,
                .env = env,
            },
            .Ref => |ref| curref = ref,
        }
    }
}

fn setEnvRef(self: *Self, src: EnvRef, dest: EnvRef) void {
    var curref = src;
    while (true) {
        const next = self.envContext.items[curref.id];
        self.envContext.items[curref.id] = .{ .Ref = dest };
        switch (next) {
            .Ref => |ref| curref = ref,
            .Env => return, // already mutated, so we just return
        }
    }
}

pub fn unifyMatch(self: *Self, lm: *const ast.Match(ast.Type), rm: *const ast.Match(ast.Type), locs: Locs, full: Full) !void {
    try self.unifyParams(lm.tvars, rm.tvars, locs, full);
}

pub fn unifyParams(self: *Self, lps: []TyRef, rps: []TyRef, locs: Locs, full: Full) !void {
    if (lps.len != rps.len) {
        try self.paramLenMismatch(lps.len, rps.len, locs, full);
        return;
    }

    for (lps, rps) |lp, rp| {
        try self.unify_(lp, rp, locs, full);
    }
}

fn errMismatch(self: *Self, lt: TyRef, rt: TyRef, locs: Locs, full: Full) !void {
    std.debug.assert(locs != null);
    try self.reportError(locs, .{ .MismatchingTypes = .{
        .lt = lt,
        .lpos = locs.?.l,
        .lfull = full.lfull,
        .rt = rt,
        .rpos = locs.?.r,
        .rfull = full.rfull,
    } });
}

fn envMismatch(self: *Self, lenv: ast.Env, renv: ast.Env, locs: Locs) !void {
    std.debug.assert(locs != null);
    try self.reportError(locs, .{ .MismatchingEnv = .{
        .le = lenv,
        .re = renv,
        .lpos = locs.?.l,
        .rpos = locs.?.r,
    } });
}

fn paramLenMismatch(self: *Self, lpl: usize, rpl: usize, locs: Locs, full: Full) !void {
    std.debug.assert(locs != null);
    _ = full;
    try self.reportError(locs, .{ .MismatchingParamLen = .{
        .lpl = lpl,
        .rpl = rpl,
        .lloc = locs.?.l,
        .rloc = locs.?.r,
    } });
}

fn typeDoesNotHaveField(self: *Self, t: ast.Type, f: Str, locs: Locs, full: ast.Type) !void {
    _ = full;
    try self.reportError(locs, .{ .TypeDoesNotHaveField = .{
        .t = t,
        .field = f,
        .loc = locs.?.l,
    } });
}

// TODO: ignoring allocation failures to keep method signature. figure out a way to do a performant occurs check
fn setType(self: *Self, tref: TyRef, tdest: TyRef) void {
    // occurs check
    // var ftvAl = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    // defer ftvAl.deinit();
    // var curftvs = FTVs.init(ftvAl.allocator());
    // self.ftvs(&curftvs, tdest) catch unreachable; // ALLOCATION FAILURE BRUH. (i dont care now, current occurs check is kinda crap and slow)
    // const tyv = self.getType(tref).TyVar;
    // if (curftvs.contains(tref, tyv)) {
    //     // self.reportError(.{ .RecursiveType = .{
    //     //     .tyv = tyv,
    //     //     .in = tdest,
    //     // } }) catch unreachable;
    //     // return;
    //     var hadNewline: bool = undefined;
    //     const ctx = ast.Ctx.init(&hadNewline, self);
    //     ctx.print(.{ "SHIT: ", tref, " | ", tdest, "\n" });

    //     unreachable;
    // }

    var current = tref;
    while (true) {
        if (current.eq(tdest)) { // check for cycles. checking here to shorten.
            break;
        }
        const next = self.context.items[current.id];
        self.context.items[current.id] = .{ .Ref = tdest };
        switch (next) {
            .Ref => |newTy| current = newTy,
            .Type => return,
        }
    }
}

pub fn getType(self: *const Self, t: TyRef) ast.TypeF(TyRef) {
    var current = t;
    while (true) {
        switch (self.context.items[current.id]) {
            .Ref => |newTy| current = newTy,
            .Type => |actualType| return actualType,
        }
    }
}

pub fn ftvs(self: *Self, store: *FTVs, tref: ast.Type) !void {
    const t = self.getType(tref);
    switch (t) {
        .Anon => |fields| {
            for (fields) |field_| {
                try self.ftvs(store, field_.t);
            }
        },
        .TyVar => |tyv| {
            try store.tyvars.insert(.{ .tyv = tyv, .t = tref });
            if (self.getFieldsForTVar(tyv)) |fields| {
                for (fields) |field_| {
                    try self.ftvs(store, field_.t);
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

            const env = self.getEnv(fun.env);
            if (env.env == null) {
                try store.envs.insert(env.base);
            }

            try self.ftvs(store, fun.ret);
        },
        .TVar => {},
    }
}

pub const FTVs = struct {
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
    const Envs = Set(ast.EnvRef, struct {
        pub fn eql(ctx: @This(), a: ast.EnvRef, b: ast.EnvRef) bool {
            _ = ctx;
            return a.id == b.id;
        }

        pub fn hash(ctx: @This(), k: ast.EnvRef) u64 {
            _ = ctx;
            // return @truncate(k.tyv);
            return k.id;
        }
    });
    tyvars: TyVars,

    envs: Envs,

    pub fn init(al: std.mem.Allocator) @This() {
        return .{
            .tyvars = TyVars.init(al),
            .envs = Envs.init(al),
        };
    }

    fn contains(self: *const @This(), t: ast.Type, tyv: ast.TyVar) bool {
        return self.tyvars.contains(.{ .tyv = tyv, .t = t });
    }

    pub fn difference(self: *@This(), diff: *const @This()) void {
        self.tyvars.difference(&diff.tyvars);
        self.envs.difference(&diff.envs);
    }

    pub fn deinit(self: *@This()) void {
        self.tyvars.deinit();
        self.envs.deinit();
    }
};

pub const FTV = struct { tyv: ast.TyVar, t: ast.Type };

pub fn mapType(self: *Self, match: *const ast.Match(ast.Type), ty: ast.Type) error{OutOfMemory}!ast.Type {
    const t = self.getType(ty);
    return switch (t) {
        .Con => |con| b: {
            const conMatch = try self.mapMatch(match, con.application) orelse {
                break :b ty;
            };

            break :b try self.newType(.{ .Con = .{
                .type = con.type,
                .application = conMatch,
            } });
        },
        .Fun => |fun| b: {
            var changed = false;

            var args = std.ArrayList(ast.Type).init(self.arena);
            for (fun.args) |oldTy| {
                const newTy = try self.mapType(match, oldTy);
                changed = changed or !newTy.eq(oldTy);
                try args.append(newTy);
            }

            const ret = try self.mapType(match, fun.ret);
            changed = changed or !ret.eq(fun.ret);

            // this obv. won't be necessary with Match
            //   (I meant the recursively applying env part!)
            const env = try self.mapEnv(match, fun.env);
            changed = changed or env.id != fun.env.id;

            if (!changed) {
                args.deinit();
                break :b ty;
            }

            break :b try self.newType(.{ .Fun = .{
                .args = args.items,
                .ret = ret,
                .env = env,
            } });
        },
        .Anon => |recs| b: {
            const mapped = try self.arena.alloc(ast.TypeF(ast.Type).Field, recs.len);
            var changed = false;
            for (recs, 0..) |rec, i| {
                mapped[i] = .{
                    .t = try self.mapType(match, rec.t),
                    .field = rec.field,
                };

                if (!rec.t.eq(mapped[i].t)) {
                    changed = true;
                }
            }

            if (changed) {
                break :b try self.newType(.{ .Anon = mapped });
            } else {
                break :b ty;
            }
        },
        .TVar => |tv| match.mapTVar(tv) orelse ty,
        .TyVar => ty,
    };
}

// null when match did not change (so we can keep the same data structure)
fn mapMatch(self: *Self, match: *const ast.Match(ast.Type), mm: *const ast.Match(ast.Type)) !?*ast.Match(ast.Type) {
    var changed = false;

    var tvars = std.ArrayList(ast.Type).init(self.arena);
    for (mm.tvars) |oldTy| {
        const newTy = try self.mapType(match, oldTy);
        changed = changed or !newTy.eq(oldTy);
        try tvars.append(newTy);
    }

    var envs = std.ArrayList(ast.EnvRef).init(self.arena);
    for (mm.envVars) |oldEnv| {
        const nuEnv = try self.mapEnv(match, oldEnv);
        changed = changed or oldEnv.id != nuEnv.id;
        try envs.append(nuEnv);
    }

    if (!changed) {
        tvars.deinit();
        envs.deinit();
        return null;
    }

    return try common.allocOne(self.arena, ast.Match(ast.Type){
        .scheme = mm.scheme,
        .tvars = tvars.items,
        .envVars = envs.items,
        .assocs = match.assocs,
    });
}

fn mapEnv(self: *Self, match: *const ast.Match(ast.Type), envref: ast.EnvRef) !ast.EnvRef {
    const envAndBase = self.getEnv(envref);
    return if (envAndBase.env) |env| bb: {
        var envChanged = false;
        var nuenv = std.ArrayList(ast.VarInst).init(self.arena);
        for (env) |inst| {
            const newTy = try self.mapType(match, inst.t);
            envChanged = envChanged or !newTy.eq(inst.t);
            try nuenv.append(.{ .v = inst.v, .t = newTy, .m = inst.m });
        }

        break :bb if (envChanged) try self.newEnv(nuenv.items) else envref;
    } else bb: {
        if (match.mapEnv(envAndBase.base)) |nue| {
            break :bb nue;
        }

        // i guess we just return the normal one? random choice.
        break :bb envref;

        // try self.newEnv(null); // IMPORTANT: must instantiate new env..
    };
}

fn reportError(self: *const Self, locs: Locs, ierr: Error) !void {
    try self.errors.append(.{ .err = ierr, .module = locs.?.l.module });
}

// copied from parser.zig until I solve how I should report errors (probably an Errors struct that can be passed down to various components.)
fn err(self: *Self, comptime t: type, comptime fmt: []const u8, args: anytype) !t {
    std.debug.print(fmt ++ " at {}\n", args ++ .{self.currentToken});
    std.debug.print("{s}\n", .{self.lexer.source[self.currentToken.from -% 5 .. @min(self.lexer.source.len, self.currentToken.to +% 5)]});
    return error.ParseError;
}
