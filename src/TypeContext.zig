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
const TypeMap = @import("TypeMap.zig").TypeMap;
const Prelude = @import("Prelude.zig");

// Personal Note: bruh, I mean, an allocator for this would basically be the same.
// I guess this is more local.
const TyRef = ast.TyRef;
pub const TyStoreElem = union(enum) {
    Ref: TyRef,
    Type: ast.TypeF(TyRef), // not necessarily good, because it takes a lot of space. Ideally, it would be a second index to an array of actual immutable AST types.
};
const TyStore = std.ArrayList(if (ast.TyRefPointer) *TyStoreElem else TyStoreElem);

const EnvRef = ast.EnvRef;
pub const Env = struct {
    env: *ast.Env,
    match: *const ast.Match,
    level: usize,
    fun: ?*ast.Function, // probably not needed

    pub fn empty(id: Unique, al: std.mem.Allocator) !@This() {
        return .{
            .env = try common.allocOne(al, ast.Env.empty(id)),
            .match = try common.allocOne(al, ast.Match.empty(ast.Scheme.empty())),
            .fun = null,
            .level = 0,
        };
    }

    pub fn toEnvFun(self: *const @This()) ast.EnvFun {
        return .{ .env = self.env, .fun = self.fun };
    }
};
pub const MatchLink = struct {
    match: *const ast.Match,
    next: ?*const MatchLink,

    pub fn init(al: std.mem.Allocator, match: *const ast.Match) !*const @This() {
        return try common.allocOne(al, MatchLink{
            .match = match,
            .next = null,
        });
    }

    pub fn empty(al: std.mem.Allocator) !*const @This() {
        return try MatchLink.init(al, try common.allocOne(al, ast.Match.empty(ast.Scheme.empty())));
    }

    pub fn link(al: std.mem.Allocator, match: *const ast.Match, next: ?*const MatchLink) !*const @This() {
        return try common.allocOne(al, MatchLink{
            .match = match,
            .next = next,
        });
    }

    fn length(self: *const @This()) u32 {
        var l: u32 = 1;
        var nml = self.next;
        while (nml) |ml| {
            l += 1;
            nml = ml.next;
        }

        return l;
    }

    pub fn zip(left: *const MatchLink, right: *const MatchLink) Iterator {
        const ll = left.length();
        const rl = right.length();

        if (ll == rl) {
            return MatchLink.Iterator{ .l = left, .r = right };
        } else {
            unreachable;
        }
    }

    const Iterator = struct {
        l: ?*const MatchLink,
        r: ?*const MatchLink,

        pub fn next(self: *@This()) ?struct { l: *const ast.Match, r: *const ast.Match } {
            if (self.l) |ll| {
                const rr = self.r.?;
                self.l = ll.next;
                self.r = rr.next;

                return .{ .l = ll.match, .r = rr.match };
            } else return null;
        }
    };

    pub fn print(self: @This(), c: ast.Ctx) void {
        c.print(.{self.match});
        if (self.next) |nml| {
            c.print(.{ "|| ", nml });
        }
    }
};
const EnvStore = std.ArrayList(union(enum) {
    Ref: EnvRef,
    Env: ?Env,
});

const NumRef = ast.NumRef;
const NumStore = std.ArrayList(union(enum) {
    Ref: NumRef,
    Num: ast.TypeOrNum.TyNum,
});

// instead of putting it in tyvars, make a separate map (as only a minority of tyvars will have any fields.)
const TyVarFields = std.HashMap(
    ast.TyVar,
    TyVarSpecs,
    ast.TyVar.comparator(),
    std.hash_map.default_max_load_percentage,
);
const TyVarSpecs = struct {
    fields: std.ArrayList(ast.Record),
    areFieldsLockedIn: bool,
};

prelude: ?Prelude = null,
context: TyStore,
envContext: EnvStore,
numContext: NumStore,
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
        .numContext = NumStore.init(al),
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
        },
    });
}

pub fn newAnon(self: *Self, fields: []ast.TypeF(ast.Type).Field) !ast.Type {
    const tyv = ast.TyVar{ .uid = self.gen.newUnique() };
    const t = try self.newType(.{ .TyVar = tyv });
    const gp = try self.tyvarFields.getOrPut(tyv);

    const tyvstuff = gp.value_ptr;
    tyvstuff.* = .{ .areFieldsLockedIn = true, .fields = std.ArrayList(ast.Record).init(self.arena) };
    for (fields) |f| {
        try tyvstuff.fields.append(.{ .field = f.field, .t = f.t });
    }

    return t;
}

pub fn newType(self: *Self, t: ast.TypeF(TyRef)) !ast.Type {
    if (!ast.TyRefPointer) {
        try self.context.append(.{ .Type = t });
        const tid = self.context.items.len - 1;
        return .{ .id = tid };
    } else {
        const tp = try common.allocOne(self.arena, TyStoreElem{ .Type = t });
        try self.context.append(tp);
        return .{ .id = tp };
    }
}

pub fn newEnv(self: *Self, e: ?Env) !ast.EnvRef {
    try self.envContext.append(.{ .Env = e });
    const envid = self.envContext.items.len - 1;
    return .{ .id = envid };
}

pub fn newNum(self: *Self, n: ast.TypeOrNum.TyNum) !ast.NumRef {
    try self.numContext.append(.{ .Num = n });
    const numid = self.numContext.items.len - 1;
    return .{ .id = numid };
}

pub fn getNum(self: *const Self, n: ast.NumRef) ast.TypeOrNum.TyNum {
    var curref = n;
    while (true) {
        switch (self.numContext.items[curref.id]) {
            .Num => |num| return num,
            .Ref => |ref| curref = ref,
        }
    }
}

fn setNum(self: *Self, src: NumRef, dest: NumRef) void {
    var curref = src;
    while (true) {
        const next = self.numContext.items[curref.id];
        self.numContext.items[curref.id] = .{ .Ref = dest };
        switch (next) {
            .Ref => |ref| curref = ref,
            .Num => return, // already mutated, so we just return
        }
    }
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
            if (self.getFieldsForTVar(tyv)) |tyvspecs| {
                for (tyvspecs.fields) |f| {
                    const t2f = try self.field(t2, f.field, locs);
                    try self.unify_(t2f, f.t, locs, fullTys);
                }
                if (!tyvspecs.total) {} else {
                    try self.ensureExactFields(tyvspecs.fields, t1, t2, locs, fullTys);
                }
            }
            try self.setType(t1, t2, locs, false);
            return;
        },
        else => {
            const tt2 = self.getType(t2);
            switch (tt2) {
                .TyVar => |tyv| {
                    // COPYPASTA.
                    if (self.getFieldsForTVar(tyv)) |tvs| {
                        for (tvs.fields) |f| {
                            const t1f = try self.field(t1, f.field, locs);
                            try self.unify_(t1f, f.t, locs, fullTys);
                        }
                        if (!tvs.total) {} else {
                            try self.ensureExactFields(tvs.fields, t2, t1, locs, fullTys);
                        }
                    }
                    try self.setType(t2, t1, locs, true);
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
                    try self.setType(t1, t2, locs, false);
                },
                // .Con => |rcon| {
                //     switch (rcon.type.stuff) {
                //         .recs => |recs| {
                //             try self.matchFields(.{ .t = t1, .fields = fields1 }, .{ .t = t2, .fields = recs }, locs, fullTys);
                //             try self.setType(t1, t2, locs, false);
                //         },
                //         .cons => unreachable, // error! (make it more specialized, explain that this constructor does not have fields)
                //     }
                // },
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
                    try self.unifyParamsWithTNums(lcon.outerApplication, rcon.outerApplication, locs, fullTys);
                },

                // .Anon => |fields2| {
                //     switch (lcon.type.stuff) {
                //         .recs => |recs| {
                //             try self.matchFields(.{ .t = t1, .fields = recs }, .{ .t = t2, .fields = fields2 }, locs, fullTys);
                //             try self.setType(t2, t1, locs, true);
                //         },
                //         .cons => unreachable, // explain
                //     }
                // },
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

fn ensureExactFields(self: *Self, fields: []ast.TypeF(ast.Type).Field, t: ast.Type, ot: ast.Type, locs: Locs, fullTys: Full) !void {
    switch (self.getType(ot)) {
        .Con => |con| {
            switch (con.type.stuff) {
                .recs => |recs| {
                    const mrecs = try common.mapSlice(self.arena, ast.Data.DecRecord, ast.Record, ast.Data.DecRecord.mapRecord, recs);
                    defer self.arena.free(mrecs);
                    // SMELL: PARTIALLY COPIED FROM fn field(). SAME, NON OBIOUS LOGIC. SUSSY.
                    for (mrecs, recs) |*mrec, rec| {
                        const outerMatch = ast.Match.fromOuterTVars(con.type.outerTVars, con.outerApplication);
                        mrec.* = .{ .field = rec.rec.field, .t = try self.mapType(con.application, try self.mapType(&outerMatch, rec.rec.t)) };
                    }
                    try self.matchFields(.{ .t = t, .fields = fields }, .{ .t = ot, .fields = mrecs }, locs, fullTys);
                },
                .cons => unreachable, // error! (make it more specialized, explain that this constructor does not have fields)
            }
        },
        .Anon => unreachable,
        .TyVar => |tyv| {
            const mofields = self.getFieldsForTVar(tyv);
            if (mofields) |ofields| {
                try self.matchFields(.{ .t = t, .fields = fields }, .{ .t = ot, .fields = ofields.fields }, locs, fullTys);
            } else {
                // in case it has no fields, "transfer them"
                for (fields) |f| {
                    const ft = try self.field(ot, f.field, locs);
                    try self.unify_(ft, f.t, locs, fullTys);
                }
            }

            // at the end, MUST SET THE fields total FLAG.
            self.tyvarFields.getPtr(tyv).?.areFieldsLockedIn = true; // possibility of null when we add no fields doe. IN THE FUTURE, IF THE NULLCHECK TRIPS, ADD THE INITIALIZATION TOO.
        },
        .TVar => |tv| {
            // here, we don't care about tv.fieldsTotal, because we are not going to be extending tvars fields bruh. they must match exactly either way.
            try self.matchFields(.{ .t = t, .fields = tv.fields }, .{ .t = ot, .fields = fields }, locs, fullTys);
        },
        else => unreachable,
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

pub fn getFieldsForTVar(self: *const Self, tyv: ast.TyVar) ?struct { fields: []ast.Record, total: bool } {
    return if (self.tyvarFields.get(tyv)) |tvspecs|
        .{ .fields = tvspecs.fields.items, .total = tvspecs.areFieldsLockedIn }
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
                gpr.value_ptr.* = .{
                    .fields = std.ArrayList(ast.Record).init(self.tyvarFields.allocator),
                    .areFieldsLockedIn = false,
                };
            }

            // try put result
            // if it exists, UNIFY!
            const tyvstats = gpr.value_ptr;
            const fields = &tyvstats.fields;
            for (fields.items) |rec| {
                if (common.streq(rec.field, mem)) {
                    return rec.t;
                }
            } else {
                const ft = try self.fresh();
                if (tyvstats.areFieldsLockedIn) {
                    try self.typeDoesNotHaveField(t, mem, locs, t);
                } else {
                    try fields.append(.{ .field = mem, .t = ft });
                }
                return ft;
            }
        },

        .Con => |con| {
            const data = con.type;
            switch (data.stuff) {
                .cons => {
                    try self.reportError(locs, .{ .TypeIsNotARecord = .{ .t = t, .field = mem, .loc = locs.?.l } });
                    return try self.fresh();
                },
                .recs => |recs| {
                    for (recs) |arec| {
                        const rec = arec.rec;
                        if (common.streq(rec.field, mem)) {
                            const outerMatch = ast.Match.fromOuterTVars(data.outerTVars, con.outerApplication);
                            return try self.mapType(con.application, try self.mapType(&outerMatch, rec.t));
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

pub fn unifyEnv(self: *Self, lenvref: EnvRef, renvref: EnvRef, locs: Locs, full: Full) !void {
    _ = full; // use later
    // SLOW AND BAD! Structurally check if environments are the same (but it's BAD, because we are not deduplicating them!!)
    // Later (after we implement classes) we will probably have an id associated with it.
    // Then I can decide if I want structural equality.
    if (lenvref.id == renvref.id) return;
    const llenv = self.getEnv(lenvref).env.* orelse {
        // self.envContext.items[lenvref.id] = self.envContext.items[renvref.id];
        self.setEnvRef(lenvref, renvref);
        return;
    };
    const lenv = llenv.env;
    const rrenv = self.getEnv(renvref).env.* orelse {
        // self.envContext.items[renvref.id] = self.envContext.items[lenvref.id];
        self.setEnvRef(renvref, lenvref);
        return;
    };
    const renv = rrenv.env;

    if (lenv.insts.items.len != renv.insts.items.len) {
        try self.envMismatch(lenv, renv, locs);
        return;
    }

    for (lenv.insts.items, renv.insts.items) |lv, rv| {
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
    env: *?Env,
    base: EnvRef,
} {
    var curref = envref;
    while (true) {
        switch (self.envContext.items[curref.id]) {
            .Env => |*env| return .{
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

fn unifyMatch(self: *Self, lm: *const ast.Match, rm: *const ast.Match, locs: Locs, full: Full) !void {
    try self.unifyParamsWithTNums(lm.tvars, rm.tvars, locs, full);

    for (lm.envVars, rm.envVars) |le, re| {
        try self.unifyEnv(le, re, locs, full);
    }

    // when should / does matching associations happen?
    // ONLY when matching datatypes (Con)
    //    what does that mean?
    // it means that unifyMatch happens only in DTs like this:
    // StrBox
    //   StrBox Str  <- Str is a class. this, we ALLOW.
    // 1. the Str (== `from` type in the association) type must be the same.
    //    It already lands in scheme's tvars, so it's checked before.
    // 2. check that the selected instance is the same / unify assocs.
    //    NAH. (5_t50)
    //    We don't actually want that. In the end, we want them to be resolved at function call and not during construction.
    //    The assosation that is created is also weak - we only care that the instance is implemented at all.
    //    Since we already checked, that the types are the same, then both either implement it or don't so that's enough.
}

pub fn unifyParamsWithTNums(self: *Self, lps: []ast.TypeOrNum, rps: []ast.TypeOrNum, locs: Locs, full: Full) !void {
    if (lps.len != rps.len) {
        try self.paramLenMismatch(lps.len, rps.len, locs, full);
        return;
    }

    for (lps, rps) |lp, rp| {
        switch (lp) {
            .Type => |lpt| try self.unify_(lpt, rp.Type, locs, full),
            .Num => |lpn| try self.unifyNum(lpn, rp.Num, locs, full),
        }
    }
}

pub fn unifyNum(self: *Self, l: NumRef, r: NumRef, locs: Locs, full: Full) !void {
    if (l.id == r.id) return;

    const ln = self.getNum(l);
    const rn = self.getNum(r);
    switch (ln) {
        .Unknown => {
            self.setNum(l, r);
            return;
        },
        .Literal => |ll| switch (rn) {
            .Unknown => {
                self.setNum(r, l);
                return;
            },
            .Literal => |rl| {
                if (ll != rl) {
                    try self.reportError(locs, .{ .MismatchingTNum = .{
                        .l = ll,
                        .lloc = locs.?.l,
                        .r = rl,
                        .rloc = locs.?.r,
                    } });
                    // unreachable;
                }
            },
            .TNum => {
                try self.reportError(locs, .{ .MismatchingNumTypes = .{
                    .l = ln,
                    .lfull = full.lfull,
                    .lpos = locs.?.l,
                    .r = rn,
                    .rfull = full.rfull,
                    .rpos = locs.?.r,
                } });
            },
        },
        .TNum => |lt| {
            switch (rn) {
                .Unknown => {
                    self.setNum(r, l);
                    return;
                },
                .TNum => |rt| {
                    if (lt.uid != rt.uid) {
                        unreachable; // TODO: error
                    }
                },
                .Literal => {
                    unreachable; // TODO: error
                },
            }
        },
    }
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

fn errOccursCheck(self: *Self, t: TyRef, tyvar: ast.TyVar, tl: ?Loc, tyvarl: ?Loc, moduleInfo: common.ModuleInfo) !void {
    try self.errors.append(.{
        .module = moduleInfo,
        .err = .{
            .OccursCheck = .{
                .t = t,
                .tpos = tl,
                .tyv = tyvar,
                .tyvpos = tyvarl,
            },
        },
    });
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

fn envMismatch(self: *Self, lenv: *ast.Env, renv: *ast.Env, locs: Locs) !void {
    std.debug.assert(locs != null);
    // NOTE: sussy, because an env can change later and POSSIBLY match.
    try self.reportError(locs, .{ .MismatchingEnv = .{
        .le = lenv,
        .re = renv,
        .lpos = locs.?.l,
        .rpos = locs.?.r,
    } });
    // unreachable;
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

fn setType(self: *Self, tref: TyRef, tdest: TyRef, locs: Locs, reversed: bool) !void {
    // TODO: *definitely* look through this and fix this code again because it's weird.
    //   1. We `getType` again which is following a pointer around.
    //   2. funny code. figure out when exactly we SHOULDN'T DO an occurs check. (for some reason, the tyvar thing breaks)
    switch (self.getType(tref)) {
        .TyVar => |tyvar| {
            const desttyvar = self.getType(tdest);
            b: {
                switch (desttyvar) {
                    .TyVar => |dtyv| {
                        if (dtyv.uid == tyvar.uid) {
                            return;
                        } else {
                            break :b;
                        }
                    },
                    else => {},
                }

                if (self.occursCheck(tyvar, tdest)) {
                    if (!reversed) {
                        try self.errOccursCheck(
                            tdest,
                            tyvar,
                            locs.?.l,
                            locs.?.r,
                            locs.?.l.module,
                        );
                    } else {
                        try self.errOccursCheck(
                            tdest,
                            tyvar,
                            locs.?.r,
                            locs.?.l,
                            locs.?.l.module,
                        );
                    }
                    return;
                }

                break :b;
            }
        },

        // NOTE: we are doing setType with Anons.
        // Then we don't need an occurs check... probably?????
        .Anon => {},
        else => unreachable,
    }
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
        const next = if (ast.TyRefPointer) current.id.* else self.context.items[current.id];
        if (ast.TyRefPointer) {
            current.id.* = .{ .Ref = tdest };
        } else {
            self.context.items[current.id] = .{ .Ref = tdest };
        }

        switch (next) {
            .Ref => |newTy| current = newTy,
            .Type => return,
        }
    }
}

fn occursCheck(self: *const Self, tyv: ast.TyVar, ty: TyRef) bool {
    const t = self.getType(ty);
    switch (t) {
        .TyVar => |ttyv| {
            return tyv.uid == ttyv.uid;
        },
        .Con => |con| {
            return self.occursCheckMatch(tyv, con.application);
        },
        .Fun => |fun| {
            if (self.getEnv(fun.env).env.*) |env| {
                if (self.occursCheckMatch(tyv, env.match)) return true;
            }

            for (fun.args) |aty| {
                if (self.occursCheck(tyv, aty)) return true;
            }

            if (self.occursCheck(tyv, fun.ret)) return true;
            return false;
        },
        .TVar => return false,
        .Anon => |anon| {
            for (anon) |an| {
                if (self.occursCheck(tyv, an.t)) return true;
            }
            return false;
        },
    }
}

fn occursCheckMatchLink(self: *const Self, tyv: ast.TyVar, match: *const MatchLink) bool {
    var m: ?*const MatchLink = match;
    while (m) |ml| {
        if (self.occursCheckMatch(tyv, ml.match)) return true;
        m = ml.next;
    }

    return false;
}

fn occursCheckMatch(self: *const Self, tyv: ast.TyVar, match: *const ast.Match) bool {
    for (match.tvars) |tynum| {
        switch (tynum) {
            .Type => |ty| if (self.occursCheck(tyv, ty)) return true,
            .Num => continue,
        }
    }

    return false;
}

pub fn getType(self: *const Self, t: TyRef) ast.TypeF(TyRef) {
    return self.getTypeAndBase(t).t;
}

fn getTypeAndBase(self: *const Self, t: TyRef) struct { base: ast.Type, t: ast.TypeF(TyRef) } {
    var current = t;
    while (true) {
        const next = if (ast.TyRefPointer) current.id.* else self.context.items[current.id];
        switch (next) {
            .Ref => |newTy| current = newTy,
            .Type => |actualType| return .{ .base = current, .t = actualType },
        }
    }
}

pub fn ftvs(self: *Self, store: *FTVs, tref: ast.Type) error{OutOfMemory}!void {
    const t = self.getType(tref);
    switch (t) {
        .Anon => |fields| {
            for (fields) |field_| {
                try self.ftvs(store, field_.t);
            }
        },
        .TyVar => |tyv| {
            try store.tyvars.insert(.{ .tyv = tyv, .t = tref });
            if (self.getFieldsForTVar(tyv)) |tvs| {
                for (tvs.fields) |field_| {
                    try self.ftvs(store, field_.t);
                }
            }
        },
        .Con => |con| {
            try self.ftvsFromMatch(store, con.application);
            // TODO: outer tvars
        },
        .Fun => |fun| {
            for (fun.args) |arg| {
                try self.ftvs(store, arg);
            }

            const env = self.getEnv(fun.env);
            if (env.env.*) |ee| {
                try self.ftvsFromMatch(store, ee.match);
            }
            // if (env.env == null) { // Q: @grok is this correct? A: if we add unions, we should remove this check.
            // NOTE: check removed. what I'm doing is going to ADD a match of the instantiating function to provide context to the function.
            try store.envs.insert(env.base);
            // }

            try self.ftvs(store, fun.ret);
        },
        .TVar => {},
    }
}

pub fn ftvsFromMatch(self: *Self, store: *FTVs, m: *const ast.Match) !void {
    for (m.tvars) |tvn| {
        switch (tvn) {
            .Type => |t| {
                try self.ftvs(store, t);
            },
            .Num => |tnum| {
                switch (self.getNum(tnum)) {
                    .Unknown => {
                        try store.nums.insert(tnum);
                    },
                    else => {},
                }
            },
        }
    }

    for (m.envVars) |ev| {
        const env = self.getEnv(ev);
        if (env.env.*) |ee| {
            try self.ftvsFromMatch(store, ee.match);
        }
        try store.envs.insert(env.base);
    }

    for (m.assocs) |massoc| {
        if (massoc.*) |assoc| {
            switch (assoc) {
                .Id => {},
                .InstFun => |ifun| {
                    try self.ftvsFromMatch(store, ifun.m);
                },
            }
        }
    }
}

pub fn ftvsFromEnv(self: *Self, envftvs: *FTVs, env: *ast.Env) !void {
    for (env.insts.items) |inst| {
        // TODO: this is incorrect. For functions, I must extract ftvs from UNINSTANTIATED types.
        // NOTE: but that's what I'm doing now??
        switch (inst.v) {
            .TNum => {},
            .Var => try self.ftvs(envftvs, inst.t),
            .Fun => |fun| {
                for (fun.params) |p| {
                    try self.ftvs(envftvs, p.d.t);
                }

                try self.ftvs(envftvs, fun.ret);
            },

            .ClassFun => |vv| {
                const cfun = vv.cfun;
                for (cfun.params) |p| {
                    try self.ftvs(envftvs, p.t);
                }

                try self.ftvs(envftvs, cfun.ret);
            },
        }
    }
}

// !! Only used in datatypes. !!
pub const TVarStore = Set(ast.TVarOrNum, ast.TVarOrNum.comparator());
pub fn getOuterTVars(self: *Self, binding: ?ast.Binding, store: *TVarStore, tref: ast.Type) !void {
    const t = self.getType(tref);
    switch (t) {
        .Anon => |fields| {
            for (fields) |field_| {
                try self.getOuterTVars(binding, store, field_.t);
            }
        },
        .TyVar => |tyv| {
            if (self.getFieldsForTVar(tyv)) |tyvs| {
                for (tyvs.fields) |field_| {
                    try self.getOuterTVars(binding, store, field_.t);
                }
            }
        },
        .Con => |con| {
            for (con.application.tvars) |mtOrNum| {
                switch (mtOrNum) {
                    .Type => |mt| try self.getOuterTVars(binding, store, mt),
                    .Num => |tnumref| {
                        switch (self.getNum(tnumref)) {
                            .TNum => |tnum| {
                                // only add nums NOT from the datatype
                                if (!std.meta.eql(tnum.binding, binding.?)) {
                                    try store.insert(.{ .TNum = tnum });
                                }
                            },
                            else => {},
                        }
                    },
                }
            }

            // should we also go through the outer datatypes for the con?
            // TODO: devise a test case for this and then implement it
            //  should also ftvs() check this place??
        },
        .Fun => |fun| {
            for (fun.args) |arg| {
                try self.getOuterTVars(binding, store, arg);
            }

            try self.getOuterTVars(binding, store, fun.ret);
        },
        .TVar => |tv| {
            // only add tvars NOT from the datatype.
            if (!std.meta.eql(tv.binding, binding)) {
                try store.insert(.{ .TVar = tv });
            }
        },
    }
}

// TODO(26.03.26): maybe we should do it the same way as envs?
pub const AllStore = struct {
    al: std.mem.Allocator,
    tvars: Set(ast.TVarOrNum, ast.TVarOrNum.comparator()),
    envs: Set(ast.EnvRef, ast.EnvRef.Comparator),
    assocs: Set(ast.Association.ID, std.hash_map.AutoContext(ast.Association.ID)),

    pub fn init(al: std.mem.Allocator, tc: *const Self) @This() {
        _ = tc;
        return .{
            .al = al,
            .tvars = Set(ast.TVarOrNum, ast.TVarOrNum.comparator()).init(al),
            .envs = Set(ast.EnvRef, ast.EnvRef.Comparator).init(al),
            .assocs = Set(ast.Association.ID, std.hash_map.AutoContext(ast.Association.ID)).init(al),
        };
    }

    pub fn toScheme(self: *const @This(), envs: *const FTVs.Envs, assocs: []ast.Association) !ast.Scheme {
        var stvars = std.ArrayList(ast.TVarOrNum).init(self.al);
        var stvarIt = self.tvars.iterator();
        while (stvarIt.next()) |tv| {
            try stvars.append(tv.*);
        }

        var stenvs = std.ArrayList(ast.EnvRef).init(self.al);
        var stenvIt = self.envs.iterator();
        while (stenvIt.next()) |env| {
            if (envs.contains(env.*)) {
                try stenvs.append(env.*);
            }
        }

        var stassocs = std.ArrayList(ast.Association).init(self.al);
        for (assocs) |ass| {
            if (self.assocs.contains(ass.uid)) {
                try stassocs.append(ass);
            }
        }

        return ast.Scheme{
            .tvars = stvars.items,
            .envVars = stenvs.items,
            .associations = stassocs.items,
        };
    }

    pub fn deinit(self: *@This()) void {
        self.tvars.deinit();
        self.envs.deinit();
    }
};
pub fn getTVarsFromEnv(self: *Self, binding: ?ast.Binding, store: *AllStore, env: *ast.Env, minLevel: ast.Level) !void {
    for (env.insts.items) |inst| {
        // TODO: this is incorrect. For functions, I must extract ftvs from UNINSTANTIATED types.
        // NOTE: but that's what I'm doing now??
        // NOTE(18.04.26): what? why whould I do that. Im gonna change it to instantiated types, since it seems that's what's needed.
        try self.getTVars(binding, store, inst.t);
        switch (inst.v) {
            .TNum => {},
            .Var => {}, //try self.getTVars(binding, store, inst.t),
            .Fun => |fun| {
                if (fun.env.level > minLevel) {
                    try self.getTVars(binding, store, fun.ret);
                    for (fun.params) |param| {
                        try self.getTVars(binding, store, param.d.t);
                    }

                    try self.getTVarsFromEnv(binding, store, fun.env, minLevel);
                }
                // try self.getTVarsFromMatch(binding, store, inst.m);
                // for (fun.params) |p| {
                //     try self.getTVars(binding, store, p.d.t);
                // }

                // try self.getTVars(binding, store, fun.ret);
            },

            .ClassFun => |vv| {
                if (vv.ref.*) |ref| {
                    switch (ref) {
                        .Id => |id| {
                            try store.assocs.insert(id);
                        },
                        .InstFun => unreachable,
                    }
                }

                // NOTE: should I do it??
                // try self.getTVars(binding, store, inst.t);
                // const cfun = vv.cfun;
                // for (cfun.params) |p| {
                //     try self.getTVars(binding, store, p.t);
                // }

                // try self.getTVars(binding, store, cfun.ret);
            },
        }
    }
}

pub fn getTVarsFromTypeOrNum(self: *Self, binding: ?ast.Binding, store: *AllStore, mtOrNum: ast.TypeOrNum) error{OutOfMemory}!void {
    switch (mtOrNum) {
        .Type => |mt| try self.getTVars(binding, store, mt),
        .Num => |tnumref| {
            switch (self.getNum(tnumref)) {
                .TNum => |tnum| {
                    // only add nums NOT from the datatype
                    if (binding == null or std.meta.eql(tnum.binding, binding.?)) {
                        try store.tvars.insert(.{ .TNum = tnum });
                    }
                },
                else => {},
            }
        },
    }
}

pub fn getTVarsFromMatch(self: *Self, binding: ?ast.Binding, store: *AllStore, m: *const ast.Match) !void {
    for (m.tvars) |tv| {
        try self.getTVarsFromTypeOrNum(binding, store, tv);
    }

    for (m.envVars) |envref| {
        const env = self.getEnv(envref);
        if (env.env.*) |e| {
            try self.getTVarsFromMatch(binding, store, e.match);
        }
    }

    for (m.assocs) |massoc| {
        if (massoc.*) |assoc| {
            switch (assoc) {
                .Id => |id| try store.assocs.insert(id),
                .InstFun => |ifun| {
                    try self.getTVarsFromMatch(binding, store, ifun.m);
                },
            }
        }
        // TODO
    }
}

pub fn getTVars(self: *Self, binding: ?ast.Binding, store: *AllStore, tref: ast.Type) !void {
    const t = self.getType(tref);
    switch (t) {
        .Anon => |fields| {
            for (fields) |field_| {
                try self.getTVars(binding, store, field_.t);
            }
        },
        .TyVar => |tyv| {
            if (self.getFieldsForTVar(tyv)) |tyvs| {
                for (tyvs.fields) |field_| {
                    try self.getTVars(binding, store, field_.t);
                }
            }
        },
        .Con => |con| {
            try self.getTVarsFromMatch(binding, store, con.application);
            for (con.outerApplication) |numOrTy| {
                try self.getTVarsFromTypeOrNum(binding, store, numOrTy);
            }
        },
        .Fun => |fun| {
            for (fun.args) |arg| {
                try self.getTVars(binding, store, arg);
            }

            // TODO: should put this to a function?
            const env = self.getEnv(fun.env);
            if (env.env.*) |e| {
                try self.getTVarsFromMatch(binding, store, e.match);
            }
            try store.envs.insert(env.base);

            try self.getTVars(binding, store, fun.ret);
        },
        .TVar => |tv| {
            if (binding == null or std.meta.eql(tv.binding.?, binding.?)) {
                try store.tvars.insert(.{ .TVar = tv });
            }
        },
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
    const Envs = Set(ast.EnvRef, ast.EnvRef.Comparator);
    const Nums = Set(ast.NumRef, struct {
        pub fn eql(ctx: @This(), a: ast.NumRef, b: ast.NumRef) bool {
            _ = ctx;
            return a.id == b.id;
        }

        pub fn hash(ctx: @This(), k: ast.NumRef) u64 {
            _ = ctx;
            return k.id;
        }
    });

    tyvars: TyVars,
    envs: Envs,
    nums: Nums,

    pub fn init(al: std.mem.Allocator) @This() {
        return .{
            .tyvars = TyVars.init(al),
            .envs = Envs.init(al),
            .nums = Nums.init(al),
        };
    }

    pub fn contains(self: *const @This(), t: ast.Type, tyv: ast.TyVar) bool {
        return self.tyvars.contains(.{ .tyv = tyv, .t = t });
    }

    pub fn difference(self: *@This(), diff: *const @This()) void {
        self.tyvars.difference(&diff.tyvars);
        self.envs.difference(&diff.envs);
        self.nums.difference(&diff.nums);
    }

    pub fn deinit(self: *@This()) void {
        self.tyvars.deinit();
        self.envs.deinit();
        self.nums.deinit();
    }

    pub fn mkScheme(self: *const @This()) !ast.Scheme {
        _ = self;
        unreachable;
    }
};

pub const FTV = struct { tyv: ast.TyVar, t: ast.Type };

pub fn mapType(self: *Self, match: anytype, ty: ast.Type) error{OutOfMemory}!ast.Type {
    const bt = self.getTypeAndBase(ty);
    const t = bt.t;

    // for (match.tvars) |tyOrNum| {
    //     switch (tyOrNum) {
    //         .Type => |mty| {
    //             const mt = self.getTypeAndBase(mty);
    //             if (mt.base.eq(bt.base)) return ty;
    //         },
    //         else => continue,
    //     }
    // }

    return switch (t) {
        .Con => |con| b: {
            const mconMatch = try self.mapMatch_(match, con.application);

            var changed = mconMatch != null;
            const outerTys = try self.arena.alloc(ast.TypeOrNum, con.outerApplication.len);
            for (con.outerApplication, 0..) |oldTyOrNum, i| {
                switch (oldTyOrNum) {
                    .Type => |oldTy| {
                        const newTy = try self.mapType(match, oldTy);
                        changed = changed or !newTy.eq(oldTy);
                        outerTys[i] = .{ .Type = newTy };
                    },
                    .Num => |num| {
                        const nuNum = self.mapNum(match, num);
                        changed = changed or nuNum != null;
                        outerTys[i] = .{ .Num = nuNum orelse num };
                    },
                }
            }

            if (changed) {
                break :b try self.newType(.{ .Con = .{
                    .type = con.type,
                    .application = mconMatch orelse con.application,
                    .outerApplication = outerTys,
                } });
            } else {
                break :b ty;
            }
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

pub fn mapMatchLink(self: *Self, match: *const ast.Match, ml: *const MatchLink) !*const MatchLink {
    return (try self.mapMatchLink_(match, ml)) orelse ml;
}

pub fn mapMatchLink_(self: *Self, match: *const ast.Match, ml: *const MatchLink) !?*const MatchLink {
    if (ml.next) |nml| {
        const mnnml = try self.mapMatchLink_(match, nml);
        if (mnnml) |nnml| {
            // here, linked matches were changed, so we must reevaluate the thing anyway.
            const m = try self.mapMatch(match, nml.match);
            return try MatchLink.link(self.arena, m, nnml);
        } else {
            const mm = try self.mapMatch_(match, nml.match);
            if (mm) |m| {
                return try MatchLink.link(self.arena, m, ml.next);
            } else {
                // no changes here tho.
                return null;
            }
        }
    } else {
        return null;
    }
    unreachable;
}

pub fn mapMatch(self: *Self, match: anytype, mm: *const ast.Match) !*const ast.Match {
    return (try self.mapMatch_(match, mm)) orelse mm;
}

// null when match did not change (so we can keep the same data structure)
fn mapMatch_(self: *Self, match: anytype, mm: *const ast.Match) !?*ast.Match {
    var changed = false;

    var tvars = std.ArrayList(ast.TypeOrNum).init(self.arena);
    for (mm.tvars) |oldTyOrNum| {
        switch (oldTyOrNum) {
            .Type => |oldTy| {
                const newTy = try self.mapType(match, oldTy);
                changed = changed or !newTy.eq(oldTy);
                try tvars.append(.{ .Type = newTy });
            },
            .Num => |num| {
                const nuNum = self.mapNum(match, num);
                changed = changed or nuNum != null;
                try tvars.append(.{ .Num = nuNum orelse num });
            },
        }
    }

    var envs = std.ArrayList(ast.EnvRef).init(self.arena);
    for (mm.envVars) |oldEnv| {
        const nuEnv = try self.mapEnv(match, oldEnv);
        changed = changed or oldEnv.id != nuEnv.id;
        try envs.append(nuEnv);
    }

    var assocs = std.ArrayList(*?ast.Match.AssocRef).init(self.arena);
    for (mm.assocs) |moldAssoc| {
        if (moldAssoc.*) |oldAssoc| {
            switch (oldAssoc) {
                .Id => |id| {
                    // if (@TypeOf(match) == *const ast.Match) {
                    //     var hadNewline = false;
                    //     var c = ast.Ctx.init(&hadNewline, self);
                    //     c.print(.{ id, " :: ", match, "\n" });
                    // }
                    if (match.tryGetFunctionOrIDByID(id)) |ref| {
                        try assocs.append(ref);
                        changed = true;
                        continue;
                    } else {
                        try assocs.append(moldAssoc);
                        continue;
                    }
                },
                .InstFun => |ifun| {
                    if (try self.mapMatch_(match, ifun.m)) |nuMatch| {
                        const ar = try self.arena.create(?ast.Match.AssocRef);
                        ar.* = ast.Match.AssocRef{ .InstFun = ifun };
                        ar.*.?.InstFun.m = nuMatch;
                        try assocs.append(ar);

                        changed = true;
                    } else {
                        try assocs.append(moldAssoc);
                    }
                    continue;
                },
            }

            // NOTE: this was a way to catch match errors! not needed now ig?
            unreachable;
        }
        try assocs.append(moldAssoc);
    }

    std.debug.assert(tvars.items.len == mm.tvars.len);
    std.debug.assert(envs.items.len == mm.envVars.len);
    std.debug.assert(assocs.items.len == mm.assocs.len);

    if (!changed) {
        tvars.deinit();
        envs.deinit();
        assocs.deinit();
        return null;
    }

    return try common.allocOne(self.arena, ast.Match{
        .scheme = mm.scheme,
        .tvars = tvars.items,
        .envVars = envs.items,
        .assocs = assocs.items,
    });
}

fn mapNum(self: *Self, match: anytype, numref: ast.NumRef) ?ast.NumRef {
    switch (self.getNum(numref)) {
        .TNum => |tnum| return match.mapTNum(tnum),
        else => return numref,
    }

    return null;
}

fn mapEnv(self: *Self, match: anytype, envref: ast.EnvRef) error{OutOfMemory}!ast.EnvRef {
    const envAndBase = self.getEnv(envref);
    if (match.mapEnv(envAndBase.base)) |nue| {
        return nue;
    } else {
        return if (envAndBase.env.*) |env| bb: {
            const menvMatch = try self.mapMatch_(match, env.match);
            break :bb if (menvMatch) |envMatch| b: {
                break :b try self.newEnv(.{
                    .env = env.env,
                    .match = envMatch,
                    .fun = env.fun,
                    .level = env.level,
                });
            } else b: {
                break :b envref;
            };
        } else bb: {
            // i guess we just return the normal one? random choice.
            break :bb envref;

            // try self.newEnv(null); // IMPORTANT: must instantiate new env..
        };
    }
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

pub fn cloneWithAllocator(self: *const Self, nuerrs: *Errors, al: std.mem.Allocator) !Self {
    const arrclone = common.cloneArrayListWithAllocator;
    return .{
        .context = try arrclone(self.context, al),
        .envContext = try arrclone(self.envContext, al),
        .numContext = try arrclone(self.numContext, al),
        .tyvarFields = try self.tyvarFields.cloneWithAllocator(al),
        .gen = self.gen,
        .errors = nuerrs,
        .arena = self.arena, // for `mapType` functions
    };
}
