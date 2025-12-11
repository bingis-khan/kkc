const std = @import("std");
const ast = @import("ast.zig");
const UniqueGen = @import("UniqueGen.zig");
const Unique = UniqueGen.Unique;
const @"error" = @import("error.zig");
const Errors = @"error".Errors;
const common = @import("common.zig");
const Str = common.Str;
const Set = @import("Set.zig").Set;

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

const Self = @This();
pub fn init(al: std.mem.Allocator, errors: *Errors) !Self {
    const context = TyStore.init(al);

    return .{
        .context = context,
        .envContext = EnvStore.init(al),
        .tyvarFields = TyVarFields.init(al),
        .gen = UniqueGen.init(),
        .errors = errors,
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

pub fn unify(self: *Self, t1: TyRef, t2: TyRef) error{OutOfMemory}!void {
    const tt1 = self.getType(t1);

    // handle tyvars, cuz it's easier.
    // TODO: occurs check
    switch (tt1) {
        .TyVar => |tyv| {
            if (self.getFieldsForTVar(tyv)) |fields| {
                for (fields) |f| {
                    const t2f = try self.field(t2, f.name);
                    try self.unify(t2f, f.t);
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
                            const t1f = try self.field(t1, f.name);
                            try self.unify(t1f, f.t);
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
                    // TODO: unstupidify (look next)
                    for (fields2) |f2| {
                        // assume deduplicated.
                        for (fields1) |f1| {
                            if (common.streq(f2.field, f1.field)) {
                                try self.unify(f1.t, f2.t);
                                break;
                            }
                        } else {
                            try self.errors.append(.{ .TypeDoesNotHaveField = .{ .t = t1, .field = f2.field } });
                        }
                    }

                    // BRUH
                    for (fields1) |f1| {
                        // assume deduplicated.
                        for (fields2) |f2| {
                            if (common.streq(f2.field, f1.field)) {
                                try self.unify(f1.t, f2.t);
                                break;
                            }
                        } else {
                            try self.errors.append(.{ .TypeDoesNotHaveField = .{ .t = t2, .field = f1.field } });
                        }
                    }
                    self.setType(t2, t1);
                },
                .Con => unreachable,
                else => unreachable, // error!
            }
        },
        .Con => |lcon| {
            switch (tt2) {
                .Con => |rcon| {
                    if (!lcon.type.eq(rcon.type)) {
                        try self.errMismatch(t1, t2);
                        return;
                    }

                    try self.unifyMatch(lcon.application, rcon.application);
                },
                else => try self.errMismatch(t1, t2),
            }
        },
        .Fun => |lfun| {
            switch (tt2) {
                .Fun => |rfun| {
                    try self.unifyEnv(lfun.env, rfun.env);
                    try self.unify(lfun.ret, rfun.ret);
                    try self.unifyParams(lfun.args, rfun.args);
                },
                else => try self.errMismatch(t1, t2),
            }
        },
        .TVar => |ltv| {
            switch (tt2) {
                .TVar => |rtv| {
                    if (!ltv.eq(rtv)) {
                        return try self.errMismatch(t1, t2);
                    }
                },

                else => try self.errMismatch(t1, t2),
            }
        }, // TODO
    }
}

pub fn getFieldsForTVar(self: *const Self, tyv: ast.TyVar) ?[]ast.Record {
    return if (self.tyvarFields.get(tyv)) |fields|
        fields.items
    else
        null;
}

pub fn field(self: *Self, t: ast.Type, mem: Str) !ast.Type {
    switch (self.getType(t)) {
        .Anon => |recs| {
            for (recs) |rec| {
                if (common.streq(rec.field, mem)) {
                    return rec.t;
                }
            } else {
                try self.errors.append(.{ .TypeDoesNotHaveField = .{
                    .t = t,
                    .field = mem,
                } });
                return try self.fresh();
            }
        },
        .TVar => |tv| {
            for (tv.fields) |f| {
                if (common.streq(f.name, mem)) {
                    return f.t;
                }
            } else {
                try self.errors.append(.{ .TypeDoesNotHaveField = .{
                    .t = t,
                    .field = mem,
                } });
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
                if (common.streq(rec.name, mem)) {
                    return rec.t;
                }
            } else {
                const ft = try self.fresh();
                try fields.append(.{ .name = mem, .t = ft });
                return ft;
            }
        },

        .Con => |con| {
            const data = con.type;
            switch (data.stuff) {
                .cons => {
                    try self.errors.append(.{ .TypeIsNotARecord = .{ .t = t, .field = mem } });
                    return try self.fresh();
                },
                .recs => |recs| {
                    for (recs) |rec| {
                        if (common.streq(rec.name, mem)) {
                            unreachable; // TODO: must map the type
                        }
                    } else {
                        unreachable;
                    }
                },
            }
        },

        .Fun => unreachable, // error
    }
}

fn unifyEnv(self: *Self, lenvref: EnvRef, renvref: EnvRef) !void {
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
        try self.envMismatch(lenv, renv);
        return;
    }

    for (lenv, renv) |lv, rv| {
        if (!std.meta.eql(lv, rv)) {
            try self.envMismatch(lenv, renv);
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

pub fn unifyMatch(self: *Self, lm: *const ast.Match(ast.Type), rm: *const ast.Match(ast.Type)) !void {
    try self.unifyParams(lm.tvars, rm.tvars);
}

pub fn unifyParams(self: *Self, lps: []TyRef, rps: []TyRef) !void {
    if (lps.len != rps.len) {
        try self.paramLenMismatch(lps.len, rps.len);
        return;
    }

    for (lps, rps) |lp, rp| {
        try self.unify(lp, rp);
    }
}

fn errMismatch(self: *Self, lt: TyRef, rt: TyRef) !void {
    try self.errors.append(.{ .MismatchingTypes = .{ .lt = lt, .rt = rt } });
}

fn envMismatch(self: *Self, lenv: ast.Env, renv: ast.Env) !void {
    try self.errors.append(.{ .MismatchingEnv = .{ .le = lenv, .re = renv } });
}

fn paramLenMismatch(self: *Self, lpl: usize, rpl: usize) !void {
    try self.errors.append(.{ .MismatchingParamLen = .{ .lpl = lpl, .rpl = rpl } });
}

fn setType(self: *Self, tref: TyRef, tdest: TyRef) void {
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

// copied from parser.zig until I solve how I should report errors (probably an Errors struct that can be passed down to various components.)
fn err(self: *Self, comptime t: type, comptime fmt: []const u8, args: anytype) !t {
    std.debug.print(fmt ++ " at {}\n", args ++ .{self.currentToken});
    std.debug.print("{s}\n", .{self.lexer.source[self.currentToken.from -% 5 .. @min(self.lexer.source.len, self.currentToken.to +% 5)]});
    return error.ParseError;
}
