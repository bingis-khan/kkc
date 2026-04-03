const Str = @import("common.zig").Str;
const std = @import("std");
const Unique = @import("UniqueGen.zig").Unique;
const TypeContext = @import("TypeContext.zig");
const common = @import("common.zig");
const Intrinsic = @import("Intrinsic.zig");
const Loc = common.Location;
const Set = @import("Set.zig").Set;
const stack_set = @import("stack_set.zig");
const StackSet = stack_set.StackSet;

pub const TyRefPointer = true;

toplevel: []*Stmt, // top level

pub const Ctx = struct {
    indent: u32,
    hadNewline: *bool, // check newlines and automatically indent
    typeContext: *const TypeContext,
    mapTypes: ?*const Match,
    lastMatch: ?*const Match, // HACK: because envs have circular dependencies to the match...

    pub fn pp(typeContext: *const TypeContext, x: anytype) void {
        var hadNewline = false;
        var ctx = Ctx.init(&hadNewline, typeContext);
        ctx.print(x);
        ctx.print("\n");
    }

    const Self = @This();
    pub fn init(hadNewline: *bool, typeContext: *const TypeContext) Self {
        hadNewline.* = true;
        return Ctx{
            .indent = 0,
            .hadNewline = hadNewline,
            .typeContext = typeContext,
            .mapTypes = null,
            .lastMatch = null,
        };
    }

    pub fn Wrapped(t: type) type {
        return struct {
            t: t,

            pub fn print(self: @This(), c: Ctx) void {
                _ = c;
                std.debug.print("{}", .{self.t});
            }
        };
    }
    pub fn wrap(t: anytype) Wrapped(@TypeOf(t)) {
        return Wrapped(@TypeOf(t)){ .t = t };
    }

    pub fn OnlyIf(t: type) type {
        return struct {
            t: t,
            cond: bool,

            pub fn print(self: *const @This(), c: Ctx) void {
                if (self.cond) {
                    c.print(self.t);
                }
            }
        };
    }

    pub fn onlyIf(cond: bool, p: anytype) OnlyIf(@TypeOf(p)) {
        return OnlyIf(@TypeOf(p)){
            .t = p,
            .cond = cond,
        };
    }

    pub fn Iter(itt: type, sept: type, comptime mapFn: anytype) type {
        return struct {
            it: itt,
            sep: sept,

            pub fn print(self: *const @This(), c: Ctx) void {
                var it = self.it;
                if (it.next()) |x| {
                    mapFn(x, c);
                } else {
                    return;
                }

                while (it.next()) |x| {
                    c.s(self.sep);
                    mapFn(x, c); // TODO: when needed, make it more general. (eg: take in a function.)
                }
            }
        };
    }
    pub fn iter(it: anytype, sep: anytype) Iter(@TypeOf(it), @TypeOf(sep), struct {
        pub fn mapFn(x: anytype, c: Ctx) void {
            return x.*.print(c);
        }
    }.mapFn) {
        return .{
            .it = it,
            .sep = sep,
        };
    }

    pub fn iter_(it: anytype, sep: anytype, comptime mapFn: anytype) Iter(@TypeOf(it), @TypeOf(sep), mapFn) {
        return Iter(@TypeOf(it), @TypeOf(sep), mapFn){
            .it = it,
            .sep = sep,
        };
    }

    pub fn s(self: Self, ss: Str) void {
        var last_nl_i: usize = 0;
        for (ss, 0..) |c, i| {
            if (c == '\n') {
                if (last_nl_i != i) {
                    if (self.hadNewline.*) {
                        self.sindent();
                    }
                    std.debug.print("{s}\n", .{ss[last_nl_i..i]});
                } else {
                    std.debug.print("\n", .{});
                }
                self.hadNewline.* = true;
                last_nl_i = i + 1;
            }
        }

        // copied from upper shit.
        if (last_nl_i != ss.len) {
            if (self.hadNewline.*) {
                self.sindent();
                self.hadNewline.* = false;
            }
            std.debug.print("{s}", .{ss[last_nl_i..ss.len]});
        }
    }

    fn sindent(self: Self) void {
        for (0..self.indent) |_| {
            std.debug.print("  ", .{});
        }
    }

    // HACK: don't use newlines in format strings PLS
    pub fn sp(self: Self, comptime fmt: []const u8, args: anytype) void {
        if (fmt.len > 0 and self.hadNewline.*) {
            self.sindent();
            self.hadNewline.* = false;
        }
        std.debug.print(fmt, args);
    }

    pub fn print(self: Self, args: anytype) void {
        switch (@typeInfo(@TypeOf(args))) {
            .Struct => {
                const fields = @typeInfo(@TypeOf(args)).Struct.fields;
                inline for (fields) |field| {
                    // switch (@typeInfo(@TypeOf(arg))) {
                    //     .Pointer => |ptrinfo| {
                    //         switch (ptrinfo.size) {
                    //             .Slice => std.debug.print("{s}", .{arg}),
                    //             .Many => std.debug.print("{s}", .{arg}),
                    //             .One => std.debug.print("{s}", .{arg}),
                    //             else => unreachable,
                    //         }
                    //     }, // only for this: assumme an array is a STRING. (because it's hard to check for one).
                    //     else => arg.print(self),
                    // }
                    // dumbest thing. const strings are a pointer to ONE(!), which means I can't call print on pointers if I proceed in this direction.
                    const arg = @field(args, field.name);
                    self.printArg(arg, field.type);
                }
            },

            else => {
                // assume it's a single arg!
                self.printArg(args, @TypeOf(args));
            },
        }
    }

    fn printArg(self: Self, arg: anytype, t: anytype) void {
        if (comptime std.meta.hasMethod(t, "print")) {
            arg.print(self);
        } else {
            switch (@typeInfo(@TypeOf(arg))) {
                .Int, .Float, .ComptimeInt, .ComptimeFloat => self.sp("{}", .{arg}),
                else => self.s(@as(Str, arg)), // IF THIS CAST FAILS, IT MEANS YOU MUST ADD `pub` TO YOUR `fn print()`
            }
        }
    }

    pub fn sepBy(self: Self, args: anytype, sep: Str) void {
        if (args.len == 0) return;

        var margs = args;
        margs.len = args.len - 1;

        for (margs) |e| {
            e.print(self);
            self.s(sep);
        }
        args[args.len - 1].print(self);
    }

    pub fn encloseSepBy(self: Self, args: anytype, sep: Str, l: Str, r: Str) void {
        self.s(l);
        self.sepBy(args, sep);
        self.s(r);
    }

    // not sure I'll keep the :: (and instead opt for something shorter), so I will use this function from now on.
    fn typed(self: Self, t: Type) void {
        self.s(" :: ");
        t.print(self);
    }
};

// NOTE: right now, use debug statements
pub fn print(self: @This(), c: Ctx) void {
    for (self.toplevel) |dec| {
        dec.print(c);
    }
}

pub const Function = struct {
    pub const Instantiation = struct {
        t: Type,
        m: *Match,

        fn print(self: @This(), c: Ctx) void {
            self.t.print(c);
        }
    };

    pub const Use = union(enum) {
        Fun: struct { fun: *Function, m: *const Match },
        ClassFun: struct { cfun: *ClassFun, ref: InstFunInst },

        fn print(self: @This(), c: Ctx) void {
            switch (self) {
                .Fun => |fun| {
                    c.s("@"); // tag that it's a function instantiation.
                    fun.fun.name.print(c);
                },
                .ClassFun => |cf| {
                    const cfunName = cf.cfun.name;
                    c.print(.{ "$", cfunName });
                },
            }
        }
    };

    pub const MonoMatchStuff = struct {
        uses: u32,
        completes: EnvCompletes,
    };
    pub const MonoMatches = std.HashMap(*const Match, MonoMatchStuff, Match.Comparator, std.hash_map.default_max_load_percentage);
    pub const EnvCompletes = Set(FunApp, FunApp.Comparator);
    pub const FunApp = struct {
        fun: *const Function,
        m: *const Match,

        pub fn print(self: @This(), c: Ctx) void {
            c.print(.{ self.fun.name, self.m });
        }

        pub const Comparator = struct {
            typeContext: *const TypeContext,

            pub fn eql(ctx: @This(), a: FunApp, b: FunApp) bool {
                return a.fun.name.uid == b.fun.name.uid and Match.Comparator.eql(.{ .typeContext = ctx.typeContext }, a.m, b.m);
            }

            pub fn hash(ctx: @This(), k: FunApp) u64 {
                return k.fun.name.uid * 497192 + Match.Comparator.hash(.{ .typeContext = ctx.typeContext }, k.m);
            }
        };
    };
    pub const Mono = struct {
        uses: std.ArrayList(Use),
        alreadyExpanded: bool,
        alreadyGenerated: bool, // TEMP: unused for now
        matches: MonoMatches,

        pub fn empty(typeContext: *const TypeContext, al: std.mem.Allocator) @This() {
            return .{
                .uses = std.ArrayList(Use).init(al),
                .alreadyExpanded = false,
                .alreadyGenerated = false,
                .matches = MonoMatches.initContext(al, .{ .typeContext = typeContext }),
            };
        }
    };

    name: Var,
    params: []DeconBase,
    ret: Type,
    body: []*Stmt,
    scheme: Scheme,
    env: *Env,
    temp__isRecursive: bool, // TODO(true-recursion): very hacky!!

    temp__mono: Mono,

    // TODO(environment-representation)
    // so, uhh, there were problems properly adding proper environments. currently it's all fucked up.
    // In short, problems and stuff Ive done. This will be useful, because even though i might change the way it gets typechecked, itll still be useful for monomorphising. currently, i tried the monomorphization-friendly version.
    //  - top level calls were wrong and i discovered the weird environment making thingy.
    //  - what even should I put in the env? i tried making themem properly monomorphised (if I figure it out and make it fast, mono is gonna be easy)
    //     - in the future, i should make a basic version that would work for a polymorphic interpreter / bytecode compiler and by these metrics define correctness. it should also be fast.
    //  - current version is requires tracking calls and if we're finished parsing
    //   - tracking parsing state is because when we close over the function, we later use it to "spill over", so adding stuff to lower envs would duplicate env insts.
    //  - with class calls, we need to add "monomorphised" versions of functions, which means we must track through which "match"es they are called, so we need to track calls to duplicate the environment through each type of call.
    //  - as a hack, the call to addEnvIfPossible is split into two versions.
    //  - the other version expands environment from the place of the function (because outer class functions can call "inner" functions, check 5_t36)
    //  - but, when evaluating recursive class calls, we actively add calls, so we have to check if the call is coming from the same function we are expanding the environment from and only use its one match it came from.(all of it is in addEnvIfPossible)
    temp__calls: std.ArrayList(Instantiation), // when retroactively adding stuff to env, we want to know if a function was even called and HOW.
    temp__finishedParsing: bool,

    fn print(self: @This(), c: Ctx) void {
        c.print(.{ self.name, " (" });
        c.sepBy(self.params, ", ");
        c.s(")");
        // self.env.print(c); // TEMP
        // c.encloseSepBy(self.temp__calls.items, ", ", "[", "]");
        c.s(" -> ");
        self.ret.print(c);
        c.s(" ## ");
        self.scheme.print(c);
        c.s("\n");

        printBody(self.body, c);
    }

    pub fn isDefinedAfterOrAt(self: *const @This(), other: *const @This()) bool {
        // FUNNY IMPLEMENTATION BRUHHHHHHH but technically true.
        // TODO: what about "recursive" definitions?
        return self.name.uid >= other.name.uid;
    }
};

pub const EnvFun = struct {
    env: *Env,
    fun: ?*Function,

    pub fn getEnv(mself: ?@This()) ?*Env {
        return if (mself) |self| self.env else null;
    }

    pub fn getFun(mself: ?@This()) ?*Function {
        var mef = mself;
        while (mef) |ef| {
            if (ef.fun) |fun| {
                return fun;
            }

            mef = ef.env.outer;
        }

        return null;
    }
};
pub const Env = struct {
    id: Unique,
    insts: std.ArrayList(EnvVar),
    level: usize,
    monoInsts: Mono, // TEMP/TODO: for now we will reuse this env struct for mono insts. We might need to remake the structure??
    monoFinished: bool = false,

    outer: ?EnvFun,

    pub const Mono = Set(EnvVar, EnvVar.Comparator);

    // NOTE: that's due to a small thing where a polymorphic variable might use the same function as the one that's "prebaked".
    // eg:
    // fn f(x)
    //   id(x)
    //   id(1)
    // f(1) <- it'll have two functions, so it's kinda cringe.
    // So, we need to deduplicate them.
    // this is gay, cuz it's slow.
    pub fn deduplicatedEnvInsts(self: *const @This(), al: std.mem.Allocator, m: anytype, tc: *TypeContext) !Mono {
        var dedupped = Mono.initContext(al, self.monoInsts.hash.ctx);
        var oleIt = self.monoInsts.iterator();
        while (oleIt.next()) |inst| {
            var minst = inst.*;
            switch (inst.v) {
                .ClassFun => |cfun| {
                    switch (cfun.ref.*.?) {
                        .Id => |id| {
                            const ifn = m.tryGetFunctionByID(id).?;
                            minst.v = .{ .Fun = ifn.fun };
                            minst.m = ifn.m;
                        },
                        .InstFun => |ifn| {
                            minst.v = .{ .Fun = ifn.fun };
                            minst.m = ifn.m;
                        },
                    }
                },
                else => {},
            }
            minst.m = try tc.mapMatch(m, minst.m);
            try dedupped.insert(minst);
        }

        return dedupped;
    }

    pub fn print(self: *const @This(), c: Ctx) void {
        if (self.monoFinished) {
            c.print(.{ "{", Ctx.iter(self.monoInsts.iterator(), ", "), "}" });
        } else {
            c.encloseSepBy(self.insts.items, ", ", "[", "]");
        }
        c.print(.{ "(", self.level, ")" });
    }

    // for special cases where there is no body
    pub fn empty(id: Unique) Env {
        return .{
            .id = id,
            .insts = std.ArrayList(EnvVar){
                .allocator = undefined,
                .capacity = 0,
                .items = &.{},
            },
            .level = 0,
            .outer = null,
            .monoInsts = Mono{
                .hash = .{
                    .unmanaged = .{},
                    .allocator = undefined,
                    .ctx = undefined,
                },
            },
        };
    }

    pub fn nextFunction(self: *const @This()) ?*const Function {
        var out = self.outer;
        while (out) |envfun| {
            if (envfun.fun) |fun| {
                return fun;
            }

            out = envfun.env.outer;
        }

        return null;
    }
};

pub const Level = usize;
pub const EnvVar = struct {
    v: union(enum) {
        Fun: *Function,
        ClassFun: struct { cfun: *ClassFun, ref: InstFunInst }, // used when a class function depends on a TVar from outer scope - then we MUST consider it a class function.
        Var: DeconVar,
        TNum: TNum,
    },
    m: *const Match,
    t: Type,
    l: Level,

    // when we get to equality, we'll ignore the type and level, because I think they are irrelevant except for displaying.

    pub fn locality(self: *const @This(), funenv: *const Env) Locality { // funenv: check if the current envvar came from the outside relative to env instantiation(!)
        if (funenv.outer) |outer| {
            return if (self.l < outer.env.level) .External else .Local;
        } else {
            return .Local;
        }
    }

    pub fn getVar(self: @This()) Var {
        return switch (self.v) {
            .Fun => |fun| fun.name,
            .ClassFun => |cfun| cfun.cfun.name,
            .Var => |v| v,
            .TNum => |tnum| .{
                .name = tnum.name,
                .uid = tnum.uid,
            },
        };
    }

    pub fn print(self: @This(), c: Ctx) void {
        switch (self.v) {
            .TNum => |tnum| {
                tnum.print(c);
            },

            .Fun => |fun| {
                c.s("@"); // tag that it's a function instantiation.
                fun.name.print(c);
            },

            .ClassFun => |cfun| {
                const cfunName = cfun.cfun.name;
                c.print(.{ "$", cfunName });
            },

            .Var => |v| {
                v.print(c);
            },
        }

        c.print(.{ "(", self.l, ")" });
        c.s(" ");
        self.m.print(c);
        self.t.print(c);
    }

    const Comparator = struct {
        typeContext: *const TypeContext,

        pub fn eql(ctx: @This(), a: EnvVar, b: EnvVar) bool {
            return switch (a.v) {
                .Fun => |f1| switch (b.v) {
                    .Fun => |f2| bb: {
                        if (!Var.comparator().eql(.{}, f1.name, f2.name)) break :bb false;

                        return Match.Comparator.eql(.{ .typeContext = ctx.typeContext }, a.m, b.m);
                    },
                    else => false,
                },
                .ClassFun => |lfun| switch (b.v) {
                    .ClassFun => |rfun| {
                        if (lfun.cfun.uid != rfun.cfun.uid) return false;
                        switch (lfun.ref.*.?) {
                            .Id => |lid| switch (rfun.ref.*.?) {
                                .Id => |rid| return lid == rid,
                                .InstFun => unreachable,
                            },
                            .InstFun => unreachable,
                        }
                    },
                    else => false,
                },
                .Var => |v1| switch (b.v) {
                    .Var => |v2| Var.comparator().eql(.{}, v1.v, v2.v),
                    else => false,
                },
                .TNum => unreachable,
            };
        }

        pub fn hash(ctx: @This(), k: EnvVar) u64 {
            _ = ctx;
            return switch (k.v) {
                .Fun => |fun| fun.name.uid * 557891,
                .ClassFun => |cfun| cfun.cfun.name.uid * 14981,
                .Var => |v| v.v.uid * 1337,
                .TNum => unreachable,
            };
        }
    };
};

fn printBody(stmts: []*Stmt, oldC: Ctx) void {
    var c = oldC;
    c.indent +%= 1;

    for (stmts) |stmt| {
        stmt.print(c);
        // this will happen in Stmt.print(), because we don't want to put newlines after nesting stmts
        // c.s("\n");
    }
}

pub const Stmt = union(enum) {
    VarDec: struct { varDef: Var, varValue: *Expr },
    VarMut: struct { varRef: DeconVar, locality: Locality, accessors: []Accessor, varValue: *Expr },
    If: struct {
        cond: *Expr,
        bTrue: []Rec,
        bOthers: []Elif,
        bElse: ?[]Rec,
    },
    While: struct {
        cond: *Expr,
        body: []Rec,
    },
    For: struct { // TODO: later, I should remove it and just generate some code. The only benefit is better type errors (which I can ensure, because we typecheck while parsing) and easier debuggability (only for compiler development, so whatever)
        decon: DeconBase,
        iter: *Expr,
        intoIterFun: InstFunInst,
        nextFun: InstFunInst,
        body: []Rec,
    },
    Switch: struct {
        switchOn: *Expr,
        refvar: Var,
        cases: []Case,
    },
    Return: *Expr,
    Break: struct {},
    Function: *Function,
    Instance: *Instance,
    Pass: ?i64,
    Expr: *Expr,

    const Rec = *@This();
    pub const Elif = struct { cond: *Expr, body: []Rec };
    pub const Accessor = struct {
        tBefore: Type, // ease our interpreter and compiler
        acc: union(enum) {
            Deref,
            Access: Str,
            // index
        },
    };

    pub fn print(self: @This(), c: Ctx) void {
        switch (self) {
            .Pass => |mlbl| {
                c.s("pass");
                if (mlbl) |lbl| {
                    c.print(.{ " ", lbl });
                }
                c.s("\n");
            },
            .VarDec => |vd| {
                vd.varDef.print(c);
                c.s(" = ");
                vd.varValue.print(c);
                c.s("\n");
            },
            .VarMut => |vm| {
                c.print(.{ vm.varRef, " <" });
                for (vm.accessors) |acc| {
                    switch (acc.acc) {
                        .Deref => c.s("&"),
                        .Access => |field| c.sp(".{s}", .{field}),
                    }
                }
                c.print(.{ "= ", vm.varValue, "\n" });
            },
            .Expr => |expr| {
                c.print(.{ expr, "\n" });
            },
            .If => |ifstmt| {
                c.s("if ");
                ifstmt.cond.print(c);
                c.s("\n");
                printBody(ifstmt.bTrue, c);

                for (ifstmt.bOthers) |elif| {
                    c.s("elif ");
                    elif.cond.print(c);
                    c.s("\n");
                    printBody(elif.body, c);
                }

                if (ifstmt.bElse) |els| {
                    c.s("else\n");
                    printBody(els, c);
                }
            },
            .While => |whl| {
                c.print(.{ "while ", whl.cond, "\n" });
                printBody(whl.body, c);
            },
            .For => |forl| {
                c.print(.{ "for ", forl.decon, " in ", forl.iter, "\n" });
                printBody(forl.body, c);
            },
            .Switch => |sw| {
                c.s("case ");
                sw.switchOn.print(c);
                c.s("\n");

                var ic = c;
                ic.indent +%= 1;

                for (sw.cases) |case| {
                    case.print(ic);
                }
            },
            .Return => |expr| {
                c.s("return ");
                expr.print(c);
                c.s("\n");
            },
            .Break => {
                c.s("break\n");
            },
            .Function => |fun| {
                fun.print(c);
            },
            .Instance => |inst| {
                inst.print(c);
            },
        }
    }
};

pub const Case = struct {
    decon: *Decon,
    body: []*Stmt,

    fn print(self: *const @This(), c: Ctx) void {
        self.decon.print(c);
        c.s("\n");
        printBody(self.body, c);
    }
};

pub const DeconVar = struct {
    v: Var,
    dc: ?DeconUse,

    pub fn print(self: @This(), c: Ctx) void {
        self.v.print(c);
    }
};
pub const DeconUse = struct { dp: *const Decon.Path };
pub const DeconBase = struct {
    d: *Decon,
    refvar: Var,

    pub fn print(self: @This(), c: Ctx) void {
        self.d.print(c);
    }
};
pub const Decon = struct {
    t: Type,
    l: Loc,
    d: union(enum) {
        None: struct {},
        Num: i64,
        Var: Var,
        Con: struct {
            con: *Con,
            decons: []*Decon,
        },
        Record: []Field,

        List: struct {
            l: []DeconBase,
            r: ?struct {
                spreadVar: ?struct { v: Var, t: Type },
                r: []DeconBase,
            },

            elemTy: Type,
            spreadTy: Type,

            // for class function "deconstruct"
            assocRef: InstFunInst,
        },
    },

    pub const Field = struct {
        field: Str,
        decon: *Decon,

        fn print(self: @This(), c: Ctx) void {
            c.print(.{ self.field, ": ", self.decon });
        }
    };

    pub fn print(self: *const @This(), c: Ctx) void {
        switch (self.d) {
            .None => c.s("_"),
            .Var => |v| v.print(c),
            .Num => |num| c.print(num),
            .Con => |con| {
                con.con.print(c);
                if (con.decons.len > 0) {
                    c.encloseSepBy(con.decons, ", ", "(", ")");
                }
            },
            .Record => |fields| {
                c.encloseSepBy(fields, ", ", "{ ", " }");
            },
            .List => |l| {
                if (l.r) |r| {
                    c.print("[");
                    c.sepBy(l.l, ", ");
                    if (l.l.len > 0) {
                        c.print(", ");
                    }
                    c.print("...");
                    if (r.spreadVar) |v| {
                        c.print(.{ v.v, " :: ", v.t });
                    }
                    if (r.r.len > 0) {
                        c.print(", ");
                    }
                    c.sepBy(r.r, ", ");
                    c.print("]");
                } else {
                    c.encloseSepBy(l.l, ", ", "[", "]");
                }
            },
        }

        c.typed(self.t);
    }

    pub const Path = PathF(.Poly);
    pub const PathM = PathF(.Mono);
    pub fn PathF(e: enum { Poly, Mono }) type {
        return union(enum) {
            Tip: struct { v: Var, t: TyRef },
            Concat: struct {
                path: PathF(e).Type,
                next: *const PathF(e),
            },

            pub const Type = union(enum) {
                Con: struct {
                    con: *const Con,
                    field: usize,
                    t: if (e == .Poly) TyRef else Unique,
                },
                Field: Str,
                Ptr,
                None, // in case of tuples, we don't know if it's just a grouping paren OR a tuple only AFTER we parse a deconstruction. So, we need to patch it in later. In case it's just grouping, have a None path, which does nothing.
                // List: *const struct {},

            };

            pub fn init(al: std.mem.Allocator, self: @This()) !*@This() {
                return try common.allocOne(al, self);
            }

            pub fn concat(al: std.mem.Allocator, next: *const Path, path: Path.Type) !*@This() {
                return try common.allocOne(al, Path{
                    .Concat = .{
                        .path = path,
                        .next = next,
                    },
                });
            }
        };
    }
};

pub const Expr = struct {
    t: Type,
    l: Loc,
    e: union(enum) {
        BinOp: struct { l: Rec, op: BinOp, r: Rec },
        UnOp: struct { e: Rec, op: UnOp },
        Call: struct { callee: Rec, args: []Rec },
        Var: struct { v: VarType, match: *Match, locality: Locality }, // NOTE: Match is owned here!
        Con: *Con,
        Intrinsic: struct { intr: Intrinsic, args: []Rec },
        Int: struct { int: i64, ref: InstFunInst }, // obv temporary.
        Float: f64,
        IfElse: struct {
            cond: Rec,
            ifTrue: Rec,
            ifOthers: []Elif,
            ifFalse: Rec,
        },
        CaseExpr: struct {
            switchOn: Rec,
            refvar: Var,
            cases: []ExprCase,
        },
        Str: Str,
        Char: u8, // later RUNE!
        NamedRecord: struct {
            data: *Data,
            fields: []Field,

            pub fn print(self: @This(), c: Ctx) void {
                self.data.print(c);
                c.s(" ");
                c.encloseSepBy(self.fields, ", ", "{ ", " }");
            }
        },
        AnonymousRecord: []Field,
        Lam: Lam,
        StaticArray: []Rec,
    },

    pub const ExprCase = union(enum) {
        Case: Case,
        Expr: struct { decon: *Decon, expr: Rec },

        pub fn print(self: @This(), c: Ctx) void {
            switch (self) {
                .Case => |case| {
                    case.decon.print(c);
                    c.s("{\n");
                    printBody(case.body, c);
                    c.s("}\n");
                },

                .Expr => |exp| {
                    c.print(.{ exp.decon, ": ", exp.expr, "\n" });
                },
            }
        }
    };

    pub const Elif = struct {
        cond: Rec,
        then: Rec,
    };

    pub const Lam = struct {
        params: []DeconBase,
        env: *Env,
        body: union(enum) {
            Expr: Rec,
            Body: struct {
                stmts: []*Stmt,
                ret: Type,
            },

            pub fn print(self: *const @This(), c: Ctx) void {
                switch (self.*) {
                    .Expr => |expr| c.print(.{ ": ", expr }),
                    .Body => |body| {
                        c.print("{\n");
                        printBody(body.stmts, c);
                        c.print("}");
                    },
                }
            }
        },

        pub fn print(self: *const @This(), c: Ctx) void {
            c.print("fn ");
            c.encloseSepBy(self.params, ", ", "(", ")");
            self.env.print(c);
            c.print(self.body);
        }
    };

    pub const Field = struct {
        field: Str,
        value: Rec,

        pub fn print(self: @This(), c: Ctx) void {
            c.print(.{ self.field, ": ", self.value });
        }
    };

    pub const VarType = union(enum) {
        Fun: *Function,
        ClassFun: struct {
            cfun: *ClassFun,
            ref: InstFunInst,
        }, // SMELL: this one I have to allocate, because I'm returning the whole struct from a function, so the address will change.
        Var: DeconVar,
        ExternalFun: *ExternalFunction,
        TNum: TNum,

        // pub fn getVar(self: @This()) Var {
        //     return switch (self) {
        //         .Var => |v| v,
        //         .Fun => |fun| fun.name,
        //         else => undefined,
        //     };
        // }

        pub fn print(self: @This(), c: Ctx) void {
            switch (self) {
                .TNum => |tnum| {
                    tnum.print(c);
                },
                .Var => |v| {
                    v.print(c);
                },
                .Fun => |fun| {
                    fun.name.print(c);
                },
                .ClassFun => |cfun| {
                    cfun.cfun.name.print(c);
                },
                .ExternalFun => |extfun| {
                    extfun.name.print(c);
                },
            }
        }
    };
    const Rec = *@This();

    pub fn print(self: @This(), c: Ctx) void {
        c.s("(");
        switch (self.e) {
            .Var => |v| {
                v.v.print(c);
            },
            .Con => |con| {
                con.print(c);
            },
            .Int => |i| c.sp("{}", .{i.int}),
            .Float => |f| c.sp("{}", .{f}),
            .Char => |ch| c.sp("c'{}'", .{ch}),
            .Str => |s| {
                std.debug.lockStdErr();
                defer std.debug.unlockStdErr();
                std.zig.stringEscape(s, "'", .{}, std.io.getStdErr().writer()) catch unreachable;
            },
            .Intrinsic => |intr| {
                std.debug.print("{}", .{intr.intr.ty});
                if (intr.args.len > 0) {
                    c.encloseSepBy(intr.args, ", ", "(", ")");
                }
            },
            .BinOp => |bop| {
                c.s("(");
                bop.l.print(c);
                c.s(" ");
                bop.op.print(c);
                c.s(" ");
                bop.r.print(c);
                c.s(")");
            },
            .UnOp => |uop| {
                c.s("(");
                switch (uop.op) {
                    .Ref => {
                        c.s("&");
                        uop.e.print(c);
                    },
                    .Deref => {
                        uop.e.print(c);
                        c.s("&");
                    },
                    .Access => |acc| {
                        uop.e.print(c);
                        c.sp(".{s}", .{acc});
                    },
                    .Update => |upd| {
                        uop.e.print(c);
                        c.encloseSepBy(upd, ",", " { ", " }");
                    },
                    .As => |t| {
                        c.print(.{ uop.e, " as ", t });
                    },
                    .Not => {
                        c.print(.{ "not ", uop.e });
                    },
                    .Negate => {
                        c.print(.{ "-", uop.e });
                    },
                    .ElementAccess => |acc| {
                        c.print(.{ uop.e, "[", acc.index, "]" });
                    },
                }
                c.s(")");
            },
            .Call => |call| {
                call.callee.print(c);
                c.s("(");
                c.sepBy(call.args, ", ");
                c.s(")");
            },

            .AnonymousRecord => |fields| {
                c.encloseSepBy(fields, ", ", "{", "}");
            },
            .NamedRecord => |nrec| {
                c.print(.{ nrec.data, " " });
                c.encloseSepBy(nrec.fields, ", ", "{", "}");
            },
            .Lam => |l| {
                l.print(c);
            },
            .StaticArray => |arr| {
                c.encloseSepBy(arr, ", ", "[", "]");
            },
            .IfElse => |ifelse| {
                c.print(.{ "if ", ifelse.cond, ": ", ifelse.ifTrue });
                for (ifelse.ifOthers) |elif| {
                    c.print(.{ " elif ", elif.cond, ": ", elif.then });
                }
                c.print(.{ " else: ", ifelse.ifFalse });
            },
            .CaseExpr => |caseexpr| {
                c.print(.{ "case ", caseexpr.switchOn, "{\n" });

                var ic = c;
                ic.indent +%= 1;

                for (caseexpr.cases) |case| {
                    case.print(ic);
                }

                c.print("}");
            },
        }
        c.s(" :: ");
        self.t.print(c);
        c.s(")");
    }
};

pub const Locality = enum {
    Local,
    External,
};

pub const UnOp = union(enum) {
    Ref,
    Deref,
    Access: Str,
    Update: []Expr.Field,
    As: Type,
    Not,
    Negate: InstFunInst,
    ElementAccess: struct {
        access: InstFunInst,
        index: *Expr,
    },
};

pub const BinOp = union(enum) {
    Plus: InstFunInst,
    Minus: InstFunInst,
    Times: InstFunInst,
    Divide: InstFunInst,

    Equals: InstFunInst,
    NotEquals: InstFunInst,
    GreaterThan: InstFunInst,
    LessThan: InstFunInst,
    GreaterEqualThan: InstFunInst,
    LessEqualThan: InstFunInst,

    Or,
    And,

    // FAKE OPS
    // TODO: explain
    Call,
    PostfixCall,
    RecordAccess,
    RecordUpdate,
    Deref,
    As,
    ElementAccess,

    fn print(self: @This(), c: Ctx) void {
        const sop = switch (self) {
            .Plus => "+",
            .Minus => "-",
            .Times => "*",
            .Divide => "/",
            .Equals => "==",
            else => "XXX",
        };

        c.s(sop);
    }
};

pub const InstFunInst = *?Match.AssocRef;

pub const Type = TyRef;
pub const TyRef = struct {
    id: if (!TyRefPointer) usize else *TypeContext.TyStoreElem,

    pub fn print(tid: @This(), c: Ctx) void {
        c.typeContext.getType(tid).print(c);
    }

    pub fn eq(l: @This(), r: @This()) bool {
        return l.id == r.id;
    }

    pub fn tyEq(l: @This(), r: @This(), tyc: *const TypeContext) bool {
        if (l.eq(r)) return true;
        return switch (tyc.getType(l)) {
            .Con => |c1| switch (tyc.getType(r)) {
                .Con => |c2| {
                    if (!c1.type.eq(c2.type)) return false;
                    return Match.Comparator.eql(.{ .typeContext = tyc }, c1.application, c2.application);
                },
                .TVar => return false,
                .TyVar => |tyv| {
                    // assume Unit
                    if (tyc.getFieldsForTVar(tyv) != null) unreachable; // if has any fields, should not happen yo.
                    const unitData = tyc.prelude.?.defined(.Unit);
                    return c1.type.eq(unitData);
                },
                else => false,
            },
            .Fun => |f1| switch (tyc.getType(r)) {
                .Fun => |f2| {
                    if (!tyEq(f1.ret, f2.ret, tyc)) return false;
                    if (!tyEqs(f1.args, f2.args, tyc)) return false;
                    if (!EnvRef.envEq(f1.env, f2.env, tyc)) return false;
                    return true;
                },
                else => false,
            },
            .Anon => unreachable,

            .TVar => |ltv| {
                switch (tyc.getType(r)) {
                    .TVar => |rtv| return ltv.eq(rtv),
                    else => return false,
                }
            },
            .TyVar => |tyv| {
                if (tyc.getFieldsForTVar(tyv) != null) unreachable; // if has any fields, should not happen yo.

                // assume Unit if a simple tyvar
                switch (tyc.getType(r)) {
                    .Con => |con| {
                        const unitData = tyc.prelude.?.defined(.Unit);
                        return con.type == unitData;
                    },
                    .TyVar => |tyv2| {
                        if (tyc.getFieldsForTVar(tyv2) != null) unreachable; // if has any fields, should not happen yo.

                        return true;
                    },
                    else => unreachable,
                }
            },
        };
    }

    pub fn tyEqs(ls: []@This(), rs: []@This(), tyc: *const TypeContext) bool {
        for (ls, rs) |l, r| {
            if (!tyEq(l, r, tyc)) return false;
        }

        return true;
    }

    // fn mapTVar(self: @This(), tyc: *const TypeContext) @This() {
    //     return switch (tyc.getType(self)) {
    //         .TVar => |tv| {
    //         },
    //         else => self,
    //     };
    // }
};
pub const EnvRef = struct {
    id: usize,

    pub fn print(eid: @This(), c: Ctx) void {
        const eb = c.typeContext.getEnv(eid);
        // c.print(.{ "(", eb.env.id, ")" });
        if (eb.env.*) |env| {
            c.print(.{ "(", env.env.id, ")", "(", env.match, ")" });
            env.env.print(c);
        } else {
            c.s("[X]");
        }
    }

    // BRUH, this only makes sense when using mono, if it's used in parser, then bruh.
    // this is temporary, because I want to add unions at some point tho.
    pub fn envEq(l: @This(), r: @This(), tyc: *const TypeContext) bool {
        const le = tyc.getEnv(l);
        const re = tyc.getEnv(r);

        if (le.env.* == null and re.env.* == null) {
            return le.base.id == re.base.id;
        } else {
            // currently, we don't care about structural equality that much, especially if we're gonna implement unions.
            if (le.env.*.?.env.id != re.env.*.?.env.id) return false;
            // Ctx.pp(ctx.typeContext, a);
            return Match.Comparator.eql(.{ .typeContext = tyc }, le.env.*.?.match, re.env.*.?.match);
        }
    }
};
pub const NumRef = struct {
    id: usize,

    pub fn print(eid: @This(), c: Ctx) void {
        c.typeContext.getNum(eid).print(c);
    }

    pub fn eq(l: NumRef, r: NumRef, tc: *const TypeContext) bool {
        const lnum = tc.getNum(l);
        const rnum = tc.getNum(r);
        switch (lnum) {
            .TNum => |ltnum| {
                switch (rnum) {
                    .TNum => |rtnum| return ltnum.uid == rtnum.uid,
                    else => unreachable,
                }
            },
            .Literal => |llit| {
                switch (rnum) {
                    .Literal => |rlit| return llit == rlit,
                    else => unreachable,
                }
            },
            .Unknown => unreachable,
        }
    }
};
pub const TyVar = struct {
    uid: Unique,

    pub fn print(self: @This(), c: Ctx) void {
        c.sp("#{}", .{self.uid});

        const mFields = c.typeContext.getFieldsForTVar(self);

        if (mFields) |tvs| {
            std.debug.assert(tvs.fields.len > 0);
            c.encloseSepBy(tvs.fields, ", ", " { ", " }");
        }
    }

    pub fn comparator() type {
        return struct {
            pub fn eql(ctx: @This(), a: TyVar, b: TyVar) bool {
                _ = ctx;
                return a.uid == b.uid;
            }

            pub fn hash(ctx: @This(), k: TyVar) u64 {
                _ = ctx;
                return k.uid;
            }
        };
    }
};
pub const TVar = struct {
    uid: Unique,
    name: Str,
    binding: ?Binding, // null value for placeholder values.
    inferred: bool, // funny. it means if this tvar was made from a tyvar.
    fields: []Record,

    pub fn comparator() type {
        return struct {
            pub fn eql(ctx: @This(), a: TVar, b: TVar) bool {
                _ = ctx;
                return a.uid == b.uid;
            }

            pub fn hash(ctx: @This(), k: TVar) u64 {
                _ = ctx;
                return k.uid;
            }
        };
    }

    pub fn eq(l: @This(), r: @This()) bool {
        return l.uid == r.uid;
    }

    pub fn print(self: @This(), c: Ctx) void {
        c.sp("{s}#{}", .{ self.name, self.uid });
        if (self.fields.len > 0) {
            c.encloseSepBy(self.fields, ", ", " {", "}");
        }
    }
};

pub const Binding = union(enum) {
    Data: Unique,
    Function: Unique,
    ClassFunction: Unique,
};

pub fn TypeF(comptime a: ?type) type {
    return union(enum) {
        const Rec = a orelse *@This();
        pub const Field = struct {
            t: Rec,
            field: Str,

            pub fn print(self: @This(), c: Ctx) void {
                c.print(.{ self.field, ": ", self.t });
            }

            pub fn anonEq(ls: []Field, rs: []Field, tc: *const TypeContext) bool {
                if (ls.len != rs.len) return false;

                for (rs) |f2| {
                    // assume deduplicated.
                    for (ls) |f1| {
                        if (common.streq(f2.field, f1.field)) {
                            if (!Type.tyEq(f1.t, f2.t, tc)) return false;
                            break;
                        }
                    } else {
                        return false;
                    }
                }

                for (ls) |f1| {
                    // assume deduplicated.
                    for (rs) |f2| {
                        if (common.streq(f2.field, f1.field)) {
                            if (!Type.tyEq(f1.t, f2.t, tc)) return false;
                            break;
                        }
                    } else {
                        return false;
                    }
                }

                return true;
            }
        };

        Con: TypeApplication,
        Fun: Fun,
        TVar: TVar,
        TyVar: TyVar,
        Anon: []Field,

        pub const Fun = struct {
            args: []Rec,
            ret: Rec,
            env: EnvRef,

            pub const Comparator = struct {
                typeContext: *const TypeContext,

                pub fn eql(ctx: @This(), l: Fun, r: Fun) bool {
                    if (!l.ret.tyEq(r.ret, ctx.typeContext)) return false;
                    for (l.args, r.args) |ll, rr| {
                        if (!ll.tyEq(rr, ctx.typeContext)) return false;
                    }
                    if (!EnvRef.envEq(l.env, r.env, ctx.typeContext)) return false;

                    return true;
                }

                pub fn hash(ctx: @This(), k: Fun) u64 {
                    _ = ctx;
                    _ = k;
                    // TEMP, because we want to test equality.
                    return 0;
                }
            };
        };

        fn print(self: @This(), c: Ctx) void {
            switch (self) {
                .Anon => |fields| {
                    c.encloseSepBy(fields, ", ", "{ ", " }");
                },
                .Con => |con| {
                    if (con.application.tvars.len > 0 or con.application.envVars.len > 0 or con.outerApplication.len > 0) {
                        c.s("(");
                        con.type.print(c);
                        if (con.application.tvars.len > 0) {
                            c.s(" ");
                            c.sepBy(con.application.tvars, " ");
                        }
                        // if (con.application.envVars.len > 0) {
                        //     c.s(" |] ");
                        //     c.sepBy(con.application.envVars, " ");
                        // }
                        // if (con.outerApplication.len > 0) {
                        //     c.s(" |> ");
                        //     c.sepBy(con.outerApplication, " ");
                        // }
                        c.s(")");
                    } else {
                        con.type.print(c);
                    }
                },

                .Fun => |fun| {
                    c.encloseSepBy(fun.args, ", ", "(", ")");
                    // fun.env.print(c); // TEMP
                    c.s(" -> ");
                    fun.ret.print(c);
                },

                .TyVar => |tyv| {
                    tyv.print(c);
                },

                .TVar => |tv| {
                    if (c.mapTypes) |m| {
                        if (m.mapTVar(tv)) |t| {
                            t.print(c);
                        } else {
                            tv.print(c);
                        }
                    } else {
                        tv.print(c);
                    }
                },
                // else => unreachable,
            }
        }
    };
}
pub const TypeApplication = struct {
    type: *const Data,
    application: *const Match,
    outerApplication: []TypeOrNum,

    pub const Comparator = struct {
        typeContext: *const TypeContext,

        pub fn eql(ctx: @This(), a: TypeApplication, b: TypeApplication) bool {
            if (a.type.uid != b.type.uid) return false;
            if (!Match.Comparator.eql(.{ .typeContext = ctx.typeContext }, a.application, b.application)) return false;

            for (a.outerApplication, b.outerApplication) |l, r| {
                _ = r;
                switch (l) {
                    .Num => unreachable,
                    .Type => unreachable,
                }
            }

            return true;
        }

        pub fn hash(ctx: @This(), k: TypeApplication) u64 {
            _ = ctx;
            _ = k;
            // TEMP, because we want to test equality.
            return 0;
        }
    };
};

pub const Var = struct {
    name: Str,
    uid: Unique,

    pub fn print(v: @This(), c: Ctx) void {
        c.s(v.name);
        c.sp("{}", .{v.uid}); // ZIG BUG(?): there was a thing, where the stack trace was pointing to this place as an error, but actually it was in TyRef.
    }

    pub fn comparator() type {
        return struct {
            pub fn eql(ctx: @This(), a: Var, b: Var) bool {
                _ = ctx;
                return a.uid == b.uid;
            }

            pub fn hash(ctx: @This(), k: Var) u64 {
                _ = ctx;
                return k.uid;
            }
        };
    }
};

pub const TypeSynonym = struct {
    // we don't even need a name bruv ㅠㅠㅠ
    uid: Unique, // for tvar bindings.
    scheme: Scheme,
    t: Type,
};

pub const Data = struct {
    uid: Unique,
    name: Str,

    scheme: Scheme,
    outerTVars: []TVarOrNum,
    stuff: union(enum) {
        cons: []Con,
        recs: []Record,
    },
    annotations: []Annotation, // TODO: we may later check if annotations are valid for each thing they are assigned to.

    pub fn eq(l: *const @This(), r: *const @This()) bool {
        return l.uid == r.uid;
    }

    pub fn print(self: *const @This(), c: Ctx) void {
        c.sp("{s}@{}", .{ self.name, self.uid });
    }

    pub const StructureType = enum {
        Opaque,
        EnumLike,
        RecordLike,
        ADT,
    };
    pub fn structureType(self: *const @This()) StructureType {
        const cons = switch (self.stuff) {
            .cons => |cons| b: {
                if (cons.len == 0) {
                    return .Opaque;
                }
                break :b cons;
            },
            .recs => |fields| {
                // empty datatypes should become "constructor-like"
                std.debug.assert(fields.len > 0);
                return .RecordLike;
            },
        };

        var noTys = true;
        for (cons) |c| {
            noTys = noTys and c.tys.len == 0;
        }

        if (noTys) {
            return .EnumLike;
        } else if (cons.len == 1) {
            return .RecordLike;
        } else {
            return .ADT;
        }
    }

    pub fn isPointer(self: *const @This()) bool {
        return Annotation.find(self.annotations, "actual-pointer-type") != null;
    }
};
pub const Record = TypeF(Type).Field;

pub const Scheme = struct {
    tvars: []TVarOrNum,
    // tnums: []TNum, // TODO: tnubs :) I think it's better because we don't do weird casts.
    envVars: []EnvRef, // like unions. same environments can appear in different places, and they need to be the same thing.

    associations: []Association,
    env: ?*Env,

    pub const SchemeEnv = struct {
        ref: EnvRef,
        numatch: ?*const Match,

        pub fn print(self: @This(), c: Ctx) void {
            self.ref.print(c);
            if (self.numatch) |numatch| {
                c.print(.{" :: "});
                c.print(.{numatch});
            }
        }
    };
    pub fn empty() @This() {
        return .{
            .tvars = &.{},
            .envVars = &.{},
            .associations = &.{},
            .env = null,
        };
    }

    pub const Empty = empty();

    pub fn print(self: @This(), c: Ctx) void {
        c.s("{");
        c.sepBy(self.tvars, ", ");
        c.s("|");
        c.sepBy(self.envVars, ", ");
        c.s("|");
        c.sepBy(self.associations, ", ");
        c.s("}");
    }
};

pub const TVarOrNum = union(enum) {
    TVar: TVar,
    TNum: TNum,

    pub fn comparator() type {
        return struct {
            pub fn eql(ctx: @This(), a: TVarOrNum, b: TVarOrNum) bool {
                _ = ctx;
                return switch (a) {
                    .TVar => |tv1| switch (b) {
                        .TVar => |tv2| tv1.eq(tv2),
                        else => false,
                    },
                    .TNum => |tnum1| switch (b) {
                        .TNum => |tnum2| tnum1.uid == tnum2.uid,
                        else => false,
                    },
                };
            }

            pub fn hash(ctx: @This(), k: TVarOrNum) u64 {
                _ = ctx;
                return switch (k) {
                    .TVar => |tv| tv.uid,
                    .TNum => |tnum| tnum.uid * 1337,
                };
            }
        };
    }

    fn print(self: @This(), c: Ctx) void {
        switch (self) {
            .TVar => |tv| tv.print(c),
            .TNum => |n| n.print(c),
        }
    }

    fn binding(self: @This()) Binding {
        return switch (self) {
            .TVar => |tv| tv.binding,
            .TNum => |num| num.binding,
        };
    }
};

pub const TNum = struct {
    uid: Unique,
    name: Str,
    binding: ?Binding,

    pub fn print(self: @This(), c: Ctx) void {
        c.print(.{ "^", self.name, "#", self.uid });
    }

    pub fn asVar(self: @This()) Var {
        return .{ .name = self.name, .uid = self.uid };
    }
};

// Explanation: since we don't have unions, do we still need to do associations?
// Kind of. Because unifying environments can fail (the class function has NO ENVS), we must check the whole function type, so something like this is actually easier gegeg.
// Also, I'm not planning to do real associated types YET, they will work the same as in kc.
pub const Association = struct {
    depends: TVar,
    class: *Class,
    uid: ID,
    default: ?*Data,

    // when it's null, it's just a `constraint` and not based on a class function call.
    // when it's a value, it's an actual association with an associated function call.
    concrete: ?struct {
        to: Type,
        classFun: *ClassFun,
        ref: InstFunInst,

        env: ?EnvFun, // when ~instantiating the scheme~ this is special :3 (fukkk i dont know how to explain it.)

        // NOTE: kinda bad, used only for ClassFunctions
        match: *const Match,
    },

    pub const ID = Unique;

    fn print(self: @This(), c: Ctx) void {
        if (self.concrete) |conc| {
            c.s("(");
            self.depends.print(c);
            c.s(" => ");
            conc.to.print(c);
            c.print(.{ " :$", conc.classFun });
            c.s(")");
        } else {
            c.print(.{ self.depends, ": ", self.class });
        }
    }
};
pub const Match = struct {
    scheme: Scheme,
    tvars: []TypeOrNum,
    envVars: []EnvRef,
    assocs: []?AssocRef, // null to check for errors. normally, by the end of parsing, it must not be "undefined".
    // ALSO, a null is here when an assoc is not concrete!

    pub const AssocRef = union(enum) {
        InstFun: InstPair, // some top level association.
        Id: Association.ID, // this one means the assoc is contained in some Scheme.

        // I FUCKING HATE THESE NAMES
        pub const InstPair = struct { fun: *Function, m: *Match, locality: Locality };
    };

    pub fn joinScheme(self: *const @This(), scheme: *const Scheme, tc: *TypeContext, al: std.mem.Allocator) !*const @This() {
        const s = Scheme{
            .tvars = try std.mem.concat(al, TVarOrNum, &.{
                self.scheme.tvars,
                scheme.tvars,
            }),
            .envVars = try std.mem.concat(al, EnvRef, &.{
                self.scheme.envVars,
                scheme.envVars,
            }),
            .associations = try std.mem.concat(al, Association, &.{
                self.scheme.associations,
                scheme.associations,
            }),
            .env = if (scheme.env) |env| env else scheme.env,
        };

        var tvars = std.ArrayList(TypeOrNum).init(al);
        try tvars.appendSlice(self.tvars);
        for (scheme.tvars) |tom| {
            switch (tom) {
                .TVar => |tv| {
                    try tvars.append(.{ .Type = try tc.newType(.{ .TVar = tv }) });
                },
                .TNum => |tnum| {
                    try tvars.append(.{ .Num = try tc.newNum(.{ .TNum = tnum }) });
                },
            }
        }

        var envVars = std.ArrayList(EnvRef).init(al);
        try envVars.appendSlice(self.envVars);
        for (scheme.envVars) |ev| {
            try envVars.append(ev);
        }

        var assocs = std.ArrayList(?AssocRef).init(al);
        try assocs.appendSlice(self.assocs);
        for (scheme.associations) |assoc| {
            if (assoc.concrete != null) {
                try assocs.append(.{ .Id = assoc.uid });
            } else {
                try assocs.append(null);
            }
        }

        return try common.allocOne(al, @This(){
            .scheme = s,
            .tvars = tvars.items,
            .envVars = envVars.items,
            .assocs = assocs.items,
        });
    }

    pub fn print(self: *const @This(), oc: Ctx) void {
        if (self == oc.lastMatch) {
            oc.s("(X)");
            return;
        }

        var c = oc;
        c.lastMatch = self;

        c.s("<(");
        if (self.tvars.len > 0) {
            c.sepBy(self.tvars, " ");
        }
        if (self.envVars.len > 0) {
            c.s(" |] ");
            c.sepBy(self.envVars, " ");
        }
        if (self.assocs.len > 0) {
            c.s(" |> ");
            for (self.assocs, 0..) |maref, i| {
                if (i >= 1) {
                    c.print(" ");
                }
                if (maref) |aref| {
                    switch (aref) {
                        .InstFun => |ifn| {
                            c.print(.{ "[", ifn.fun.name, " ", ifn.m, "]" });
                        },
                        .Id => |iid| c.print(iid),
                    }
                } else {
                    c.print("[X]");
                }
            }
            // c.sepBy(self.assocs, " ");
        }
        c.s(")>");
    }

    pub fn fromOuterTVars(outerTVars: []TVarOrNum, outerApplication: []TypeOrNum) Match {
        return .{
            .scheme = Scheme{
                .tvars = outerTVars,
                .envVars = &.{},
                .associations = &.{},
                .env = null,
            },

            .tvars = outerApplication,
            .envVars = &.{},
            .assocs = &.{},
        };
    }

    pub fn mapTVar(self: *const @This(), tvar: TVar) ?Type {
        for (self.scheme.tvars, self.tvars) |tvOrNum, t| {
            switch (tvOrNum) {
                .TVar => |tv| {
                    if (tv.eq(tvar)) {
                        return t.Type;
                    }
                },

                .TNum => {},
            }
        }

        return null;
    }

    pub fn mapTNum(self: *const @This(), tnum: TNum) ?NumRef {
        for (self.scheme.tvars, self.tvars) |s, m| {
            switch (s) {
                .TNum => |tn| {
                    if (tn.uid == tnum.uid) {
                        return m.Num;
                    }
                },

                else => {},
            }
        }

        return null;
    }

    pub fn mapEnv(self: *const @This(), base: EnvRef) ?EnvRef {
        for (self.scheme.envVars, self.envVars) |s, m| {
            if (base.id == s.id) {
                return m;
            }
        }

        return null;
    }

    pub fn tryGetFunctionByID(self: *const @This(), uid: Association.ID) ?Match.AssocRef.InstPair {
        for (self.scheme.associations, self.assocs) |a, r| {
            if (a.uid == uid) {
                return switch (r.?) {
                    .Id => null,
                    .InstFun => |instfun| instfun,
                };
            }
        } else {
            return null;
        }
    }

    pub fn getFunctionOrIDByID(self: *const @This(), uid: Association.ID) ?Match.AssocRef {
        for (self.scheme.associations, self.assocs) |a, r| {
            if (a.uid == uid) {
                return r.?;
            }
        } else {
            return null;
        }
    }

    pub const Empty = empty(Scheme.Empty);
    pub fn empty(scheme: Scheme) @This() {
        // sanity check. right now only used for placeholders in case of errors.
        if (scheme.tvars.len > 0) {
            unreachable;
        }
        return .{
            .scheme = scheme,
            .tvars = &.{},
            .envVars = &.{},
            .assocs = &.{},
        };
    }

    pub const Comparator = struct {
        typeContext: *const TypeContext,

        pub fn eql(ctx: @This(), a: *const Match, b: *const Match) bool {
            for (a.tvars, b.tvars) |ltv, rtv| {
                switch (ltv) {
                    .Type => |lt| if (!lt.tyEq(rtv.Type, ctx.typeContext)) return false,
                    .Num => |ln| if (!ln.eq(rtv.Num, ctx.typeContext)) return false,
                }
            }

            for (a.envVars, b.envVars) |leRef, reRef| {
                // ENV SHOULD BE THE SAME SIZE (thats what I'm )
                if (!EnvRef.envEq(leRef, reRef, ctx.typeContext)) return false;
            }

            for (a.assocs, b.assocs) |mla, mra| {
                if (mla) |la| {
                    const ra = mra.?;
                    switch (la) {
                        .InstFun => |lifn| {
                            const rifn = ra.InstFun;
                            if (!Var.comparator().eql(.{}, lifn.fun.name, rifn.fun.name)) return false;
                            if (!Match.Comparator.eql(ctx, lifn.m, rifn.m)) return false;
                        },
                        .Id => unreachable,
                    }
                } else {
                    std.debug.assert(mra == null);
                }
            }

            return true;
        }

        pub fn hash(ctx: @This(), k: *const Match) u64 {
            _ = ctx;
            _ = k;
            // TODO: pointless doing it by len, because the Match Sets will be instantiations of the same scheme. do real hash.
            // ALSO OBV TEMP, because we want to test equality.
            return 0;
        }
    };
};

pub const TypeOrNum = union(enum) {
    Type: Type,
    Num: NumRef,

    pub const TyNum = union(enum) {
        TNum: TNum,
        Literal: i64,
        Unknown,

        pub fn print(self: @This(), c: Ctx) void {
            switch (self) {
                .TNum => |tnum| tnum.print(c),
                .Literal => |lit| c.print(lit),
                .Unknown => c.print("#?"),
            }
        }
    };

    pub fn print(self: @This(), c: Ctx) void {
        switch (self) {
            .Type => |t| t.print(c),
            .Num => |n| switch (c.typeContext.getNum(n)) {
                .TNum => |tnum| tnum.print(c),
                .Literal => |x| c.print(x),
                .Unknown => c.print("#/#"),
            },
        }
    }

    pub fn isNum(self: @This()) bool {
        return switch (self) {
            .Num => true,
            else => false,
        };
    }
};

pub const Con = struct {
    uid: Unique,
    name: Str,
    tys: []Type,
    data: *Data,

    tagValue: u32,
    anns: []Annotation,

    fn print(self: *const @This(), c: Ctx) void {
        c.s(self.name);
        // if (self.tys.len > 0) {
        //     c.s(" ");
        //     c.sepBy(self.tys, " ");
        // }
    }
};

pub const Class = struct {
    uid: Unique,
    name: Str,
    classFuns: []*ClassFun,
    selfType: TVar,
    default: ?*Data,
    level: usize,

    pub fn print(self: *const @This(), c: Ctx) void {
        c.print(.{ self.name, "@", self.uid });
    }
};

pub const ClassFun = struct {
    uid: Unique,
    name: Var,
    params: []Param,
    ret: Type,
    scheme: Scheme,
    self: Type, // or TVar?
    class: *Class,

    pub const Param = struct {
        t: Type,
    };

    pub fn print(self: @This(), c: Ctx) void {
        c.print(.{self.name});
    }
};

pub const Instance = struct {
    uid: Unique,
    class: *Class,
    data: *Data,
    level: usize,

    instFuns: []InstFun,
    pub const InstFun = struct {
        fun: *Function,
        classFunId: Unique,
    };

    fn print(self: @This(), c: Ctx) void {
        c.sp("inst {s} {s}", .{ self.class.name, self.data.name });
        c.s("\n");

        var ic = c;
        ic.indent +%= 1;
        for (self.instFuns) |instFun| {
            ic.sp("# class fun uid: {}", .{instFun.classFunId});
            ic.s("\n");
            instFun.fun.print(ic);
        }
    }
};

pub const ExternalFunction = struct {
    name: Var,
    params: []Param, // kinda cringe, but whatever. I assume tvars won't come through.
    ret: Type,

    scheme: Scheme,

    anns: []Annotation,

    pub const Param = struct {
        pn: Var,
        pt: Type,

        fn print(param: @This(), c: Ctx) void {
            param.pn.print(c);
            c.s(" ");
            param.pt.print(c);
        }
    };
};

pub const Annotation = struct {
    name: Str,
    params: []Str,

    pub fn find(anns: []Annotation, name: Str) ?Annotation {
        for (anns) |ann| {
            if (common.streq(ann.name, name)) {
                return ann;
            }
        }

        return null;
    }
};

//////////////////////////////////

// pub fn Cata(comptime C: type, comptime checkStmt: fn (*C, *Stmt) void, comptime checkExpr: fn (*C, *Expr) void, comptime checkType: fn (*C, *TyRef) void) type {
//     return struct {
//         ctx: *C,

//         // TODO: for later adaptation, change void to custom type.
//         pub fn cataExpr(self: *@This(), expr: *Expr) void {
//             checkType(self.ctx, expr.t);
//             switch (expr.e) {
//                 .Var => {},
//                 .Con => {},
//                 .Int => {},
//                 .Float => {},
//                 .Char => {},
//                 .Str => {},
//                 .Intrinsic => |intr| {
//                     for (intr.args) |arg| {
//                         self.cataExpr(arg);
//                     }
//                 },
//                 .BinOp => |bop| {
//                     self.cataExpr(bop.l);
//                     self.cataExpr(bop.r);
//                 },
//                 .UnOp => |uop| {
//                     self.cataExpr(uop.e);
//                     switch (uop.op) {
//                         .Update => |upd| {
//                             for (upd) |ex| {
//                                 self.cataExpr(ex.value);
//                             }
//                         },
//                         .As => |t| {
//                             self.cataType(t);
//                         },
//                         else => {},
//                     }
//                 },
//                 .Call => |call| {
//                     self.cataExpr(call.callee);
//                     for (call.args) |arg| {
//                         self.cataExpr(arg);
//                     }
//                 },

//                 .AnonymousRecord => |fields| {
//                     for (fields) |field| {
//                         self.cataExpr(field.value);
//                     }
//                 },
//                 .NamedRecord => |nrec| {
//                     for (nrec.fields) |field| {
//                         self.cataExpr(field.value);
//                     }
//                 },
//                 .Lam => |l| {
//                     for (l.body)kk
//                 },
//                 .StaticArray => |arr| {
//                     c.encloseSepBy(arr, ", ", "[", "]");
//                 },
//                 .IfElse => |ifelse| {
//                     c.print(.{ "if ", ifelse.cond, ": ", ifelse.ifTrue });
//                     for (ifelse.ifOthers) |elif| {
//                         c.print(.{ " elif ", elif.cond, ": ", elif.then });
//                     }
//                     c.print(.{ " else: ", ifelse.ifFalse });
//                 },
//                 .CaseExpr => |caseexpr| {
//                     c.print(.{ "case ", caseexpr.switchOn, "{\n" });

//                     var ic = c;
//                     ic.indent +%= 1;

//                     for (caseexpr.cases) |case| {
//                         case.print(ic);
//                     }

//                     c.print("}");
//                 },
//             }

//             checkExpr(ctx, self);
//         }
//     };
// }
