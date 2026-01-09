const Str = @import("common.zig").Str;
const std = @import("std");
const Unique = @import("UniqueGen.zig").Unique;
const TypeContext = @import("TypeContext.zig");
const common = @import("common.zig");
const Intrinsic = @import("Intrinsic.zig");
const Loc = common.Location;

toplevel: []*Stmt, // top level

pub const Ctx = struct {
    indent: u32,
    hadNewline: *bool, // check newlines and automatically indent
    typeContext: *const TypeContext,

    const Self = @This();
    pub fn init(hadNewline: *bool, typeContext: *const TypeContext) Self {
        hadNewline.* = true;
        return Ctx{
            .indent = 0,
            .hadNewline = hadNewline,
            .typeContext = typeContext,
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

    pub fn Iter(itt: type, sept: type) type {
        return struct {
            it: itt,
            sep: sept,

            pub fn print(self: *const @This(), c: Ctx) void {
                var it = self.it;
                while (it.next()) |x| {
                    x.key_ptr.*.print(c); // TODO: when needed, make it more general. (eg: take in a function.)
                    c.s(self.sep);
                }
            }
        };
    }
    pub fn iter(it: anytype, sep: anytype) Iter(@TypeOf(it), @TypeOf(sep)) {
        return Iter(@TypeOf(it), @TypeOf(sep)){
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

    fn sepBy(self: Self, args: anytype, sep: Str) void {
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
    name: Var,
    params: []*Decon,
    ret: Type,
    body: []*Stmt,
    scheme: Scheme,
    env: Env,

    fn print(self: @This(), c: Ctx) void {
        c.sp("{s} (", .{self.name.name});
        c.sepBy(self.params, ", ");
        c.s(")[");
        c.sepBy(self.env, ", ");
        c.s("] -> ");
        self.ret.print(c);
        c.s(" ## ");
        self.scheme.print(c);
        c.s("\n");

        printBody(self.body, c);
    }
};

pub const Env = []VarInst;
pub const VarInst = struct {
    v: union(enum) {
        Fun: *Function,
        ClassFun: struct { cfun: *ClassFun, ref: InstFunInst },
        Var: Var,
        TNum: TNum,
    },
    m: *Match,
    t: Type,

    pub fn getVar(self: @This()) Var {
        return switch (self.v) {
            .Fun => |fun| fun.name,
            .ClassFun => |cfun| cfun.cfun.name,
            .Var => |v| v,
        };
    }

    fn print(self: @This(), c: Ctx) void {
        switch (self.v) {
            .TNum => |tnum| {
                tnum.print(c);
            },

            .Fun => |fun| {
                c.s("@"); // tag that it's a function instantiation.
                fun.name.print(c);
            },

            .ClassFun => |cfun| {
                c.s("$");
                cfun.cfun.name.print(c);
            },

            .Var => |v| {
                v.print(c);
            },
        }

        c.s(" ");
        self.t.print(c);
    }
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
    VarMut: struct { varRef: Var, accessors: []Accessor, varValue: *Expr },
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
    Switch: struct {
        switchOn: *Expr,
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

    fn print(self: @This(), c: Ctx) void {
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
            l: []*Decon,
            r: ?struct {
                spreadVar: ?struct { v: Var, t: Type },
                r: []*Decon,
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
};

pub const Expr = struct {
    t: Type,
    l: Loc,
    e: union(enum) {
        BinOp: struct { l: Rec, op: BinOp, r: Rec },
        UnOp: struct { e: Rec, op: UnOp },
        Call: struct { callee: Rec, args: []Rec },
        Var: struct { v: VarType, match: *Match }, // NOTE: Match is owned here!
        Con: *Con,
        Intrinsic: struct { intr: Intrinsic, args: []Rec },
        Int: i64, // obv temporary.
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

    pub const Lam = struct {
        params: []*Decon,
        expr: Rec,
        env: Env,

        pub fn print(self: *const @This(), c: Ctx) void {
            c.print("fn ");
            c.encloseSepBy(self.params, ", ", "(", ")");
            c.encloseSepBy(self.env, ", ", "[", "]");
            c.print(.{ ": ", self.expr });
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
        Var: Var,
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
            .Int => |i| c.sp("{}", .{i}),
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
                    .As => |t| {
                        c.print(.{ uop.e, " as ", t });
                    },
                    .Not => {
                        c.print(.{ "not ", uop.e });
                    },
                    .Negate => {
                        c.print(.{ "-", uop.e });
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
        }
        c.s(" :: ");
        self.t.print(c);
        c.s(")");
    }
};

pub const UnOp = union(enum) {
    Ref,
    Deref,
    Access: Str,
    As: Type,
    Not,
    Negate,
};

pub const BinOp = union(enum) {
    Plus,
    Minus,
    Times,
    Divide,

    Equals: InstFunInst,
    NotEquals: InstFunInst,
    GreaterThan,
    LessThan,
    GreaterEqualThan,
    LessEqualThan,

    Or,
    And,

    // FAKE OPS
    // TODO: explain
    Call,
    PostfixCall,
    RecordAccess,
    Deref,
    As,

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
    id: usize,

    pub fn print(tid: @This(), c: Ctx) void {
        c.typeContext.getType(tid).print(c);
    }

    pub fn eq(l: @This(), r: @This()) bool {
        return l.id == r.id;
    }
};
pub const EnvRef = struct {
    id: usize,

    pub fn print(eid: @This(), c: Ctx) void {
        if (c.typeContext.getEnv(eid).env) |env| {
            c.encloseSepBy(env, ", ", "[", "]");
        } else {
            c.s("[X]");
        }
    }
};
pub const NumRef = struct {
    id: usize,

    pub fn print(eid: @This(), c: Ctx) void {
        c.typeContext.getNum(eid).print(c);
    }
};
pub const TyVar = struct {
    uid: Unique,

    pub fn print(self: @This(), c: Ctx) void {
        c.sp("#{}", .{self.uid});

        const mFields = c.typeContext.getFieldsForTVar(self);

        if (mFields) |fields| {
            std.debug.assert(fields.len > 0);
            c.encloseSepBy(fields, ", ", " { ", " }");
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
        };

        Con: struct { type: *Data, application: *Match },
        Fun: struct { args: []Rec, ret: Rec, env: EnvRef },
        TVar: TVar,
        TyVar: TyVar,
        Anon: []Field,

        fn print(self: @This(), c: Ctx) void {
            switch (self) {
                .Anon => |fields| {
                    c.encloseSepBy(fields, ", ", "{ ", " }");
                },
                .Con => |con| {
                    if (con.application.tvars.len > 0) {
                        c.s("(");
                        con.type.print(c);
                        c.s(" ");
                        c.sepBy(con.application.tvars, " ");
                        c.s(")");
                    } else {
                        con.type.print(c);
                    }
                },

                .Fun => |fun| {
                    c.encloseSepBy(fun.args, ", ", "(", ")");
                    // fun.env.print(c);  // TEMP
                    c.s(" -> ");
                    fun.ret.print(c);
                },

                .TyVar => |tyv| {
                    tyv.print(c);
                },

                .TVar => |tv| {
                    tv.print(c);
                },
                // else => unreachable,
            }
        }
    };
}

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

pub const Data = struct {
    uid: Unique,
    name: Str,

    scheme: Scheme,
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

    pub fn structureType(self: *const @This()) enum {
        Opaque,
        EnumLike,
        RecordLike,
        ADT,
    } {
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
};
pub const Record = TypeF(Type).Field;

pub const Scheme = struct {
    tvars: []TVarOrNum,
    // tnums: []TNum, // TODO: tnubs :) I think it's better because we don't do weird casts.
    envVars: []EnvRef, // like unions. same environments can appear in different places, and they need to be the same thing.
    associations: []Association,

    pub fn empty() @This() {
        return .{
            .tvars = &.{},
            .envVars = &.{},
            .associations = &.{},
        };
    }

    fn print(self: @This(), c: Ctx) void {
        c.s("{");
        c.sepBy(self.tvars, ", ");
        c.s("|");
        c.sepBy(self.associations, ", ");
        c.s("}");
    }
};

pub const TVarOrNum = union(enum) {
    TVar: TVar,
    TNum: TNum,

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

    fn print(self: @This(), c: Ctx) void {
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
    to: Type,
    classFun: *ClassFun,
    uid: ID,

    pub const ID = Unique;

    fn print(self: @This(), c: Ctx) void {
        c.s("(");
        self.depends.print(c);
        c.s(" => ");
        self.to.print(c);
        c.print(.{ " :$", self.classFun });
        c.s(")");
    }
};
pub const Match = struct {
    scheme: Scheme,
    envVars: []EnvRef,
    tvars: []TypeOrNum,
    assocs: []?AssocRef, // null to check for errors. normally, by the end of parsing, it must not be "undefined"

    pub const AssocRef = union(enum) {
        InstFun: InstPair, // some top level association.
        Id: Association.ID, // this one means the assoc is contained in some Scheme.

        // I FUCKING HATE THESE NAMES
        pub const InstPair = struct { fun: *Function, m: *Match };
    };

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

    pub fn mapEnv(self: *const @This(), base: EnvRef) ?EnvRef {
        for (self.scheme.envVars, self.envVars) |s, m| {
            if (base.id == s.id) {
                return m;
            }
        }

        return null;
    }

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
};

pub const TypeOrNum = union(enum) {
    Type: Type,
    Num: NumRef,

    pub const TyNum = union(enum) {
        TNum: TNum,
        Literal: i64,
        Unknown,
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
