const Str = @import("common.zig").Str;
const std = @import("std");
const Unique = @import("UniqueGen.zig").Unique;
const TypeContext = @import("TypeContext.zig");
const common = @import("common.zig");

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
    fn sp(self: Self, comptime fmt: []const u8, args: anytype) void {
        if (fmt.len > 0 and self.hadNewline.*) {
            self.sindent();
            self.hadNewline.* = false;
        }
        std.debug.print(fmt, args);
    }

    fn print(self: Self, args: anytype) void {
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
            if (comptime std.meta.hasMethod(field.type, "print")) {
                arg.print(self);
            } else {
                self.s(@as(Str, arg)); // IF THIS CAST FAILS, IT MEANS YOU MUST ADD `pub` TO YOUR `fn print()`
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
    params: []Param,
    ret: Type,
    body: []*Stmt,
    scheme: Scheme,
    env: Env,

    pub const Param = struct {
        pn: Var,
        pt: Type,

        fn print(param: @This(), c: Ctx) void {
            param.pn.print(c);
            c.s(" ");
            param.pt.print(c);
        }
    };

    fn print(self: @This(), c: Ctx) void {
        c.sp("{s} (", .{self.name.name});
        c.sepBy(self.params, ", ");
        c.s(")[");
        c.sepBy(self.env, ", ");
        c.s("] -> ");
        self.ret.print(c);
        c.s(" ");
        self.scheme.print(c);
        c.s("\n");

        printBody(self.body, c);
    }
};

pub const Env = []VarInst;
pub const VarInst = struct {
    v: union(enum) {
        Fun: *Function,
        ClassFun: struct { cfun: *ClassFun, ref: *?Match(Type).AssocRef },
        Var: Var,
    },
    m: *Match(Type),
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
    VarMut: struct { varRef: Var, refs: usize, varValue: *Expr },
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
    Function: *Function,
    Instance: *Instance,
    Expr: *Expr,

    const Rec = *@This();
    pub const Elif = struct { cond: *Expr, body: []Rec };

    fn print(self: @This(), c: Ctx) void {
        switch (self) {
            .VarDec => |vd| {
                vd.varDef.print(c);
                c.s(" = ");
                vd.varValue.print(c);
                c.s("\n");
            },
            .VarMut => |vm| {
                c.print(.{ vm.varRef, " <" });
                for (0..vm.refs) |_| {
                    c.s("&");
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
    d: union(enum) {
        None: struct {},
        Var: Var,
        Con: struct {
            con: *Con,
            decons: []*Decon,
        },
    },

    fn print(self: *const @This(), c: Ctx) void {
        switch (self.d) {
            .Var => |v| v.print(c),
            .Con => |con| {
                con.con.print(c);
                if (con.decons.len > 0) {
                    c.encloseSepBy(con.decons, ", ", "(", ")");
                }
            },
        }

        c.typed(self.t);
    }
};

pub const Expr = struct {
    t: Type,
    e: union(enum) {
        BinOp: struct { l: Rec, op: BinOp, r: Rec },
        UnOp: struct { e: Rec, op: UnOp },
        Access: struct { e: Rec, acc: Str },
        Call: struct { callee: Rec, args: []Rec },
        Var: struct { v: VarType, match: *Match(Type) }, // NOTE: Match is owned here!
        Con: *Con,
        Int: i64, // obv temporary.
        Str: struct { lit: Str, og: Str },
    },

    pub const VarType = union(enum) {
        Fun: *Function,
        ClassFun: struct { cfun: *ClassFun, ref: *?Match(Type).AssocRef }, // SMELL: this one I have to allocate, because I'm returning the whole struct from a function, so the address will change.
        Var: Var,
        ExternalFun: *ExternalFunction,

        // pub fn getVar(self: @This()) Var {
        //     return switch (self) {
        //         .Var => |v| v,
        //         .Fun => |fun| fun.name,
        //         else => undefined,
        //     };
        // }

        pub fn print(self: @This(), c: Ctx) void {
            switch (self) {
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
            .Str => |s| c.sp("'{s}'", .{s.og}),
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
                }
                c.s(")");
            },
            .Access => |acc| {
                acc.e.print(c);
                c.sp(".{s}", .{acc.acc});
            },
            .Call => |call| {
                call.callee.print(c);
                c.s("(");
                c.sepBy(call.args, ", ");
                c.s(")");
            },
        }
        c.s(" :: ");
        self.t.print(c);
        c.s(")");
    }
};

pub const UnOp = enum {
    Ref,
    Deref,
};

pub const BinOp = enum {
    Plus,
    Minus,
    Times,
    Divide,

    Equals,
    NotEquals,
    GreaterThan,
    LessThan,
    GreaterEqualThan,
    LessEqualThan,

    // FAKE OPS
    // TODO: explain
    Call,
    PostfixCall,
    RecordAccess,
    Deref,

    fn print(self: @This(), c: Ctx) void {
        const sop = switch (self) {
            .Plus => "+",
            .Times => "*",
            .Equals => "==",
            else => "XXX",
        };

        c.s(sop);
    }
};

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
pub const TyVar = struct {
    uid: Unique,
    classes: std.ArrayList(*Class),

    pub fn print(self: @This(), c: Ctx) void {
        c.sp("#{}", .{self.uid});
    }
};
pub const TVar = struct {
    uid: Unique,
    name: Str,
    binding: ?Binding, // null value for placeholder values.

    pub const Binding = union(enum) {
        Data: Unique,
        Function: Unique,
        ClassFunction: Unique,
    };

    pub fn eq(l: @This(), r: @This()) bool {
        return l.uid == r.uid;
    }

    pub fn print(self: @This(), c: Ctx) void {
        c.sp("{s}#{}", .{ self.name, self.uid });
    }
};
pub fn TypeF(comptime a: ?type) type {
    return union(enum) {
        const Rec = a orelse *@This();

        Con: struct { type: *Data, application: *Match(Rec) },
        Fun: struct { args: []Rec, ret: Rec, env: EnvRef },
        TVar: TVar,
        TyVar: TyVar,

        fn print(self: @This(), c: Ctx) void {
            switch (self) {
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
                    fun.env.print(c);
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
    cons: []Con,

    pub fn eq(l: *const @This(), r: *const @This()) bool {
        return l.uid == r.uid;
    }

    fn print(self: *const @This(), c: Ctx) void {
        c.sp("{s}@{}", .{ self.name, self.uid });
    }

    pub fn structureType(self: *const @This()) enum {
        Opaque,
        EnumLike,
        RecordLike,
        ADT,
    } {
        if (self.cons.len == 0) return .Opaque;

        var noTys = true;
        for (self.cons) |c| {
            noTys = noTys and c.tys.len == 0;
        }

        if (noTys) {
            return .EnumLike;
        } else if (self.cons.len == 1) {
            return .RecordLike;
        } else {
            return .ADT;
        }
    }
};

pub const Scheme = struct {
    tvars: []TVar,
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
        c.sp(" :${}", .{self.classFun.uid});
        c.s(")");
    }
};
// TODO: pointless T.
pub fn Match(comptime T: type) type {
    return struct {
        scheme: Scheme,
        envVars: []EnvRef,
        tvars: []T,
        assocs: []?AssocRef, // null to check for errors. normally, by the end of parsing, it must not be "undefined"

        pub const AssocRef = union(enum) {
            InstFun: InstPair, // some top level association.
            Id: Association.ID, // this one means the assoc is contained in some Scheme.

            // I FUCKING HATE THESE NAMES
            pub const InstPair = struct { fun: *Function, m: *Match(Type) };
        };

        pub fn mapTVar(self: *const @This(), tvar: TVar) ?Type {
            for (self.scheme.tvars, self.tvars) |tv, t| {
                if (tv.eq(tvar)) {
                    return t;
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
}

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
    params: []Function.Param, // kinda cringe, but whatever. I assume tvars won't come through.
    ret: Type,

    scheme: Scheme,

    anns: []Annotation,
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
