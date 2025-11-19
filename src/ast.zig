const Str = @import("common.zig").Str;
const std = @import("std");
const Unique = @import("UniqueGen.zig").Unique;
const TypeContext = @import("TypeContext.zig");

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
        ClassFun: *ClassFun,
        Var: Var,
    },
    t: Type,

    fn print(self: @This(), c: Ctx) void {
        switch (self.v) {
            .Fun => |fun| {
                c.s("@"); // tag that it's a function instantiation.
                fun.name.print(c);
            },

            .ClassFun => |cfun| {
                c.s("$");
                cfun.name.print(c);
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
    If: struct {
        cond: *Expr,
        bTrue: []Rec,
        bOthers: []Elif,
        bElse: ?[]Rec,
    },
    Return: *Expr,
    Function: *Function,
    Instance: *Instance,

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

pub const Expr = struct {
    t: Type,
    e: union(enum) {
        BinOp: struct { l: Rec, op: BinOp, r: Rec },
        UnOp: struct { e: Rec, op: UnOp },
        Access: struct { e: Rec, acc: Str },
        Call: struct { callee: Rec, args: []Rec },
        Var: Var,
        Fun: *Function,
        ClassFun: *ClassFun,
        Con: *Con,
        Int: i64, // obv temporary.
    },

    const Rec = *@This();

    fn print(self: @This(), c: Ctx) void {
        c.s("(");
        switch (self.e) {
            .Var => |v| {
                v.print(c);
            },
            .Fun => |fun| {
                fun.name.print(c);
            },
            .ClassFun => |cfun| {
                cfun.name.print(c);
            },
            .Con => |con| {
                con.print(c);
            },
            .Int => |i| c.sp("{}", .{i}),
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

    // TODO: explain
    Call,
    RecordAccess,

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
        if (c.typeContext.envContext.items[eid.id]) |env| {
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

        Con: struct { type: *Data, application: Match(Rec) },
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

    fn print(v: @This(), c: Ctx) void {
        c.s(v.name);
        c.sp("{}", .{v.uid}); // ZIG BUG(?): there was a thing, where the stack trace was pointing to this place as an error, but actually it was in TyRef.
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
};

pub const Scheme = struct {
    tvars: []TVar,
    associations: []Association,

    pub fn empty() @This() {
        return .{ .tvars = &.{}, .associations = &.{} };
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
    classFunId: Unique,

    fn print(self: @This(), c: Ctx) void {
        c.s("(");
        self.depends.print(c);
        c.s(" => ");
        self.to.print(c);
        c.sp(" :${}", .{self.classFunId});
        c.s(")");
    }
};
pub fn Match(comptime T: type) type {
    return struct {
        scheme: Scheme,
        tvars: []T,

        pub fn mapTVar(self: *const @This(), tvar: TVar) ?Type {
            for (self.scheme.tvars, self.tvars) |tv, t| {
                if (tv.eq(tvar)) {
                    return t;
                }
            }

            return null;
        }

        pub fn empty(scheme: Scheme) @This() {
            // sanity check. right now only used for placeholders in case of errors.
            if (scheme.tvars.len > 0) {
                unreachable;
            }
            return .{ .scheme = scheme, .tvars = &.{} };
        }
    };
}

pub const Con = struct {
    uid: Unique,
    name: Str,
    tys: []Type,
    data: *Data,

    fn print(self: *const @This(), c: Ctx) void {
        c.s(self.name);
        if (self.tys.len > 0) {
            c.s(" ");
            c.sepBy(self.tys, " ");
        }
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
