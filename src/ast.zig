const Str = @import("common.zig").Str;
const std = @import("std");
const Unique = @import("UniqueGen.zig").Unique;

declarations: []Declaration, // top level

const Ctx = struct {
    indent: u32,
    hadNewline: *bool, // check newlines and automatically indent

    const Self = @This();
    fn s(self: Self, ss: Str) void {
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
};

// NOTE: right now, use debug statements
pub fn print(self: @This()) void {
    var hadNewline = true;
    const c = Ctx{ .indent = 0, .hadNewline = &hadNewline };
    for (self.declarations) |dec| {
        dec.print(c);
    }
}

pub const Declaration = union(enum) {
    Function: Function,
    Constant: struct { name: Str, value: Expr },

    fn print(self: @This(), c: Ctx) void {
        switch (self) {
            .Function => |fnd| fnd.print(c),
            .Constant => unreachable,
        }
    }
};

pub const Function = struct {
    name: Str,
    params: []Param,
    ret: ?*Type,
    body: []*Stmt,

    pub const Param = struct { pn: Var, pt: ?*Type };

    fn print(self: @This(), c: Ctx) void {
        c.sp("{s} (", .{self.name});
        for (self.params) |param| {
            param.pn.print(c);
            if (param.pt) |t| {
                c.s(" ");
                t.print(c);
            }
        }
        c.s(")");
        if (self.ret) |r| {
            c.s(" ");
            r.print(c);
        }
        c.s("\n");

        printBody(self.body, c);
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
        }
    }
};

pub const Expr = union(enum) {
    BinOp: struct { l: Rec, op: BinOp, r: Rec },
    Var: Var,
    Int: i64, // obv temporary.

    const Rec = *@This();

    fn print(self: @This(), c: Ctx) void {
        switch (self) {
            .Var => |v| {
                v.print(c);
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
        }
    }
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

    fn print(self: @This(), c: Ctx) void {
        const sop = switch (self) {
            .Plus => "+",
            .Times => "*",
            else => "XXX",
        };

        c.s(sop);
    }
};
pub const Type = union(enum) {
    Con: struct { typename: Str, application: []Rec },
    Function: struct { args: []Rec, ret: Rec },
    TVar: Str,

    const Rec = *@This();

    fn print(self: @This(), c: Ctx) void {
        switch (self) {
            .Con => |con| {
                c.s(con.typename);

                if (con.application.len > 0) {
                    c.s("(");
                    for (con.application) |t| {
                        t.print(c);
                        c.s(" ");
                    }
                    c.s(")");
                }
            },
            else => unreachable,
        }
    }
};

pub const Var = struct {
    name: Str,
    uid: Unique,

    fn print(v: @This(), c: Ctx) void {
        c.s(v.name);
        c.sp("{}", .{v.uid});
    }
};
