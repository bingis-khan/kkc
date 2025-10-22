const Str = @import("common.zig").Str;
const std = @import("std");

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
                        for (0..self.indent) |_| {
                            std.debug.print(" ", .{});
                        }
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
                for (0..self.indent) |_| {
                    std.debug.print(" ", .{});
                }
                self.hadNewline.* = false;
            }
            std.debug.print("{s}", .{ss[last_nl_i..ss.len]});
        }
    }

    // HACK: don't use newlines in format strings PLS
    fn sp(self: Self, comptime fmt: []const u8, args: anytype) void {
        if (fmt.len > 0 and self.hadNewline.*) {
            for (0..self.indent) |_| {
                std.debug.print(" ", .{});
            }
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

    pub const Param = struct { pn: Str, pt: ?*Type };

    fn print(self: @This(), c: Ctx) void {
        c.sp("{s} (", .{self.name});
        for (self.params) |param| {
            c.s(param.pn);
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
        for (0..c.indent) |_| {
            c.s(" ");
        }
        stmt.print(c);
        c.s("\n");
    }
}

pub const Stmt = union(enum) {
    VarDec: struct { varName: Str, varValue: *Expr },
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
                c.sp("{s} = ", .{vd.varName});
                vd.varValue.print(c);
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
            },
        }
    }
};

pub const Expr = union(enum) {
    BinOp: struct { l: Rec, op: BinOp, r: Rec },
    Var: Str,
    Int: i64, // obv temporary.

    const Rec = *@This();

    fn print(self: @This(), c: Ctx) void {
        switch (self) {
            .Var => |v| c.s(v),
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
