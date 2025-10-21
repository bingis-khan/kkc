const Str = @import("common.zig").Str;
const std = @import("std");

declarations: []Declaration, // top level

// NOTE: right now, use debug statements
pub fn print(self: @This()) void {
    for (self.declarations) |dec| {
        Declaration.print(dec);
    }
}

pub const Declaration = union(enum) {
    Function: Function,
    Constant: struct { name: Str, value: Expr },

    fn print(self: @This()) void {
        switch (self) {
            .Function => |fnd| Function.print(fnd),
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

    fn print(self: @This()) void {
        sp("{s} (", .{self.name});
        for (self.params) |param| {
            s(param.pn);
            if (param.pt) |t| {
                s(" ");
                t.print();
            }
        }
        s(")");
        if (self.ret) |r| {
            s(" ");
            r.print();
        }
        s("\n");

        for (self.body) |stmt| {
            s("  ");
            stmt.print();
            s("\n");
        }
    }
};

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

    fn print(self: @This()) void {
        switch (self) {
            .VarDec => |vd| {
                sp("{s} = ", .{vd.varName});
                vd.varValue.print();
            },
            .If => |ifstmt| {
                s("if ");
                ifstmt.cond.print();
            },
            .Return => |expr| {
                s("return ");
                expr.print();
            },
        }
    }
};

pub const Expr = union(enum) {
    BinOp: struct { l: Rec, op: Op, r: Rec },
    Var: Str,

    const Rec = *@This();

    fn print(self: @This()) void {
        switch (self) {
            .Var => |v| s(v),
            .BinOp => |bop| {
                s("(");
                bop.l.print();
                s(" ");
                bop.op.print();
                s(" ");
                bop.r.print();
                s(")");
            },
        }
    }
};
pub const Op = enum {
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

    fn print(self: @This()) void {
        const sop = switch (self) {
            else => "XXX",
        };

        s(sop);
    }
};
pub const Type = union(enum) {
    Con: struct { typename: Str, application: []Rec },
    Function: struct { args: []Rec, ret: Rec },
    TVar: Str,

    const Rec = *@This();

    fn print(self: @This()) void {
        switch (self) {
            .Con => |c| {
                s(c.typename);

                if (c.application.len > 0) {
                    s("(");
                    for (c.application) |t| {
                        t.print();
                        s(" ");
                    }
                    s(")");
                }
            },
            else => unreachable,
        }
    }
};

fn s(ss: Str) void {
    std.debug.print("{s}", .{ss});
}

const sp = std.debug.print;
