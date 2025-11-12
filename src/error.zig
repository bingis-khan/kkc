const std = @import("std");
const Common = @import("common.zig");
const Str = Common.Str;
const Loc = Common.Location;
const AST = @import("ast.zig");

pub const Error = union(enum) {
    UndefinedVariable: struct {
        varname: AST.Var,
        loc: Loc,
    },

    UndefinedType: struct {
        typename: Str,
        loc: Loc,
    },

    UndefinedCon: struct {
        conname: Str,
        loc: Loc,
    },

    UndefinedTVar: struct {
        tvname: Str,
        loc: Loc,
    },

    // TODO: add location information
    MismatchingTypes: struct {
        lt: AST.Type,
        rt: AST.Type,
    },

    MismatchingParamLen: struct {
        lpl: usize,
        rpl: usize,
    },

    fn p(comptime fmt: anytype, args: anytype) void {
        std.debug.print(fmt ++ "\n", args);
    }
    pub fn print(self: @This(), c: AST.Ctx) void {
        switch (self) {
            .UndefinedVariable => |uv| p("undefined variable {s}{} at ({}, {})", .{ uv.varname.name, uv.varname.uid, uv.loc.from, uv.loc.to }),
            .UndefinedCon => |e| p("undefined con {s} at ({}, {})", .{ e.conname, e.loc.from, e.loc.to }),
            .UndefinedType => |e| p("undefined type {s} at ({}, {})", .{ e.typename, e.loc.from, e.loc.to }),
            .UndefinedTVar => |e| p("undefined tvar {s} at ({}, {})", .{ e.tvname, e.loc.from, e.loc.to }),
            .MismatchingTypes => |e| {
                c.s("Mismatching types: ");
                e.lt.print(c); // UGLY
                c.s(" =/= ");
                e.rt.print(c);
                p("", .{}); // newline
            },
            .MismatchingParamLen => |e| p("Mismatching lengths: {} =/= {}", .{ e.lpl, e.rpl }),
        }
    }
};

pub const Errors = std.ArrayList(Error);
