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

    pub fn print(self: @This()) void {
        switch (self) {
            .UndefinedVariable => |uv| std.debug.print("undefined variable {s}{} at ({}, {})\n", .{ uv.varname.name, uv.varname.uid, uv.loc.from, uv.loc.to }),
        }
    }
};
