const std = @import("std");
const ast = @import("ast.zig");
const Str = @import("common.zig").Str;
const Prelude = @import("Prelude.zig");
const Self = @This();

pub const VarAndType = struct { v: ast.Var, t: ast.Type };
pub const VarOrFun = union(enum) {
    Var: VarAndType,
    Fun: *ast.Function,
    ClassFun: *ast.ClassFun,
    Extern: *ast.ExternalFunction,
};

pub const DataOrClass = union(enum) {
    Data: *ast.Data,
    Class: *ast.Class,
};
pub const DataInstance = std.AutoArrayHashMap(*ast.Data, *ast.Instance);

pub const Exports = struct {
    vars: std.StringHashMap(VarOrFun),
    types: std.StringHashMap(DataOrClass),
    cons: std.StringHashMap(*ast.Con),
    instances: std.AutoHashMap(*ast.Class, DataInstance),
};

pub const Path = []Str;

ast: ast,
exports: Exports,

pub fn lookupVar(varName: Str) ?VarOrFun {
    _ = varName;
    unreachable;
}

pub fn lookupCon(conName: Str) ?*ast.Con {
    _ = conName;
    unreachable;
}

pub fn lookupData(self: *const Self, dataName: Str) ?DataOrClass {
    return self.exports.types.get(dataName);
}

// should be const, but stack.Fixed's iterator can return pointers. This can be fixed by using two different types.
pub fn mkPrelude(self: *const Self) !Prelude {
    var enums: [Prelude.NumPredefinedTypes]*ast.Data = .{undefined} ** Prelude.NumPredefinedTypes;
    var copy = Prelude.PremadeTypeName;
    var it = copy.iterator();
    while (it.next()) |kv| {
        if (self.lookupData(kv.value.*)) |dc| {
            switch (dc) {
                .Data => |d| enums[@intCast(@intFromEnum(kv.key))] = d,
                .Class => return error.PreludeError,
            }
        } else {
            return error.PreludeError;
        }
    }
    return .{
        .predefinedTypes = enums,
    };
}
