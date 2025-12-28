const std = @import("std");
const ast = @import("ast.zig");
const Str = @import("common.zig").Str;
const Prelude = @import("Prelude.zig");
const Self = @This();
const common = @import("common.zig");

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

    pub fn print(self: *const @This(), cc: ast.Ctx) void {
        cc.s("Instances:\n");
        var c = cc;
        c.indent += 1;

        var it = self.instances.iterator();
        while (it.next()) |e| {
            c.print(.{ e.key_ptr.*, ":\n" });

            var ec = c;
            ec.indent += 1;

            var itt = e.value_ptr.iterator();
            while (itt.next()) |ee| {
                ec.print(.{ ee.key_ptr.*, ": ", ee.value_ptr.*.uid, "\n" });
            }
        }
        cc.s("End Instances\n");
    }
};

pub const Path = []const Str;
pub const PathCtx = struct {
    pub fn eql(ctx: @This(), a: Path, b: Path) bool {
        _ = ctx;

        if (a.len != b.len) return false;
        for (a, b) |sa, sb| {
            if (!common.streq(sa, sb)) return false;
        }

        return true;
    }

    pub fn hash(ctx: @This(), k: Path) u64 {
        _ = ctx;

        var cumhash: u64 = 1;
        for (k) |kk| {
            cumhash *%= std.hash_map.StringContext.hash(.{}, kk);
        }

        return cumhash;
    }
};
pub const BasePath = struct {
    isSTD: bool,
    path: Path,

    pub const Ctx = struct {
        pub fn eql(ctx: @This(), a: BasePath, b: BasePath) bool {
            _ = ctx;
            if (a.isSTD != b.isSTD) return false;
            return PathCtx.eql(.{}, a.path, b.path);
        }

        pub fn hash(ctx: @This(), k: BasePath) u64 {
            _ = ctx;
            return PathCtx.hash(.{}, k.path) *% @as(u64, if (k.isSTD) 69 else 2137);
        }
    };
}; // hack for loading from STDs

ast: ast,
exports: Exports,

pub fn lookupVar(self: *const Self, varName: Str) ?VarOrFun {
    return self.exports.vars.get(varName);
}

pub fn lookupCon(self: *const Self, conName: Str) ?*ast.Con {
    return self.exports.cons.get(conName);
}

pub fn lookupData(self: *const Self, dataName: Str) ?DataOrClass {
    return self.exports.types.get(dataName);
}

// should be const, but stack.Fixed's iterator can return pointers. This can be fixed by using two different types.
pub fn mkPrelude(self: *const Self) !Prelude {

    // types
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

    // classes
    var classEnums: [Prelude.NumPredefinedClasses]*ast.Class = .{undefined} ** Prelude.NumPredefinedClasses;
    var classNames = Prelude.PremadeClassName;
    var cit = classNames.iterator();
    while (cit.next()) |kv| {
        if (self.lookupData(kv.value.*)) |dc| {
            switch (dc) {
                .Class => |c| classEnums[@intCast(@intFromEnum(kv.key))] = c,
                .Data => return error.PreludeError,
            }
        } else {
            return error.PreludeError;
        }
    }
    return .{
        .predefinedTypes = enums,
        .predefinedClasses = classEnums,
    };
}
