const std = @import("std");
const ast = @import("ast.zig");
const Prelude = @import("Prelude.zig");
const common = @import("common.zig");
const Str = common.Str;

const Self = @This();

arena: std.mem.Allocator,
returnValue: Value = undefined,
scope: Scope,
funLoader: DyLibLoader,

const Scope = std.HashMap(ast.Var, Value, struct {
    pub fn eql(ctx: @This(), a: ast.Var, b: ast.Var) bool {
        _ = ctx;
        return a.uid == b.uid;
    }

    pub fn hash(ctx: @This(), k: ast.Var) u64 {
        _ = ctx;
        return k.uid;
    }
}, std.hash_map.default_max_load_percentage);

// right now a very simple interpreter where we don't free.
pub fn run(module: ast, prelude: Prelude, al: std.mem.Allocator) !i64 {
    _ = prelude;
    const scope = Scope.init(al);
    var self: Self = .{
        .scope = scope,
        .arena = al,
        .funLoader = DyLibLoader.init(al),
    };
    self.stmts(module.toplevel) catch |err| switch (err) {
        error.Return => {
            return self.returnValue.v.int;
        },
        else => return err,
    };
    return 0;
}

fn stmts(self: *Self, ss: []*ast.Stmt) !void {
    for (ss) |s| {
        try self.stmt(s);
    }
}

fn stmt(self: *Self, s: *ast.Stmt) !void {
    switch (s.*) {
        .Return => |e| {
            const v = try self.expr(e);
            self.returnValue = v;
            return error.Return;
        },
        .VarDec => |vd| {
            const v = try self.expr(vd.varValue);
            try self.scope.put(vd.varDef, v);
        },
        else => {
            unreachable;
        },
    }
}

fn expr(self: *Self, e: *ast.Expr) !Value {
    switch (e.e) {
        .Int => |x| {
            return Value{
                .t = e.t,
                .v = .{ .int = x },
            };
        },
        .BinOp => |op| {
            const l = try self.expr(op.l);
            const r = try self.expr(op.r);
            return switch (op.op) {
                .Plus => .{
                    .t = l.t,
                    .v = .{ .int = l.v.int + r.v.int },
                },
                else => unreachable,
            };
        },
        .Var => |v| {
            switch (v) {
                .Var => |vv| {
                    if (self.scope.get(vv)) |val| {
                        return val;
                    } else {
                        unreachable;
                    }
                },

                .ExternalFun => |extfun| {
                    const libName = if (ast.Annotation.find(extfun.anns, "dylib")) |ann| ann.params[0] else {
                        return error.ExpectDyLibAnnotation;
                    };
                    const funName = if (ast.Annotation.find(extfun.anns, "cfunname")) |cfunname|
                        cfunname.params[0]
                    else
                        extfun.name.name;

                    const fun = try self.funLoader.loadFunction(libName, funName);
                    return .{
                        .t = e.t,
                        .v = .{ .ptr = fun },
                    };
                },

                else => {
                    unreachable;
                },
            }
        },
        .Str => |slit| {
            return .{
                .t = e.t,
                .v = .{ .ptr = @ptrCast(try self.evaluateString(slit)) },
            };
        },
        .Call => |c| {
            const fun = try self.expr(c.callee);
            const castFun: *const fn (...) callconv(.C) void = @ptrCast(fun.v.ptr);

            const MaxExtArgs = 5;
            var interopArgs: std.meta.Tuple(&(.{i64} ** MaxExtArgs)) = undefined; // max external function call args: 16 (ideally, should be equal to C's max limit)
            inline for (0..MaxExtArgs) |i| {
                if (i < c.args.len) {
                    const av = try self.expr(c.args[i]);
                    interopArgs[i] = @as(i64, @bitCast(av.v));
                }
            }

            // const d: i64 = 4;
            // const dd: i64 = 11111;
            // const miau: Str = "cock";
            const ret = @call(.auto, castFun, interopArgs);
            _ = ret;

            return .{
                .t = e.t,
                .v = undefined,
            };
        },
        else => unreachable,
    }
}

fn evaluateString(self: *const Self, s: Str) ![:0]u8 {
    return try self.arena.dupeZ(u8, s);
}

// values n shit
// note, that we must preserve the inner representation for compatibility with external functions.
const Value = struct {
    const Type = extern union {
        int: i64,
        ptr: *anyopaque,
    };
    v: Type,
    t: ast.Type,
};

const DyLibLoader = struct {
    // also cache functions yo.
    libs: LibCache,
    const LibCache = std.StringHashMap(struct {
        lib: std.DynLib,
        funs: std.StringHashMap(*anyopaque),
    });

    fn init(al: std.mem.Allocator) @This() {
        return .{
            .libs = LibCache.init(al),
        };
    }

    fn loadFunction(self: *@This(), libName: Str, funName: Str) !*anyopaque {
        const al = self.libs.allocator; // use the same allocator for all the stuff (the lifetime is until interpreter quits anyway)
        const libEntry = self.libs.getPtr(libName) orelse b: {
            const newLib: std.DynLib = try std.DynLib.open(libName);
            try self.libs.put(funName, .{
                .lib = newLib,
                .funs = std.StringHashMap(*anyopaque).init(al),
            });
            break :b self.libs.getPtr(funName).?;
        };

        const fun = libEntry.funs.get(funName) orelse b: {
            // allocate c string
            const cstr = try al.allocSentinel(u8, funName.len, 0);
            @memcpy(cstr, funName);
            const foundFun = libEntry.lib.lookup(*anyopaque, cstr) orelse {
                return error.FailedToFindExternalFunction;
            };

            try libEntry.funs.put(funName, foundFun);

            break :b foundFun;
        };

        return fun;
    }
};

// kek
const Runtime = error{
    Return,
    Break, // unused right now, just here to show ya.

    ExpectDyLibAnnotation,
    FailedToFindExternalFunction,
};
