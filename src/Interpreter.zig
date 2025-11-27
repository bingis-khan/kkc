const std = @import("std");
const ast = @import("ast.zig");
const Prelude = @import("Prelude.zig");
const common = @import("common.zig");
const Str = common.Str;
const endianness = @import("builtin").target.cpu.arch.endian();

const Self = @This();

arena: std.mem.Allocator,
returnValue: *Value = undefined, // default value is returned at the end of run()
scope: Scope,
funLoader: DyLibLoader,

const Scope = std.HashMap(ast.Var, *Value, struct {
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
            return self.returnValue.data.int;
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

fn stmt(self: *Self, s: *ast.Stmt) Err!void {
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
        .Function => |fun| {
            // construct env for this function yo.
            const envSnapshot = try self.arena.alloc(Value.Type.Fun.EnvSnapshot, fun.env.len);
            for (fun.env, 0..) |ei, i| {
                const v = ei.getVar();
                envSnapshot[i] = .{
                    .v = v,
                    .vv = self.scope.get(v) orelse unreachable,
                };
            }

            try self.scope.put(fun.name, try self.initValue(.{
                .size = @sizeOf(*Value.Type.Fun),
                .alignment = @sizeOf(*Value.Type.Fun),
                .data = .{
                    .fun = try common.allocOne(self.arena, Value.Type.Fun{
                        .fun = fun,
                        .env = envSnapshot, // TODO
                    }),
                },
                .functionType = .LocalFunction,
            }));
        },
        .If => |ifs| {
            const cond = try self.expr(ifs.cond);

            if (cond.data.enoom > 0) {
                try self.stmts(ifs.bTrue);
            } else {
                for (ifs.bOthers) |elif| {
                    if ((try self.expr(elif.cond)).data.enoom > 0) {
                        try self.stmts(elif.body);
                        break;
                    }
                } else if (ifs.bElse) |bElse| {
                    try self.stmts(bElse);
                }
            }
        },
        else => {
            unreachable;
        },
    }
}

fn exprs(self: *Self, es: []*ast.Expr) ![]*Value {
    var args = try self.arena.alloc(*Value, es.len);
    for (es, 0..) |a, i| {
        args[i] = try self.expr(a);
    }

    return args;
}

fn expr(self: *Self, e: *ast.Expr) Err!*Value {
    switch (e.e) {
        .Int => |x| {
            return self.intValue(x);
        },
        .BinOp => |op| {
            const l = try self.expr(op.l);
            const r = try self.expr(op.r);
            return switch (op.op) {
                .Plus => try self.intValue(l.data.int + r.data.int),
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

                .Fun => |fun| {
                    if (self.scope.get(fun.name)) |val| {
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
                    return try self.initValue(.{
                        .size = @sizeOf(@TypeOf(fun)),
                        .alignment = @alignOf(@TypeOf(fun)),
                        .data = .{ .ptr = fun },
                        .functionType = .ExternalFunction,
                    });
                },

                else => {
                    unreachable;
                },
            }
        },
        .Str => |slit| {
            const s: *anyopaque = @ptrCast(try self.evaluateString(slit.lit));
            return try self.initValue(Value{
                .alignment = @alignOf(@TypeOf(s)),
                .size = @sizeOf(@TypeOf(s)),
                .data = .{ .ptr = s },
                .functionType = null,
            });
        },
        .Call => |c| {
            const fun = try self.expr(c.callee);

            if (fun.functionType) |ft| {
                switch (ft) {
                    .ExternalFunction => {
                        const castFun: *const fn (...) callconv(.C) i64 = @ptrCast(fun.data.ptr);

                        const MaxExtArgs = 5;
                        var interopArgs: std.meta.Tuple(&(.{i64} ** MaxExtArgs)) = undefined; // max external function call args: 16 (ideally, should be equal to C's max limit)
                        inline for (0..MaxExtArgs) |i| {
                            if (i < c.args.len) {
                                const av = try self.expr(c.args[i]);
                                interopArgs[i] = @as(*i64, @constCast(@ptrCast(&av.data))).*;
                            }
                        }

                        const ret = @call(.auto, castFun, interopArgs);

                        return try self.intValue(ret);
                    },

                    .LocalFunction => {
                        const args = try self.exprs(c.args);
                        return self.function(fun.data.fun, args);
                    },

                    .ConstructorFunction => {
                        const args = try self.exprs(c.args);
                        return self.initRecord(fun.data.confun, args);
                    },
                }
            }
        },
        .Con => |con| {
            switch (con.data.structureType()) {
                .EnumLike => {
                    return try self.initValue(.{
                        .size = @sizeOf(u32),
                        .alignment = @alignOf(u32),
                        .functionType = null,
                        .data = .{ .enoom = con.tagValue },
                    });
                },
                else => {
                    return try self.initValue(.{
                        .size = @sizeOf(*ast.Con),
                        .alignment = @alignOf(*ast.Con),
                        .functionType = .ConstructorFunction,
                        .data = .{ .confun = con },
                    });
                },
            }
        },
        else => unreachable,
    }

    unreachable;
}

fn function(self: *Self, funAndEnv: *Value.Type.Fun, args: []*Value) Err!*Value {
    // overwrite env first.
    const env = funAndEnv.env;
    for (env) |e| {
        try self.scope.put(e.v, e.vv);
    }

    const fun = funAndEnv.fun;
    for (fun.params, args) |p, a| {
        try self.scope.put(p.pn, a);
    }
    self.stmts(fun.body) catch |err| switch (err) {
        error.Return => {
            return self.returnValue;
        },
        error.Break => unreachable,
        else => return err,
    };

    unreachable; // TODO: return unit.
}

fn evaluateString(self: *const Self, s: Str) ![:0]u8 {
    return try self.arena.dupeZ(u8, s);
}

// values n shit
// note, that we must preserve the inner representation for compatibility with external functions.
const Value = struct {
    // Interpreter shit.
    functionType: ?enum { // optional value parameter, which distinguishes local functions and external (which need to be called differently.)
        ExternalFunction,
        LocalFunction,
        ConstructorFunction,
    },

    size: usize, // TODO: this should be known from analyzing the assigned type, but it's easier for now. It greatly increases the size of a value!!
    alignment: usize,

    // This is where we store actual data. This must be compatible with C shit.
    data: Type,
    const Type = extern union { // NOTE: I might change it so that `data` only contains stuff that should be accessible by C.
        int: i64,
        ptr: *anyopaque,
        fun: *Fun,
        confun: *ast.Con,
        enoom: u32, // for enum-like data structures
        record: Flexible, // one constructor / record type
        adt: extern struct {
            tag: u32,
            data: Flexible,
        },

        const Fun = struct {
            fun: *ast.Function,
            env: []EnvSnapshot,

            const EnvSnapshot = struct { v: ast.Var, vv: *Value };
        };

        fn toValuePtr(self: *@This()) *Value {
            return @fieldParentPtr("data", self);
        }
    };

    fn dataSlice(self: *@This()) []u8 {
        var slice: []u8 = undefined;
        slice.len = self.size;
        slice.ptr = @ptrCast(&self.data);
        return slice;
    }

    fn headerSize() comptime_int {
        return @sizeOf(@This()) - @sizeOf(Type);
    }

    const Flexible = *anyopaque;
};

fn intValue(self: *const Self, i: i64) !*Value {
    return try self.initValue(.{
        .functionType = null,
        .size = @sizeOf(i64),
        .alignment = @alignOf(i64),
        .data = .{ .int = i },
    });
}

fn initValue(self: *const Self, v: Value) !*Value {
    return try common.allocOne(self.arena, v);
}

fn initRecord(self: *const Self, c: *ast.Con, args: []*Value) !*Value {
    // alignment:
    // https://youtu.be/E0QhZ6tNoR  <= "alignment" is actually a place where values can live.
    // I get it, but why (in the video example) the trailing padding is aligned to 8? because of last member?
    // I think it gets padded to the largest struct.
    //   (watch out: it's incomplete, because from some Zig issue I've seen, i128 padding might be 8)
    //  nested structs do not create "big alignments". If the struct's max alignment was 8, it gets carred to the outer struct.
    var buf = std.ArrayList(u8).init(self.arena);
    var w = buf.writer();

    // preallocate stuff for Value and align to 8 to make sure the payload will be correctly aligned.
    try w.writeByteNTimes(undefined, Value.headerSize());
    try pad(w, @alignOf(*anyopaque));

    var maxAlignment: usize = 1;

    if (c.data.structureType() == .ADT) {
        try w.writeInt(u32, c.tagValue, endianness);
        maxAlignment = 4;
    }

    // remember: check bytes written with `w.context.items.len`
    for (args) |a| {
        try pad(w, a.alignment);
        try w.writeAll(a.dataSlice());

        maxAlignment = @max(maxAlignment, a.alignment);
    }

    // write ending padding (from experiments it's based on max padding.)
    try pad(w, maxAlignment);

    const recordSize = w.context.items.len;
    const vptr: *Value = @alignCast(@ptrCast(w.context.items.ptr));
    vptr.size = Value.headerSize() + recordSize;
    vptr.alignment = maxAlignment;
    vptr.functionType = null;

    return vptr;
}

fn pad(w: std.ArrayList(u8).Writer, alignment: usize) !void {
    const i = w.context.items.len;
    const padding = alignment - (i % alignment);
    if (padding != alignment) { // no padding needed when padding == alignment
        try w.writeByteNTimes(undefined, padding);
    }
}

// calculates total size of the record (including tag)
//  size includes alignment!
fn sizeOf(c: *ast.Con) struct { size: usize, alignment: usize } {
    _ = c;
    unreachable;
}

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
const Err = RealErr || Runtime;
const RealErr = error{
    ExpectDyLibAnnotation,
    FailedToFindExternalFunction,

    OutOfMemory,
} || std.DynLib.Error;
const Runtime = error{
    Return,
    Break, // unused right now, just here to show ya.
};
