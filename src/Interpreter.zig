const std = @import("std");
const ast = @import("ast.zig");
const Prelude = @import("Prelude.zig");
const common = @import("common.zig");
const Str = common.Str;
const endianness = @import("builtin").target.cpu.arch.endian();
const TypeContext = @import("TypeContext.zig");

const Self = @This();

arena: std.mem.Allocator,
returnValue: *Value = undefined, // default value is returned at the end of run()
scope: Scope,
funLoader: DyLibLoader,
typeContext: *const TypeContext,

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
pub fn run(module: ast, prelude: Prelude, typeContext: *const TypeContext, al: std.mem.Allocator) !i64 {
    _ = prelude;
    const scope = Scope.init(al);
    var self: Self = .{
        .scope = scope,
        .typeContext = typeContext,
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
                .data = .{
                    .fun = try common.allocOne(self.arena, Value.Type.Fun{
                        .fun = fun,
                        .env = envSnapshot, // TODO
                    }),
                },
                .header = .{ .functionType = .LocalFunction },
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
        .Switch => |sw| {
            const switchOn = try self.expr(sw.switchOn);

            for (sw.cases) |case| {
                if (try self.tryDeconstruct(case.decon, switchOn.toTypePtr())) {
                    try self.stmts(case.body);
                    break;
                }
            } else {
                return error.CaseNotMatched;
            }
        },
        else => {
            unreachable;
        },
    }
}

// note that we are getting deep into structs. we must not convert that Value.Type pointer into *Value, because it might not have that header.
fn tryDeconstruct(self: *Self, decon: *ast.Decon, v: *align(1) Value.Type) !bool {
    switch (decon.d) {
        .Var => |vn| {
            try self.scope.put(vn, try self.copyValue(v, decon.t));
            return true;
        },
        .Con => |con| {
            switch (con.con.data.structureType()) {
                .EnumLike => return v.enoom == con.con.tagValue,
                .RecordLike => {
                    var off: usize = 0;
                    for (con.decons) |d| {
                        const sz = self.sizeOf(d.t);
                        off += calculatePadding(off, sz.alignment);
                        if (!try self.tryDeconstruct(d, v.offset(off))) {
                            return false;
                        }

                        off += sz.size;
                    }

                    return true;
                },
                .ADT => {
                    if (v.adt.tag != con.con.tagValue) return false;

                    var off: usize = @sizeOf(Value.Tag);
                    for (con.decons) |d| {
                        const sz = self.sizeOf(d.t);
                        off += calculatePadding(off, sz.alignment);
                        if (!try self.tryDeconstruct(d, v.offset(off))) {
                            return false;
                        }

                        off += sz.size;
                    }

                    return true;
                },
            }
        },
        // else => unreachable,
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
                        .data = .{ .ptr = fun },
                        .header = .{ .functionType = .ExternalFunction },
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
                .data = .{ .ptr = s },
                .header = .{ .functionType = .None },
            });
        },
        .Call => |c| {
            const fun = try self.expr(c.callee);

            switch (fun.header.functionType) {
                .ExternalFunction => {
                    const castFun: *const fn (...) callconv(.C) i64 = @ptrCast(fun.data.ptr);

                    const MaxExtArgs = 5;
                    var interopArgs: std.meta.Tuple(&(.{i64} ** MaxExtArgs)) = undefined; // max external function call args: 16 (ideally, should be equal to C's max limit)
                    inline for (0..MaxExtArgs) |i| {
                        if (i < c.args.len) {
                            const av = try self.expr(c.args[i]);
                            interopArgs[i] = @as(*align(1) i64, @alignCast(@constCast(@ptrCast(&av.data)))).*;
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
                    return self.initRecord(fun.data.confun, c.args);
                },

                else => unreachable,
            }
        },
        .Con => |con| {
            if (con.tys.len == 0) {
                return try self.initValue(.{
                    .header = .{ .functionType = .None },
                    .data = .{ .enoom = con.tagValue },
                });
            } else {
                return try self.initValue(.{
                    .header = .{ .functionType = .ConstructorFunction },
                    .data = .{ .confun = con },
                });
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
const Value = extern struct {
    header: Header align(1), // TEMP align(1)

    // This is where we store actual data. This must be compatible with C shit.
    data: Type align(1),
    const Type = extern union { // NTE: I might change it so that `data` only contains stuff that should be accessible by C.
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

        fn offset(self: *align(1) @This(), off: usize) *align(1) Value.Type {
            const p: [*]u8 = @ptrCast(self);
            const offp = p[off..];
            return @alignCast(@ptrCast(offp));
        }
    };

    fn dataSlice(self: *@This(), sz: usize) []u8 {
        var slice: []u8 = undefined;
        slice.len = sz;
        slice.ptr = @ptrCast(&self.data);
        return slice;
    }

    fn toTypePtr(self: *@This()) *align(1) Type {
        return &self.data;
    }

    fn headerSize() comptime_int {
        return @sizeOf(@This()) - @sizeOf(Type);
    }

    const Flexible = *anyopaque;
    const Tag = u32;
};

const Header = extern struct {
    // Interpreter shit.
    functionType: enum(u64) { // optional value parameter, which distinguishes local functions and external (which need to be called differently.)
        ExternalFunction,
        LocalFunction,
        ConstructorFunction,
        None,
    },
};

fn copyValue(self: *Self, vt: *align(1) Value.Type, t: ast.Type) !*Value {
    // TODO: how do we retrieve functionType??
    //  that points to a deeper problem of storing functions in datatypes... bruh.
    //   1. Ignore for now. Just set it to null and just don't put functions in datatypes.
    //   1.5. Ignore for now. Just store extra data in the packed union on second field. Incorrect offsets for C code tho, so you may not pass structs with external functions.
    //   2. it's possible to solve this by packing shit into pointer's high bits. nan boxing. Now we can pass structs with functions in C. Obviously, C code trying to call our function will fail, because it's interpreted.
    //   3. complete and utter possibility. instead of differentiating pointers, save the interpreter context SOMEWHERE, then this function would somehow retrieve it and run.
    //      Problem 1: How to access context? We may not be able to provide it (eg. atexit) So, the interpreter state must be at some known location.
    //      Problem 2: How to know which function it should execute? We might use global state if we solve Problem 1, but maybe there is a better way? Like, I need to differentiate the calls somehow. Slightly different pointer?

    // currently at 1!
    const sz = self.sizeOf(t);
    const memptr: []u8 = try self.arena.alloc(u8, Value.headerSize() + sz.size);
    @memcpy(memptr[Value.headerSize() .. Value.headerSize() + sz.size], @as([*]u8, @ptrCast(vt))[0..sz.size]);
    const vptr: *Value = @alignCast(@ptrCast(memptr.ptr));
    vptr.header.functionType = .None;
    return vptr;
}

fn intValue(self: *const Self, i: i64) !*Value {
    return try self.initValue(.{
        .header = .{ .functionType = .None },
        .data = .{ .int = i },
    });
}

fn initValue(self: *const Self, v: Value) !*Value {
    return try common.allocOne(self.arena, v);
}

fn initRecord(self: *Self, c: *ast.Con, args: []*ast.Expr) !*Value {
    // alignment:
    // https://youtu.be/E0QhZ6tNoR  <= "alignment" is actually a place where values can live.
    // I get it, but why (in the video example) the trailing padding is aligned to 8? because of last member?
    // I think it gets padded to the largest struct.
    //   (watch out: it's incomplete, because from some Zig issue I've seen, i128 padding might be 8)
    //  nested structs do not create "big alignments". If the struct's max alignment was 8, it gets carred to the outer struct.
    var buf = std.ArrayList(u8).init(self.arena);
    var w = buf.writer();

    // preallocate stuff for Value and align to 8 to make sure the payload will be correctly aligned.
    try w.writeByteNTimes(undefined, @sizeOf(Header));
    try pad(w, @alignOf(Header));

    var maxAlignment: usize = 1;

    if (c.data.structureType() == .ADT) {
        try w.writeInt(u32, c.tagValue, endianness);
        maxAlignment = 4;
    }

    // remember: check bytes written with `w.context.items.len`
    for (args) |a| {
        const ty = a.t;
        const v = try self.expr(a);
        const sz = self.sizeOf(ty);
        try pad(w, sz.alignment);
        try w.writeAll(v.dataSlice(sz.size));

        maxAlignment = @max(maxAlignment, sz.alignment);
    }

    // write ending padding (from experiments it's based on max padding.)
    try pad(w, maxAlignment);

    const vptr: *Value = @alignCast(@ptrCast(w.context.items.ptr));
    vptr.header.functionType = .None;

    return vptr;
}

fn pad(w: std.ArrayList(u8).Writer, alignment: usize) !void {
    const i = w.context.items.len;
    const padding = calculatePadding(i, alignment);
    if (padding != 0) { // no padding needed when padding == alignment
        try w.writeByteNTimes(undefined, padding);
    }
}

fn calculatePadding(cur: usize, alignment: usize) usize {
    const padding = alignment - (cur % alignment);
    if (padding == alignment) return 0;
    return padding;
}

// calculates total size of the record (including tag)
//  size includes alignment!
//  VERY SLOW, BECAUSE IT RECALCULATES ALIGNMENT EACH TIME.
const Sizes = struct { size: usize, alignment: usize };
fn sizeOf(self: *const Self, t: ast.Type) Sizes {
    switch (self.typeContext.getType(t)) {
        .Con => |c| {
            switch (c.type.structureType()) {
                // ERROR: this is not correct for ints, so watch out.
                //  I should be able to specify expected datatype size.
                .EnumLike => {
                    return .{
                        .size = @sizeOf(Value.Tag),
                        .alignment = @alignOf(Value.Tag),
                    };
                },
                .RecordLike => {
                    return self.sizeOfCon(&c.type.cons[0], 0);
                },
                .ADT => {
                    var max: ?Sizes = null;
                    for (c.type.cons) |*con| {
                        const sz = self.sizeOfCon(con, @sizeOf(Value.Tag));
                        if (max) |*m| {
                            if (sz.size > m.size) {
                                m.size = sz.size;
                            }

                            // with unions, both are split. imagine union of 13 chars and one long.
                            // it'll be aligned to 8, so size 16
                            // (i tested it, it works like that)
                            if (sz.alignment > m.alignment) {
                                m.alignment = sz.alignment;
                            }
                        } else {
                            max = sz;
                        }
                    }

                    var m = max orelse unreachable;
                    m.size += calculatePadding(m.size, m.alignment);
                    return m;
                },
            }
        },
        .TVar => unreachable, // TODO

        .Fun => unreachable, // TODO
        .TyVar => unreachable,
    }
}

fn sizeOfCon(self: *const Self, con: *const ast.Con, beginOff: usize) Sizes {
    var off = beginOff;
    // SMELL: duplicate logic with initRecord
    var maxAlignment: usize = 1;
    for (con.tys) |ty| {
        const sz = self.sizeOf(ty);
        const padding = calculatePadding(off, sz.alignment);
        off += padding + sz.size;
        maxAlignment = @max(maxAlignment, sz.alignment);
    }

    off += calculatePadding(off, maxAlignment);
    return .{
        .size = off,
        .alignment = maxAlignment,
    };
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

    CaseNotMatched,

    OutOfMemory,
} || std.DynLib.Error;
const Runtime = error{
    Return,
    Break, // unused right now, just here to show ya.
};
