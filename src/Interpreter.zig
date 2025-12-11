const std = @import("std");
const ast = @import("ast.zig");
const Prelude = @import("Prelude.zig");
const common = @import("common.zig");
const Str = common.Str;
const endianness = @import("builtin").target.cpu.arch.endian();
const TypeContext = @import("TypeContext.zig");

const Self = @This();

// NOTE: I'll try to mark areas where something is SLOW due to laziness with `SLOW`.

arena: std.mem.Allocator,
returnValue: *Value = undefined, // default value is returned at the end of run()
scope: *Scope,
tymap: *const TypeMap,
funLoader: DyLibLoader,
typeContext: *const TypeContext,

// right now a very simple interpreter where we don't free.
pub fn run(modules: []ast, prelude: Prelude, typeContext: *const TypeContext, al: std.mem.Allocator) !i64 {
    _ = prelude;
    var scope = Scope.init(null, al);
    const scheme = ast.Scheme.empty();
    const tymap = TypeMap{
        .prev = null,
        .scheme = &scheme,
        .match = &ast.Match(ast.Type).empty(scheme),
    };
    var self: Self = .{
        .scope = &scope,
        .typeContext = typeContext,
        .tymap = &tymap,
        .arena = al,
        .funLoader = DyLibLoader.init(al),
    };
    for (modules) |module| {
        self.stmts(module.toplevel) catch |err| switch (err) {
            error.Return => {
                return self.returnValue.data.int;
            },
            else => return err,
        };
    }
    return 0;
}

fn stmts(self: *Self, ss: []*ast.Stmt) !void {
    for (ss) |s| {
        try self.stmt(s);
    }
}

fn stmt(self: *Self, s: *ast.Stmt) Err!void {
    switch (s.*) {
        .Pass => |mlbl| {
            if (mlbl) |lbl| {
                std.debug.print("### LABEL {} ###\n", .{lbl});
            }
        },
        .Expr => |e| {
            _ = try self.expr(e);
        },
        .Return => |e| {
            const v = try self.expr(e);
            self.returnValue = v;
            return error.Return;
        },
        .VarDec => |vd| {
            const v = try self.expr(vd.varValue);
            try self.scope.putVar(vd.varDef, v);
        },
        .VarMut => |vm| {
            const exprVal = try self.expr(vm.varValue);
            const sz = self.sizeOf(vm.varValue.t);

            const varValOrRef = self.scope.getVar(vm.varRef);
            switch (varValOrRef) {
                .ref => |dataNTy| {
                    if (vm.refs == 0) {
                        @memcpy(dataNTy.data.slice(sz.size), exprVal.data.slice(sz.size)); // make sure to memcpy, because we want the content of REFERENCES to change also.
                    } else {
                        var varVal = dataNTy.data.ptr.toValuePtr();
                        for (1..vm.refs) |_| {
                            varVal = varVal.data.ptr.toValuePtr();
                        }
                        varVal.header = exprVal.header;
                        @memcpy(varVal.data.slice(sz.size), exprVal.data.slice(sz.size)); // make sure to memcpy, because we want the content of REFERENCES to change also.
                    }
                },
                .v => |vv| {
                    var varVal = vv;
                    for (0..vm.refs) |_| {
                        varVal = varVal.data.ptr.toValuePtr();
                    }
                    varVal.header = exprVal.header;
                    @memcpy(varVal.data.slice(sz.size), exprVal.data.slice(sz.size)); // make sure to memcpy, because we want the content of REFERENCES to change also.
                },
            }
        },
        .Function => |fun| {
            try self.addEnvSnapshot(fun);
        },
        .Instance => |inst| {
            for (inst.instFuns) |instFun| {
                try self.addEnvSnapshot(instFun.fun);
            }
        },
        .If => |ifs| {
            const cond = try self.expr(ifs.cond);

            if (isTrue(cond)) {
                try self.stmts(ifs.bTrue);
            } else {
                for (ifs.bOthers) |elif| {
                    if (isTrue(try self.expr(elif.cond))) {
                        try self.stmts(elif.body);
                        break;
                    }
                } else if (ifs.bElse) |bElse| {
                    try self.stmts(bElse);
                }
            }
        },
        .While => |whl| {
            while (true) loop: {
                const cond = try self.expr(whl.cond);
                if (!isTrue(cond)) break;

                self.stmts(whl.body) catch |err| switch (err) {
                    error.Break => {
                        break :loop;
                    },
                    else => return err,
                };
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
    }
}

fn addEnvSnapshot(self: *Self, fun: *ast.Function) !void {
    // construct env for this function yo.
    const envSnapshot = try self.arena.alloc(Value.Type.Fun.EnvSnapshot, fun.env.len);
    for (fun.env, 0..) |ei, i| {
        envSnapshot[i] = switch (ei.v) {
            .Var => |v| .{ .Snap = .{
                .v = v,
                .vv = try self.getVar(v),
            } },
            .Fun => |envfun| .{ .Snap = .{
                .v = envfun.name,
                .vv = try self.initFunction(envfun, ei.m),
            } },
            .ClassFun => |cfr| b: {
                const instfun = switch (cfr.ref.*.?) {
                    .InstFun => |instfun| instfun,
                    .Id => |id| self.tymap.tryGetFunctionByID(id) orelse break :b .{ .AssocID = id },
                };
                break :b .{ .Snap = .{
                    .v = instfun.fun.name,
                    .vv = try self.initFunction(instfun.fun, instfun.m),
                } };
            },
        };
    }

    try self.scope.funs.put(fun.name, envSnapshot);
}

// note that we are getting deep into structs. we must not convert that Value.Type pointer into *Value, because it might not have that header.
fn tryDeconstruct(self: *Self, decon: *ast.Decon, v: *align(1) Value.Type) !bool {
    switch (decon.d) {
        .None => return true,
        .Var => |vn| {
            try self.scope.vars.put(vn, .{ .ref = .{ .data = v, .t = decon.t } });
            return true;
        },
        .Con => |con| {
            switch (con.con.data.structureType()) {
                .Opaque => unreachable,
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
                .Minus => try self.intValue(l.data.int - r.data.int),
                .GreaterThan => try self.boolValue(l.data.int > r.data.int),
                else => unreachable,
            };
        },
        .Var => |v| {
            switch (v.v) {
                .Var => |vv| {
                    return self.getVar(vv);
                },

                .Fun => |fun| {
                    return try self.initFunction(fun, v.match);
                },

                .ClassFun => |cfr| {
                    const cfun = cfr.cfun;
                    _ = cfun;
                    return switch (cfr.ref.*.?) {
                        .Id => |uid| b: {
                            const instfun = self.tymap.tryGetFunctionByID(uid).?;
                            break :b try self.initFunction(instfun.fun, instfun.m);
                        },
                        .InstFun => |instfun| try self.initFunction(instfun.fun, instfun.m),
                    };
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
                        .data = .{ .extptr = nanbox(fun, .ExternalFunction) },
                        .header = .{
                            .functionType = .ExternalFunction,
                            .ogPtr = null,
                        },
                    });
                },
            }
        },
        .Str => |slit| {
            const s: *anyopaque = @ptrCast(try self.evaluateString(slit));
            return try self.initValue(Value{
                .data = .{ .extptr = s },
                .header = .{ .functionType = .None, .ogPtr = null },
            });
        },
        .Call => |c| {
            const fun = try self.expr(c.callee);

            switch (fun.header.functionType) {
                .ExternalFunction => {
                    // INCORRECT. check https://refspecs.linuxfoundation.org/elf/x86_64-abi-0.99.pdf
                    //  (also just ask an LLM, this time it seems to know what it's talking about.)
                    //    Depending on the data, return values are returned differently (like, floats are returned in XMM registers, so currently it wont work.)
                    //   We can have a function type for each case, then beat it for it to make sense for our types.
                    //   it's required, but since libc does not really return full structs, it's not needed. (rn: i only need to check for error codes)
                    //   Nah, I should just use inline assembly in this case.
                    const castFun: *const fn (...) callconv(.C) i64 = @ptrCast(nunbox(fun.data.extptr).ptr);

                    const MaxExtArgs = 8;
                    if (c.args.len > MaxExtArgs) unreachable;

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
                    const uf = nunbox(fun.data.fun);
                    if (uf.fty != .LocalFunction) unreachable;
                    return self.function(uf.ptr, args);
                },

                .ConstructorFunction => {
                    return self.initRecord(nunbox(fun.data.confun).ptr, c.args);
                },

                .None => unreachable,
            }
        },
        .Con => |con| {
            if (con.tys.len == 0) {
                return try self.initValue(.{
                    .header = .{ .functionType = .None, .ogPtr = null },
                    .data = .{ .enoom = con.tagValue },
                });
            } else {
                return try self.initValue(.{
                    .header = .{ .functionType = .ConstructorFunction, .ogPtr = null }, // not needed, because it's in the type.
                    .data = .{ .confun = nanbox(con, .ConstructorFunction) },
                });
            }
        },

        .UnOp => |uop| {
            const v = try self.expr(uop.e);
            return switch (uop.op) {
                .Deref => v.data.ptr.toValuePtr(),
                .Ref => try self.initValue(.{
                    .data = .{
                        .ptr = if (v.header.ogPtr) |ogPtr| @alignCast(ogPtr) else @alignCast(&v.data),
                    },
                    .header = .{
                        .functionType = .None,
                        .ogPtr = null,
                    },
                }),
            };
        },

        else => unreachable,
    }

    unreachable;
}

fn initFunction(self: *Self, fun: *ast.Function, m: *ast.Match(ast.Type)) !*Value {
    return try self.initValue(.{
        .data = .{
            .fun = nanbox(try common.allocOne(self.arena, Value.Type.Fun{
                .fun = fun,
                .env = self.scope.getFun(fun.name),
                .match = m,
            }), .LocalFunction),
        },
        .header = .{
            .functionType = .LocalFunction,
            .ogPtr = null,
        },
    });
}

fn function(self: *Self, funAndEnv: *Value.Type.Fun, args: []*Value) Err!*Value {
    // begin new scope
    const oldScope = self.scope;
    var scope = Scope.init(oldScope, self.arena);
    self.scope = &scope;
    defer self.scope = oldScope;

    // also new typemap yo.
    const match = funAndEnv.match;
    const oldTyMap = self.tymap;
    var tymap = TypeMap{
        .prev = oldTyMap,
        .scheme = &funAndEnv.fun.scheme,
        .match = match,
    };
    self.tymap = &tymap;
    defer self.tymap = oldTyMap;

    const fun = funAndEnv.fun;
    const env = funAndEnv.env;
    for (env) |e| {
        switch (e) {
            .Snap => |ee| try self.scope.putVar(ee.v, ee.vv),
            .AssocID => |id| {
                const efun = self.tymap.tryGetFunctionByID(id).?;
                const efunv = try self.initFunction(efun.fun, efun.m);
                try self.scope.putVar(efun.fun.name, efunv);
            },
        }
    }

    for (fun.params, args) |p, a| {
        try self.scope.putVar(p.pn, a);
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
// note about alignments: in C structs the alignments are variable (because we might represent different structs), so everything must be align(1)
const ValRef = *align(1) Value; // basically, because we don't know *where* the basic data might be, we must assume its offset can be whatever (imagine: struct with  three chars and getting a reference to one of them)
const DataRef = *align(1) Value.Type;
const Value = extern struct {
    header: Header align(1), // TEMP align(1)

    // This is where we store actual data. This must be compatible with C shit.
    data: Type align(1),
    const Type = extern union { // NOTE: I might change it so that `data` only contains stuff that should be accessible by C.
        int: i64,
        extptr: *anyopaque,
        ptr: *align(1) Type,
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
            match: *ast.Match(ast.Type),

            const EnvSnapshot = union(enum) {
                Snap: struct { v: ast.Var, vv: *Value },
                AssocID: ast.Association.ID,
            };
        };

        fn toValuePtr(self: *align(1) @This()) ValRef {
            return @fieldParentPtr("data", self);
        }

        fn offset(self: *align(1) @This(), off: usize) DataRef {
            const p: [*]u8 = @ptrCast(self);
            const offp = p[off..];
            return @alignCast(@ptrCast(offp));
        }

        fn slice(self: *align(1) @This(), sz: usize) []u8 {
            var sl: []u8 = undefined;
            sl.len = sz;
            sl.ptr = @ptrCast(self);
            return sl;
        }
    };

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
    functionType: FunctionType, // optional value parameter, which distinguishes local functions and external (which need to be called differently.)
    ogPtr: ?DataRef, // in case of deconstructions, I want pointers to point to the original value.

    // match: *ast.Match(ast.Type),
    const FunctionType = enum(u64) {
        ExternalFunction,
        LocalFunction,
        ConstructorFunction,
        None,
    };
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

    // currently at 2!
    const sz = self.sizeOf(t);
    const memptr: []u8 = try self.arena.alloc(u8, Value.headerSize() + sz.size);
    @memcpy(memptr[Value.headerSize() .. Value.headerSize() + sz.size], @as([*]u8, @ptrCast(vt))[0..sz.size]);
    const vptr: ValRef = @alignCast(@ptrCast(memptr.ptr));
    switch (self.typeContext.getType(t)) {
        .Fun => {
            const nbx = nunbox(vptr.data.fun); // any pointer is okay.
            vptr.header.functionType = nbx.fty;
        },
        else => vptr.header.functionType = .None,
    }
    vptr.header.ogPtr = vt;
    return vptr;
}

fn intValue(self: *const Self, i: i64) !*Value {
    return try self.initValue(.{
        .header = .{ .functionType = .None, .ogPtr = null },
        .data = .{ .int = i },
    });
}

fn boolValue(self: *const Self, b: bool) !*Value {
    return try self.initValue(.{
        .header = .{ .functionType = .None, .ogPtr = null },
        .data = .{ .enoom = @intFromBool(b) },
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
        try w.writeAll(v.data.slice(sz.size));

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
fn sizeOf(self: *Self, t: ast.Type) Sizes {
    switch (self.typeContext.getType(t)) {
        .Con => |c| {
            const oldTyMap = self.tymap;
            const tymap = TypeMap{
                .prev = oldTyMap,
                .scheme = &c.type.scheme,
                .match = c.application,
            };
            self.tymap = &tymap;
            defer self.tymap = oldTyMap;

            switch (c.type.structureType()) {
                // NOTE: not sure if it's correct, but assume pointer size, because that's what opaque types mostly are. I guess I should also use some annotations to check size.
                //  I wonder if I should make sizes in annotations OR will the compiler just *know* about inbuilt types?
                .Opaque => return .{
                    .size = @sizeOf(*anyopaque),
                    .alignment = @alignOf(*anyopaque),
                },
                // ERROR: this is not correct for ints, so watch out.
                //  I should be able to specify expected datatype size.
                .EnumLike => {
                    return .{
                        .size = @sizeOf(Value.Tag),
                        .alignment = @alignOf(Value.Tag),
                    };
                },
                .RecordLike => {
                    return self.sizeOfCon(&c.type.stuff.cons[0], 0);
                },
                .ADT => {
                    var max: ?Sizes = null;
                    for (c.type.stuff.cons) |*con| {
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
        .TVar => |tv| return self.sizeOf(self.tymap.getTVar(tv)),

        .Fun => return .{
            .size = @sizeOf(*Value.Type.Fun),
            .alignment = @alignOf(*Value.Type.Fun),
        },
        .TyVar => unreachable, // actual error. should not happen!
    }
}

fn sizeOfCon(self: *Self, con: *const ast.Con, beginOff: usize) Sizes {
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

fn getVar(self: *Self, v: ast.Var) !ValRef {
    return switch (self.scope.getVar(v)) {
        .v => |val| val,
        .ref => |tyNt| try self.copyValue(tyNt.data, tyNt.t), // oh man
    };
}

const Scope = struct {
    prev: ?*@This(),
    vars: Vars,
    funs: Funs,

    const Vars = std.HashMap(ast.Var, ValOrRef, ast.Var.comparator(), std.hash_map.default_max_load_percentage);

    const Funs = std.HashMap(ast.Var, []Value.Type.Fun.EnvSnapshot, ast.Var.comparator(), std.hash_map.default_max_load_percentage);

    const ValOrRef = union(enum) { v: *Value, ref: struct { data: *align(1) Value.Type, t: ast.Type } };

    fn init(prev: ?*@This(), al: std.mem.Allocator) @This() {
        return .{
            .prev = prev,
            .vars = Vars.init(al),
            .funs = Funs.init(al),
        };
    }

    // the inner datatype might change (since I'm not sure if I should keep Value/Type or just keep refs)
    fn putVar(self: *@This(), v: ast.Var, val: *Value) !void {
        try self.vars.put(v, .{ .v = val });
    }

    fn getVar(self: *const @This(), v: ast.Var) ValOrRef {
        return self.vars.get(v) orelse (self.prev orelse unreachable).getVar(v);
    }

    fn getFun(self: *const @This(), v: ast.Var) []Value.Type.Fun.EnvSnapshot {
        return self.funs.get(v) orelse (self.prev orelse unreachable).getFun(v);
    }
};

const TypeMap = struct {
    prev: ?*const @This(),
    scheme: *const ast.Scheme,
    match: *const ast.Match(ast.Type),

    fn getTVar(self: *const @This(), tv: ast.TVar) ast.Type {
        // SLOW
        for (self.scheme.tvars, self.match.tvars) |s, m| {
            if (s.eq(tv)) {
                return m;
            }
        } else {
            return (self.prev orelse unreachable).getTVar(tv);
        }
    }

    fn tryGetFunctionByID(self: *const @This(), uid: ast.Association.ID) ?ast.Match(ast.Type).AssocRef.InstPair {
        for (self.scheme.associations, self.match.assocs) |a, r| {
            if (a.uid == uid) {
                return switch (r.?) {
                    .Id => |refuid| return self.tryGetFunctionByID(refuid),
                    .InstFun => |instfun| instfun,
                };
            }
        } else {
            return (self.prev orelse return null).tryGetFunctionByID(uid);
        }
    }
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

fn nanbox(ptr: anytype, fty: Header.FunctionType) @TypeOf(ptr) {
    const choice: usize = switch (fty) {
        .None => unreachable, // we shouldn't call it on non-functions.
        .ConstructorFunction => 0b001,
        .LocalFunction => 0b010,
        .ExternalFunction => 0b100,
    };

    var pint = @intFromPtr(ptr);
    pint = @bitReverse(pint);
    if ((pint & 0b111) > 0 and (pint & 0b111) != choice) unreachable; // check just in case - this would be a bug frfr.
    pint |= choice;
    pint = @bitReverse(pint);

    return @ptrFromInt(pint);
}

fn nunbox(ptr: anytype) struct { ptr: @TypeOf(ptr), fty: Header.FunctionType } {
    var pint = @intFromPtr(ptr);
    pint = @bitReverse(pint);
    const clearedPtr: @TypeOf(ptr) = @ptrFromInt(@bitReverse(pint & ~@as(usize, 0b111)));
    return .{
        .ptr = clearedPtr,
        .fty = if ((pint & 0b100) > 0) .ExternalFunction else if ((pint & 0b010) > 0) .LocalFunction else if ((pint & 0b001) > 0) .ConstructorFunction else unreachable,
    };
}

// check if value is true.
// the impl is funny and not immediately obvious, so this function was made to document that.
fn isTrue(v: *Value) bool {
    return v.data.enoom > 0;
}

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
