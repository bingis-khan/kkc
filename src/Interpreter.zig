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
returnValue: ValueMeta = undefined, // default value is returned at the end of run()
scope: *Scope,
tymap: *const TypeMap,
funLoader: DyLibLoader,
typeContext: *const TypeContext,
prelude: Prelude,

// right now a very simple interpreter where we don't free.
pub fn run(modules: []ast, prelude: Prelude, typeContext: *const TypeContext, al: std.mem.Allocator) !i64 {
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
        .prelude = prelude,
    };
    for (modules) |module| {
        self.stmts(module.toplevel) catch |err| switch (err) {
            error.Return => {
                return self.returnValue.ref.int;
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
            try self.putVar(vd.varDef, v, vd.varValue.t);
        },
        .VarMut => |vm| {
            const exprVal = try self.expr(vm.varValue);
            const sz = self.sizeOf(vm.varValue.t);

            const v = self.getVar(vm.varRef);
            if (vm.accessors.len == 0) {
                @memcpy(v.ref.slice(sz.size), exprVal.ref.slice(sz.size)); // make sure to memcpy, because we want the content of REFERENCES to change also.
            } else {
                const firstAccess = vm.accessors[0];
                var varVal = switch (firstAccess.acc) {
                    .Deref => v.ref.ptr,
                    .Access => |mem| self.getFieldFromType(v.ref, firstAccess.tBefore, mem),
                };
                for (vm.accessors[1..]) |acc| {
                    switch (acc.acc) {
                        .Deref => {
                            varVal = varVal.ptr;
                        },
                        .Access => |field| {
                            varVal = self.getFieldFromType(varVal, acc.tBefore, field);
                        },
                    }
                }

                // incorrect write!
                // varVal.header = exprVal.header;
                @memcpy(varVal.slice(sz.size), exprVal.ref.slice(sz.size)); // make sure to memcpy, because we want the content of REFERENCES to change also.
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
                if (try self.tryDeconstruct(case.decon, switchOn.ref)) {
                    try self.stmts(case.body);
                    break;
                }
            } else {
                return error.CaseNotMatched;
            }
        },
    }
}

fn initEnvSnapshot(self: *Self, env: ast.Env) ![]RawValue.Fun.EnvSnapshot {
    const envSnapshot = try self.arena.alloc(RawValue.Fun.EnvSnapshot, env.len);
    for (env, 0..) |ei, i| {
        envSnapshot[i] = switch (ei.v) {
            .Var => |v| .{ .Snap = .{
                .v = v,
                .vv = try self.copyValue(self.getVar(v).ref, ei.t),
            } },
            .Fun => |envfun| .{ .Snap = .{
                .v = envfun.name,
                .vv = (try self.initFunction(envfun, ei.m)).ref,
            } },
            .ClassFun => |cfr| b: {
                const instfun = switch (cfr.ref.*.?) {
                    .InstFun => |instfun| instfun,
                    .Id => |id| self.tymap.tryGetFunctionByID(id) orelse break :b .{ .AssocID = id },
                };
                break :b .{ .Snap = .{
                    .v = instfun.fun.name,
                    .vv = (try self.initFunction(instfun.fun, instfun.m)).ref,
                } };
            },
        };
    }

    return envSnapshot;
}

fn addEnvSnapshot(self: *Self, fun: *ast.Function) !void {
    // construct env for this function yo.
    const envSnapshot = try self.initEnvSnapshot(fun.env);
    try self.scope.funs.put(fun.name, envSnapshot);
}

// note that we are getting deep into structs. we must not convert that Value.Type pointer into *Value, because it might not have that header.
fn tryDeconstruct(self: *Self, decon: *ast.Decon, v: RawValueRef) !bool {
    switch (decon.d) {
        .None => return true,
        .Var => |vn| {
            try self.putRef(vn, v);
            return true;
        },
        .Record => |fields| {
            for (fields) |field| {
                const nuv = self.getFieldFromType(v, decon.t, field.field);
                if (!try self.tryDeconstruct(field.decon, nuv)) return false;
            }

            return true;
        },
        .Con => |con| {
            // deref
            if (con.con.data.eq(self.prelude.defined(.Ptr))) {
                return try self.tryDeconstruct(con.decons[0], v.ptr);
            } else {
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

                        var off: usize = @sizeOf(RawValue.Tag);
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
            }
        },
        // else => unreachable,
    }
}

fn exprs(self: *Self, es: []*ast.Expr) ![]ValueMeta {
    var args = try self.arena.alloc(ValueMeta, es.len);
    for (es, 0..) |a, i| {
        args[i] = try self.expr(a);
    }

    return args;
}

fn expr(self: *Self, e: *ast.Expr) Err!ValueMeta {
    switch (e.e) {
        .Intrinsic => |intr| {
            switch (intr.intr.ty) {
                .cast => {
                    const v = try self.expr(intr.args[0]);
                    // just pass the value down :)
                    return v;
                },
                .undefined => {
                    // allocate some value but dont initialize any of it!
                    // COPYPASTA AGAIN. Suggested interface: allocValue(Type)
                    const buf = try self.arena.alloc(u8, self.sizeOf(e.t).size);
                    const emptyValue: ValueMeta = .{
                        // .header = .{
                        //     .ogPtr = null,
                        // },
                        .ref = @alignCast(@ptrCast(buf.ptr)),
                    };
                    return emptyValue;
                },
                .@"size-of" => {
                    return try self.intValue(@intCast(self.sizeOf(intr.args[0].t).size));
                },
                .@"offset-ptr" => {
                    // Should I make offset-ptr relative to type (C) or not (basado)?
                    // Nahhhhh
                    const ogPtr = try self.expr(intr.args[0]);
                    const amount = try self.expr(intr.args[1]);

                    const ptr = try self.copyValue(ogPtr.ref, intr.args[0].t);
                    if (amount.ref.int > 0) { // if pointer is null, @ptrFromInt produces an error. Offseting the null pointer by 0 is correct tho.
                        ptr.ptr = @ptrFromInt(@intFromPtr(ptr.ptr) + @as(usize, @intCast(amount.ref.int)));
                    }
                    return valFromRef(ptr);
                },
            }
        },
        .Int => |x| {
            return self.intValue(x);
        },
        .BinOp => |op| {
            // first, short circuiting ops.
            switch (op.op) {
                .Or => {
                    const lv = try self.expr(op.l);
                    if (isTrue(lv)) {
                        return lv;
                    } else {
                        return try self.expr(op.r);
                    }
                },

                .And => {
                    const lv = try self.expr(op.l);
                    if (isTrue(lv)) {
                        return try self.expr(op.r);
                    } else {
                        return lv;
                    }
                },

                // these ones don't short circuit, so we can simplify our structure.
                else => {
                    const l = try self.expr(op.l);
                    const r = try self.expr(op.r);
                    return switch (op.op) {
                        .Equals => try self.boolValue(l.ref.int == r.ref.int), // TEMP!!!

                        .Plus => try self.intValue(l.ref.int + r.ref.int),
                        .Minus => try self.intValue(l.ref.int - r.ref.int),
                        .Times => try self.intValue(l.ref.int * r.ref.int),

                        .LessThan => try self.boolValue(l.ref.int < r.ref.int),
                        .GreaterThan => try self.boolValue(l.ref.int > r.ref.int),
                        .GreaterEqualThan => try self.boolValue(l.ref.int >= r.ref.int),

                        else => {
                            std.debug.print("unimplemented op: {}\n", .{op.op});
                            unreachable;
                        },
                    };
                },
            }

            unreachable;
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
                    return try self.val(.{ .extptr = nanbox(fun, .ExternalFunction) });
                },
            }
        },
        .Str => |slit| {
            const s: *anyopaque = @ptrCast(try self.evaluateString(slit));
            return try self.val(.{ .extptr = s });
        },
        .Call => |c| {
            const fun = try self.expr(c.callee);
            const ptrAndfunType = nunbox(fun.ref.extptr); // funny, but in this case we *know* the inner value is a pointer, which just don't know what to. So, we use extptr (kind of incorrectly) as a "general pointer".
            const funPtr: *align(1) const RawValue = @ptrCast(&ptrAndfunType.ptr); // and later get the address, so we can do the .<correct-type>
            const funType = ptrAndfunType.fty;

            switch (funType) {
                .ExternalFunction => {
                    // INCORRECT. check https://refspecs.linuxfoundation.org/elf/x86_64-abi-0.99.pdf
                    //  (also just ask an LLM, this time it seems to know what it's talking about.)
                    //    Depending on the data, return values are returned differently (like, floats are returned in XMM registers, so currently it wont work.)
                    //   We can have a function type for each case, then beat it for it to make sense for our types.
                    //   it's required, but since libc does not really return full structs, it's not needed. (rn: i only need to check for error codes)
                    //   Nah, I should just use inline assembly in this case.
                    const castFun: *const fn (...) callconv(.C) i64 = @ptrCast(funPtr.extptr);

                    const MaxExtArgs = 8;
                    if (c.args.len > MaxExtArgs) unreachable;

                    var interopArgs: std.meta.Tuple(&(.{i64} ** MaxExtArgs)) = undefined; // max external function call args: 16 (ideally, should be equal to C's max limit)
                    inline for (0..MaxExtArgs) |i| {
                        if (i < c.args.len) {
                            const av = try self.expr(c.args[i]);
                            interopArgs[i] = @as(*align(1) i64, @alignCast(@constCast(@ptrCast(av.ref)))).*;
                        }
                    }

                    const ret = @call(.auto, castFun, interopArgs);

                    return try self.intValue(ret);
                },

                .LocalFunction => {
                    const args = try self.exprs(c.args);
                    return self.function(funPtr.fun, args);
                },

                .ConstructorFunction => {
                    return self.initRecord(funPtr.confun, c.args, e.t);
                },

                .Lambda => {
                    const lamAndEnv = funPtr.lam;

                    // begin new scope
                    const oldScope = self.scope;
                    var scope = Scope.init(oldScope, self.arena);
                    self.scope = &scope;
                    defer self.scope = oldScope;

                    // COPYPASTA WARNING
                    const lam = lamAndEnv.lam;
                    const env = lamAndEnv.env;
                    for (env) |ee| {
                        switch (ee) {
                            .Snap => |eee| try self.putRef(eee.v, eee.vv),
                            .AssocID => |id| {
                                const efun = self.tymap.tryGetFunctionByID(id).?;
                                const efunv = try self.initFunction(efun.fun, efun.m);
                                try self.putRef(efun.fun.name, efunv.ref);
                            },
                        }
                    }

                    const args = try self.exprs(c.args);
                    for (lam.params, args) |decon, a| {
                        if (!try self.tryDeconstruct(decon, a.ref)) {
                            return error.CaseNotMatched;
                        }
                    }

                    const ret = try self.expr(lam.expr);

                    return ret;
                },

                .None => unreachable,
            }
        },
        .Con => |con| {
            if (con.tys.len == 0) {
                return try self.val(.{ .enoom = con.tagValue });
            } else {
                return try self.val(.{ .confun = nanbox(con, .ConstructorFunction) });
            }
        },

        .UnOp => |uop| {
            const v = try self.expr(uop.e);
            return switch (uop.op) {
                .Deref => valFromRef(v.ref.ptr),
                .Ref => try self.val(.{
                    .ptr = v.ref,
                }),
                .Access => |mem| valFromRef(self.getFieldFromType(v.ref, uop.e.t, mem)),
                .As => v,
            };
        },

        .AnonymousRecord => |recs| {
            // THIS ASSUMES THAT RECORDS GET THAT `setType` TREATMENT
            // ALSO, COPYPASTA FROM `initRecord`. MAYBE WE CAN GENERALIZE IT SOMEHOW?
            const buf = try self.arena.alloc(u8, self.sizeOf(e.t).size);
            var stream = std.io.fixedBufferStream(buf);
            const w = stream.writer();

            switch (self.getType(e.t)) {
                .Anon => |fields| {
                    // order fields according to type (SLOW)
                    for (fields) |field| {
                        for (recs) |rec| {
                            if (common.streq(field.field, rec.field)) {
                                _ = try self.writeExpr(w, rec.value);
                                break;
                            }
                        } else unreachable;
                    }
                },
                .Con => |cons| {
                    switch (cons.type.stuff) {
                        .recs => |fields| {
                            // order fields according to type (SLOW)
                            for (fields) |field| {
                                for (recs) |rec| {
                                    if (common.streq(field.field, rec.field)) {
                                        _ = try self.writeExpr(w, rec.value);
                                        break;
                                    }
                                } else unreachable;
                            }
                        },
                        .cons => unreachable,
                    }
                },
                else => unreachable,
            }

            const vptr: RawValueRef = @alignCast(@ptrCast(buf.ptr));
            return valFromRef(vptr);
        },

        .NamedRecord => |nrec| {

            // THIS ASSUMES THAT RECORDS GET THAT `setType` TREATMENT
            // ALSO, COPYPASTA FROM `initRecord`. MAYBE WE CAN GENERALIZE IT SOMEHOW?
            // SECOND GRADE COPYPASTA FROM `AnonymousRecord`
            const buf = try self.arena.alloc(u8, self.sizeOf(e.t).size);
            var stream = std.io.fixedBufferStream(buf);
            const w = stream.writer();

            const fields = nrec.data.stuff.recs;

            // order fields according to type (SLOW)
            for (fields) |field| {
                for (nrec.fields) |rec| {
                    if (common.streq(field.field, rec.field)) {
                        _ = try self.writeExpr(w, rec.value);
                        break;
                    }
                } else unreachable;
            }

            const vptr: RawValueRef = @alignCast(@ptrCast(buf.ptr));
            return valFromRef(vptr);
        },

        .Lam => |*l| {
            const ptr = nanbox(try common.allocOne(self.arena, RawValue.Lam{
                .lam = l,
                .env = try self.initEnvSnapshot(l.env),
            }), .Lambda);
            return try self.val(.{ .lam = ptr });
        },
    }

    unreachable;
}

fn getFieldFromType(self: *Self, v: RawValueRef, t: ast.Type, mem: Str) RawValueRef {
    switch (self.getType(t)) {
        .Anon => |fields| {
            return self.getFieldFromFields(v, fields, mem);
        },
        .Con => |con| {
            // map it like function or sizeOf for cons!
            const oldTyMap = self.tymap;
            var tymap = TypeMap{
                .prev = oldTyMap,
                .scheme = &con.type.scheme,
                .match = con.application,
            };
            self.tymap = &tymap;
            defer self.tymap = oldTyMap;

            switch (con.type.stuff) {
                .recs => |recs| return self.getFieldFromFields(v, recs, mem),
                .cons => unreachable,
            }
        },

        else => unreachable,
    }
}

fn getFieldFromFields(self: *Self, v: RawValueRef, fields: []ast.Record, mem: Str) RawValueRef {
    var size: usize = 0;
    for (fields) |field| {
        const sz = self.sizeOf(field.t);
        size += calculatePadding(size, sz.alignment);
        if (common.streq(field.field, mem)) {
            const ptr = v;
            return ptr.offset(size);
        }

        size += sz.size;
    } else {
        unreachable;
    }
}

fn initFunction(self: *Self, fun: *ast.Function, m: *ast.Match(ast.Type)) !ValueMeta {
    return try self.val(.{
        .fun = nanbox(try common.allocOne(self.arena, RawValue.Fun{
            .fun = fun,
            .env = self.scope.getFun(fun.name),
            .match = m,
        }), .LocalFunction),
    });
}

fn function(self: *Self, funAndEnv: *RawValue.Fun, args: []ValueMeta) Err!ValueMeta {
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
            .Snap => |ee| try self.putRef(ee.v, ee.vv),
            .AssocID => |id| {
                const efun = self.tymap.tryGetFunctionByID(id).?;
                const efunv = try self.initFunction(efun.fun, efun.m);
                try self.putRef(efun.fun.name, efunv.ref);
            },
        }
    }

    for (fun.params, args) |decon, a| {
        if (!try self.tryDeconstruct(decon, a.ref)) {
            return error.CaseNotMatched;
        }
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

const RawValueRef = *align(1) RawValue; // basically, because we don't know *where* the basic data might be, we must assume its offset can be whatever (imagine: struct with  three chars and getting a reference to one of them)

// shit name: basically the value + metadata.
const ValueMeta = struct {
    // header: Header,
    ref: *align(1) RawValue,
};

fn copyValueMeta(self: *const Self, vm: ValueMeta, t: ast.Type) !ValueMeta {
    const vref = try self.copyValue(vm.ref, t);
    return valFromRef(vref);
}

// values n shit
// note, that we must preserve the inner representation for compatibility with external functions.
// note about alignments: in C structs the alignments are variable (because we might represent different structs), so everything must be align(1)
const RawValue = extern union {
    int: i64,
    extptr: *anyopaque,
    ptr: *align(1) RawValue,
    fun: *Fun,
    confun: *ast.Con,
    lam: *const Lam,
    enoom: u32, // for enum-like data structures
    record: Flexible, // one constructor / record type
    adt: extern struct {
        tag: u32,
        data: Flexible,
    },

    const Lam = struct {
        lam: *const ast.Expr.Lam,
        env: []Fun.EnvSnapshot,
    };

    const Fun = struct {
        fun: *ast.Function,
        env: []EnvSnapshot,
        match: *ast.Match(ast.Type),

        const EnvSnapshot = union(enum) {
            // i forgot what it was gegegeg
            Snap: struct { v: ast.Var, vv: *align(1) RawValue },
            AssocID: ast.Association.ID,
        };
    };

    fn offset(self: *align(1) @This(), off: usize) RawValueRef {
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

    // fn headerSize() comptime_int {
    //     return Header.PaddedSize; // @sizeOf(@This()) - @sizeOf(Type); // TODO: look at this. Prolly should be replaced with Header.PaddedSize
    // }

    const Flexible = *anyopaque;
    const Tag = u32;
};

// const Header = extern struct {
//     // Interpreter shit.
//     ogPtr: ?*ValueMeta, // in case of deconstructions, I want pointers to point to the original value.

//     // match: *ast.Match(ast.Type),

//     // const PaddedSize = @sizeOf(Header) + calculatePadding(@sizeOf(Header), @alignOf(Header));
// };

const FunctionType = enum(u64) {
    ExternalFunction,
    LocalFunction,
    ConstructorFunction,
    Lambda,
    None,
};

fn copyValue(self: *Self, vt: RawValueRef, t: ast.Type) !RawValueRef {
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
    const memptr: []u8 = try self.arena.alloc(u8, sz.size);
    @memcpy(memptr[0..sz.size], @as([*]u8, @ptrCast(vt))[0..sz.size]);
    const vptr: RawValueRef = @alignCast(@ptrCast(memptr.ptr));
    return vptr;
}

fn intValue(self: *const Self, i: i64) !ValueMeta {
    return self.val(.{ .int = i });
}

fn boolValue(self: *const Self, b: bool) !ValueMeta {
    return self.val(.{ .enoom = @intFromBool(b) });
}

fn valFromRef(ref: RawValueRef) ValueMeta {
    return .{
        // .header = .{ .ogPtr = null },
        .ref = ref,
    };
}

fn val(self: *const Self, v: RawValue) !ValueMeta {
    const ref = try self.arena.create(RawValue);
    ref.* = v;
    return .{
        // .header = .{ .ogPtr = null },
        .ref = ref,
    };
}

fn initRecord(self: *Self, c: *ast.Con, args: []*ast.Expr, t: ast.Type) !ValueMeta {
    // alignment:
    // https://youtu.be/E0QhZ6tNoR  <= "alignment" is actually a place where values can live.
    // I get it, but why (in the video example) the trailing padding is aligned to 8? because of last member?
    // I think it gets padded to the largest struct.
    //   (watch out: it's incomplete, because from some Zig issue I've seen, i128 padding might be 8)
    //  nested structs do not create "big alignments". If the struct's max alignment was 8, it gets carred to the outer struct.
    const buf = try self.arena.alloc(u8, self.sizeOf(t).size);
    var stream = std.io.fixedBufferStream(buf);
    var w = stream.writer();

    var maxAlignment: usize = 1;
    if (c.data.structureType() == .ADT) {
        try w.writeInt(u32, c.tagValue, endianness);
        maxAlignment = 4;
    }

    // remember: check bytes written with `w.context.items.len`
    for (args) |a| {
        const alignment = try self.writeExpr(w, a);

        maxAlignment = @max(maxAlignment, alignment);
    }

    // write ending padding (from experiments it's based on max padding.)
    try pad(w, maxAlignment);

    const vptr: RawValueRef = @alignCast(@ptrCast(buf.ptr));
    return valFromRef(vptr);
}

fn writeExpr(self: *Self, w: anytype, a: *ast.Expr) !usize {
    const ty = a.t;
    const v = try self.expr(a);
    const sz = self.sizeOf(ty);
    try pad(w, sz.alignment);
    try w.writeAll(v.ref.slice(sz.size));

    return sz.alignment;
}

fn pad(w: anytype, alignment: usize) !void {
    const i = w.context.pos;
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
    switch (self.getType(t)) {
        .Anon => |fields| {
            return self.sizeOfRecord(fields, 0);
        },
        .Con => |c| {
            const oldTyMap = self.tymap;
            const tymap = TypeMap{
                .prev = oldTyMap,
                .scheme = &c.type.scheme,
                .match = c.application,
            };
            self.tymap = &tymap;
            defer self.tymap = oldTyMap;

            // check if ptr
            if (c.type.eq(self.prelude.defined(.Ptr))) {
                return .{ .size = @sizeOf(*anyopaque), .alignment = @alignOf(*anyopaque) };
            }

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
                        .size = @sizeOf(RawValue.Tag),
                        .alignment = @alignOf(RawValue.Tag),
                    };
                },
                .RecordLike => {
                    return switch (c.type.stuff) {
                        .cons => |cons| self.sizeOfCon(&cons[0], 0),
                        .recs => |recs| self.sizeOfRecord(recs, 0),
                    };
                },
                .ADT => {
                    var max: ?Sizes = null;
                    for (c.type.stuff.cons) |*con| {
                        const sz = self.sizeOfCon(con, @sizeOf(RawValue.Tag));
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
                    m.size += @sizeOf(RawValue.Tag); // don't forget to add a tag. we don't need to change alignment tho, because we took care of it beforehand.
                    m.size += calculatePadding(m.size, m.alignment);
                    return m;
                },
            }
        },
        .TVar => unreachable, // |tv| return self.sizeOf(self.tymap.getTVar(tv)),

        .Fun => return .{
            .size = @sizeOf(*RawValue.Fun),
            .alignment = @alignOf(*RawValue.Fun),
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

// stupid copy
fn sizeOfRecord(self: *Self, fields: []ast.TypeF(ast.Type).Field, beginOff: usize) Sizes {
    var off = beginOff;
    // SMELL: duplicate logic with initRecord
    var maxAlignment: usize = 1;
    for (fields) |field| {
        const ty = field.t;
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

fn getType(self: *const Self, ogt: ast.Type) ast.TypeF(ast.Type) {
    return switch (self.typeContext.getType(ogt)) {
        .TVar => |tv| self.getType(self.tymap.getTVar(tv)),
        else => |t| t,
    };
}

fn getVar(self: *Self, v: ast.Var) ValueMeta {
    const vref = self.scope.getVar(v);
    return valFromRef(vref);
}

fn putVar(self: *Self, v: ast.Var, value: ValueMeta, t: ast.Type) !void {
    try self.scope.vars.put(v, try self.copyValue(value.ref, t));
}

fn putRef(self: *Self, v: ast.Var, value: RawValueRef) !void {
    try self.scope.vars.put(v, value);
}

const Scope = struct {
    prev: ?*@This(),
    vars: Vars,
    funs: Funs,

    const Vars = std.HashMap(ast.Var, RawValueRef, ast.Var.comparator(), std.hash_map.default_max_load_percentage);

    const Funs = std.HashMap(ast.Var, []RawValue.Fun.EnvSnapshot, ast.Var.comparator(), std.hash_map.default_max_load_percentage);

    fn init(prev: ?*@This(), al: std.mem.Allocator) @This() {
        return .{
            .prev = prev,
            .vars = Vars.init(al),
            .funs = Funs.init(al),
        };
    }

    fn getVar(self: *const @This(), v: ast.Var) RawValueRef {
        return self.vars.get(v) orelse (self.prev orelse unreachable).getVar(v);
    }

    fn getFun(self: *const @This(), v: ast.Var) []RawValue.Fun.EnvSnapshot {
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

// https://muxup.com/2023q4/storing-data-in-pointers
fn nanbox(ptr: anytype, fty: FunctionType) @TypeOf(ptr) {
    const choice: usize = switch (fty) {
        .None => unreachable, // we shouldn't call it on non-functions.
        .ConstructorFunction => 0b0001,
        .LocalFunction => 0b0010,
        .ExternalFunction => 0b0100,
        .Lambda => 0b1000,
    };

    var pint = @intFromPtr(ptr);
    pint = @bitReverse(pint);
    if ((pint & 0b1111) > 0 and (pint & 0b1111) != choice) unreachable; // check just in case - this would be a bug frfr.
    pint |= choice;
    pint = @bitReverse(pint);

    return @ptrFromInt(pint);
}

fn nunbox(ptr: anytype) struct { ptr: @TypeOf(ptr), fty: FunctionType } {
    var pint = @intFromPtr(ptr);
    pint = @bitReverse(pint);
    // NOTE: WE DON'T NEED TO SIGN EXTEND AS A LINUX USERSPACE PROGRAM.
    const clearedPtr: @TypeOf(ptr) = @ptrFromInt(@bitReverse(pint & ~@as(usize, 0b1111)));
    return .{
        .ptr = clearedPtr,
        .fty = if ((pint & 0b1000) > 0) .Lambda else if ((pint & 0b0100) > 0) .ExternalFunction else if ((pint & 0b0010) > 0) .LocalFunction else if ((pint & 0b0001) > 0) .ConstructorFunction else unreachable,
    };
}

// check if value is true.
// the impl is funny and not immediately obvious, so this function was made to document that.
fn isTrue(v: ValueMeta) bool {
    return v.ref.enoom > 0;
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
