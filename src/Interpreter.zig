const std = @import("std");
const builtin = @import("builtin");
const ffi = @import("ffi");
const ast = @import("ast.zig");
const Prelude = @import("Prelude.zig");
const common = @import("common.zig");
const Str = common.Str;
const endianness = builtin.target.cpu.arch.endian();
const TypeContext = @import("TypeContext.zig");
const Args = @import("Args.zig");
const errr = @import("error.zig");
const stdlib = @cImport(@cInclude("stdlib.h"));
const TypeMap = @import("TypeMap.zig").TypeMap;
const posix = std.posix;
const sizer = @import("sizer.zig");
const Size = sizer.Size;

// this turns on non-deallocating mode. it makes it 5x faster (crying emoji ahh)
// why so slou
const ArenaAlloc = true;

const Self = @This();
var sigself: ?*Self = null;

// IMPORTANT: check all nocheckins before checking in! Im planning on solving the allocator issue after I sort out the Value problem.

alBase: std.mem.Allocator,
arena: std.mem.Allocator,

scope: *Scope, // scope allocator is here.
alCurStack: ?std.mem.Allocator, // not very good miau. null if not currently executing an expression.

returnValue: Value = undefined, // default value is returned at the end of run()
tymap: *const TypeMap,
funLoader: DyLibLoader,
typeContext: *TypeContext,
prelude: Prelude,
progArgs: []Args.Arg,

signalHandlers: SignalHandlers,
constStrings: ConstStrings,

const SignalHandlers = std.AutoArrayHashMap(i32, struct {
    funval: Value,
    funty: ast.Type,
});

const ConstStrings = std.AutoArrayHashMap([*]const u8, [:0]u8);

// right now a very simple interpreter where we don't free.
pub fn run(modules: []ast, prelude: Prelude, typeContext: *TypeContext, progArgs: []Args.Arg, arena: std.mem.Allocator, alBase: std.mem.Allocator) !i64 {
    const base = if (!ArenaAlloc) alBase else arena;
    var scope = Scope.init(null, base);
    defer scope.deinit();
    const scheme = ast.Scheme.empty();
    const tymap = TypeMap{
        .prev = null,
        .scheme = &scheme,
        .match = &ast.Match.empty(scheme),
    };
    var self: Self = .{
        .scope = &scope,
        .arena = arena,
        .alBase = base,

        .alCurStack = if (!ArenaAlloc) null else arena,

        .typeContext = typeContext,
        .tymap = &tymap,

        .funLoader = DyLibLoader.init(arena),
        .prelude = prelude,
        .progArgs = progArgs,
        .signalHandlers = SignalHandlers.init(arena),
        .constStrings = ConstStrings.init(arena),
    };
    sigself = &self;

    for (modules) |module| {
        self.stmts(module.toplevel) catch |err| switch (err) {
            error.Return => {
                return self.returnValue.val.Owned.int;
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
    var defaultStmtExprStack: ExprStack = undefined;
    self.beginStack(&defaultStmtExprStack);
    defer self.restoreStack(&defaultStmtExprStack);

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
            // BUG(return-decon-ref) remember to copy the value in case it's a pure reference.
            const cv = try v.copyTo(self.scope.allocator());
            self.returnValue = cv;
            return error.Return;
        },
        .Break => {
            return error.Break;
        },
        .VarDec => |vd| {
            const v = try self.expr(vd.varValue);
            try self.putVar(vd.varDef, v);
        },
        .VarMut => |vm| {
            const exprVal = try (try self.expr(vm.varValue)).copyTo(self.alCurStack.?);
            const sz = exprVal.size;

            const v = self.getVar(vm.varRef.v);
            if (vm.accessors.len == 0) {
                exprVal.copyToLValue(&v);
            } else {
                const firstAccess = vm.accessors[0];
                var varVal = switch (firstAccess.acc) {
                    .Deref => v.get().ptr,
                    .Access => |mem| self.getFieldFromType(v.getRefFromLValue(), firstAccess.tBefore, mem),
                };
                for (vm.accessors[1..]) |acc| {
                    switch (acc.acc) {
                        .Deref => {
                            varVal = varVal.smol.ptr;
                        },
                        .Access => |field| {
                            varVal = self.getFieldFromType(varVal, acc.tBefore, field);
                        },
                    }
                }

                const nuVal = Value.initLValueFromRef(varVal, sz);
                exprVal.copyToLValue(&nuVal);
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
            loop: while (true) {
                var es: ExprStack = undefined;
                self.beginStack(&es);
                defer self.restoreStack(&es);

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
        .For => |forl| {
            const beforeIntoIter = try self.expr(forl.iter);
            const intoIterFun = try self.getAndUnboxInstFunRef(forl.intoIterFun);
            var intoIterArgs = [_]Value{beforeIntoIter};
            var iterator = try self.function(intoIterFun.get().fun, &intoIterArgs);

            // copied from .Ref case.
            const itptr = Value.ptrToOwned(&iterator);
            const nextFun = try self.getAndUnboxInstFunRef(forl.nextFun);
            loop: while (true) {
                var es: ExprStack = undefined;
                self.beginStack(&es);
                defer self.restoreStack(&es);

                var nextArgs = [_]Value{itptr};
                var nextVal = try self.function(nextFun.get().fun, &nextArgs);

                // HACK HACK HACK: Check if null.
                if (nextVal.isMaybeNone()) {
                    break;
                }

                // TODO(07.06.26): use the sizer API
                var off: usize = sizer.Size.tag.size;
                const sz = self.sizeOf(forl.decon.d.t);
                off += calculatePadding(off, sz.alignment);
                const justValueRef = nextVal.getRefToMemory().offset(off);

                if (try self.tryDeconstruct(forl.decon.d, justValueRef)) {
                    self.stmts(forl.body) catch |err| switch (err) {
                        error.Break => {
                            break :loop;
                        },
                        else => return err,
                    };
                } else {
                    const l = forl.decon.d.l;

                    var hadNewline = false;
                    const ctx = ast.Ctx.init(&hadNewline, self.typeContext);
                    (errr.Error.ErrCtx{
                        .module = l.module,
                        .c = ctx,
                    }).atLocation(l, .{ .label = .{"miau"} });

                    return error.CaseNotMatched;
                }
            }
        },
        .Switch => |sw| {
            var switchOn = try self.expr(sw.switchOn);

            for (sw.cases) |case| {
                if (try self.tryDeconstruct(case.decon, switchOn.getRefToMemory())) {
                    try self.stmts(case.body);
                    break;
                }
            } else {
                const l = sw.switchOn.l;
                var hadNewline = false;
                const ctx = ast.Ctx.init(&hadNewline, self.typeContext);
                (errr.Error.ErrCtx{
                    .module = l.module,
                    .c = ctx,
                }).atLocation(l, .{ .label = .{"miau"} });
                return error.CaseNotMatched;
            }
        },
    }
}

// TODO(07.06.26): define proper allocation
fn initEnvSnapshot(self: *Self, env: *ast.Env, scheme: ?*ast.Scheme) !EnvSnapshot {
    const maxGeneralClassFuns = if (scheme) |sch| sch.associations.len else 0;
    var varSnapshot = try std.ArrayList(EnvSnapshot.VarSnapshot).initCapacity(self.arena, env.insts.items.len + maxGeneralClassFuns);
    for (env.insts.items) |ei| {
        try varSnapshot.append(switch (ei.v) {
            .TNum => |tnum| b: {
                const tnumvar = tnum.asVar();
                break :b .{
                    .Snap = .{
                        .v = tnumvar,
                        .vv = try self.getVar(tnumvar).copyTo(self.arena),
                    },
                };
            },
            .Var => |v| .{ .Snap = .{
                .v = v.v,
                .vv = try self.getVar(v.v).copyTo(self.arena),
            } },
            .Fun => |envfun| .{
                .Snap = .{
                    .v = envfun.name,
                    .vv = try Value.copyTo(&try self.initFunction(envfun, ei.m), self.arena), // NOTE(07.06.26): the `copyTo` is a noop for now, since the function returns a pointer (Smol). That's on purpose - it's a sign to correctly handle it in the future.
                },
            },
            .ClassFun => |cfr| b: {
                const instfun = switch (cfr.ref.*.?) {
                    .InstFun => |instfun| instfun,
                    .Id => |id| self.tymap.tryGetFunctionByID(id) orelse break :b .{ .AssocID = id },
                };
                break :b .{ .Snap = .{
                    .v = instfun.fun.name,
                    .vv = try (try self.initFunction(instfun.fun, instfun.m)).copyTo(self.arena),
                } };
            },
        });
    }

    // TODO(07.06.26): what is this??? why do we need this?
    if (scheme) |sch| {
        for (sch.associations) |assoc| {
            if (assoc.concrete) |conc| {
                try varSnapshot.append(b: {
                    const instfun = switch (conc.ref.*.?) {
                        .InstFun => |instfun| instfun,
                        .Id => |id| self.tymap.tryGetFunctionByID(id) orelse break :b .{ .AssocID = id },
                    };
                    break :b .{ .Snap = .{
                        .v = instfun.fun.name,
                        .vv = try (try self.initFunction(instfun.fun, instfun.m)).copyTo(self.arena),
                    } };
                });
            }
        }
    }

    // now, slowly copy the tymaps!
    const firstTyMap: *TypeMap = try common.allocOne(self.arena, self.tymap.*);

    var oldTyMap = firstTyMap;
    while (oldTyMap.prev) |tymap| {
        const nextTyMap = try common.allocOne(self.arena, tymap.*);
        oldTyMap.prev = nextTyMap;
        oldTyMap = nextTyMap;
    }

    const lastPointer = &oldTyMap.prev;

    const envSnapshot = EnvSnapshot{
        .vars = varSnapshot.items,
        .tymaps = firstTyMap,
        .lastPtr = lastPointer,
    };

    return envSnapshot;
}

fn addEnvSnapshot(self: *Self, fun: *ast.Function) !void {
    // construct env for this function yo.
    const envSnapshot = try self.initEnvSnapshot(fun.env, &fun.scheme);
    try self.scope.funs.put(fun.name, envSnapshot);
}

// NOTE: we are getting deep into structs. we must not convert that Value.Type pointer into *Value, because it might not have that header.
// TODO(07.06.26): use sizer API
fn tryDeconstruct(self: *Self, decon: *ast.Decon, v: RawValueRef) !bool {
    switch (decon.d) {
        .None => return true,
        .Var => |vn| {
            const sz = self.sizeOf(decon.t);
            const vref = Value.initLValueFromRef(v, sz);
            try self.putRef(vn, vref);
            return true;
        },
        .Num => |num| {
            const sz = self.sizeOf(decon.t);
            const vref = Value.initLValueFromRef(v, sz);
            const vnum = Value.int(num.num);

            const vfi = try self.quickCallInst(num.instFromIntegral, &.{vnum});
            const vneg = if (num.instNegate) |instNeg| try self.quickCallInst(instNeg, &.{vfi}) else vfi;
            const veq = try self.quickCallInst(num.instEq, &.{ vneg, vref });

            return isTrue(veq);
        },
        .Str => |s| {
            const sv = Value.initLValueFromRef(v, self.sizeOf(decon.t));
            const cmpsv = try self.strFromString(s.str, s.instFromString);

            const instFun = try self.getAndUnboxInstFunRef(s.instEq);

            var args = [_]Value{ cmpsv, sv };
            const eqResult = try self.function(instFun.get().fun, &args);

            return isTrue(eqResult);
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
                return try self.tryDeconstruct(con.decons[0], v.smol.ptr);
            } else {
                switch (con.con.data.structureType()) {
                    .Opaque => unreachable,
                    .EnumLike => return v.smol.tag == con.con.tagValue,
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
        .List => |listDecon| {
            // const cfun = self.prelude.definedClass(.ListLike).classFuns[0];
            // NOTE COPYPASTA: from .Var in expr()
            // TODO: also, we should avoid allocating and boxing here, since we're gonna call it immediately anyway.
            //  (in the future :))
            const instFun = try self.getAndUnboxInstFunRef(listDecon.assocRef);

            const sal = self.alCurStack.?;

            // ALLOCATE DATA FOR DECONSTRUCTION.
            // TODO: note, it gets copied!!! (unlike general deconstruction)
            //  For slices, we can take a real pointer
            //  but for stuff like iterators, it's impossible!
            //  What should I do?
            const elemSize = self.sizeOf(listDecon.elemTy).size;
            const lsize = listDecon.l.len;
            const lsizeVal = Value.int(lsize);
            const lslice = try sal.alloc(u8, lsize * elemSize);
            var lptr = Value.ptrFromRef(@ptrCast(lslice.ptr));
            const lptrptr = Value.ptrToOwned(&lptr);

            const rsize: usize = if (listDecon.r) |r| r.r.len else 0;
            const rsizeVal = Value.int(rsize);
            const rslice: []u8 = if (rsize > 0) try self.alCurStack.?.alloc(u8, rsize * elemSize) else &.{};
            var rptr = Value.ptrFromRef(@ptrCast(rslice.ptr));
            const rptrptr = Value.ptrToOwned(&rptr);

            // handle possible spread.
            const spreadTy = listDecon.spreadTy;
            const spreadSize = self.sizeOf(spreadTy);
            var spreadInnerVal: ?Value = null; // (Ptr!)
            const spreadVal = if (listDecon.r) |r| b: {
                if (r.spreadVar) |svar| {
                    // init space for the spread variable array.
                    // (the function call will initialize it, we only care about the pointer, which will be passed to the function)
                    const innerSize = self.sizeOf(svar.t);
                    spreadInnerVal = try Value.initOwnedAlloc(innerSize, sal);
                    const innerValPtr = Value.ptrToOwned(&spreadInnerVal.?);

                    // COPYPASTA: need to somehow allocate records and partially initialize them.
                    // initialize whole ListSpread.
                    var buf = try Value.initOwnedAlloc(spreadSize, sal);
                    std.debug.assert(!buf.smol());
                    var stream = std.io.fixedBufferStream(buf.getSlice());
                    const w = stream.writer();

                    try w.writeInt(RawValue.Tag, 2, endianness);
                    try pad(w, @alignOf(*anyopaque)); // why align to anyopaque??? (cuz we pass a pointer here bruh)
                    try w.writeInt(usize, @intFromPtr(innerValPtr.get().ptr), endianness);

                    break :b buf;
                } else {
                    break :b try Value.soleTag(1, spreadSize, sal);
                }
            } else try Value.soleTag(0, spreadSize, sal);

            var args = [_]Value{
                Value.initLValueFromRef(v, self.sizeOf(decon.t)),
                lptrptr,
                lsizeVal,
                spreadVal,
                rptrptr,
                rsizeVal,
            };
            const deconSucceeded = try self.function(instFun.get().fun, &args);

            // if not matched, don't bother setting vars
            if (!isTrue(deconSucceeded)) return false;

            // var lit = valArrayIterator(lslice.ptr, elemSize);
            for (listDecon.l, 0..) |ldecon, i| {
                const lvalref = valArrayGet(lptrptr.get().ref.smol.ref, elemSize, i); // maybe we should just use val.offset()?
                if (!try self.tryDeconstruct(ldecon, lvalref)) {
                    return false;
                }
            }

            if (rsize > 0) {
                for (listDecon.r.?.r, 0..) |rdecon, i| {
                    const rvalref = valArrayGet(rptrptr.get().ref.smol.ref, elemSize, i);
                    if (!try self.tryDeconstruct(rdecon, rvalref)) {
                        return false;
                    }
                }
            }

            if (spreadInnerVal) |innerVal| {
                try self.putRef(listDecon.r.?.spreadVar.?.v, innerVal);
            }

            return true;
        },
        // else => unreachable,
    }
}

fn strFromString(self: *Self, s: Str, inst: ast.InstFunInst) !Value {
    const fromStringFun = try self.getAndUnboxInstFunRef(inst);

    // NOTE: copied from .Str case of expr
    const sopq: *anyopaque = @ptrCast(try self.evaluateString(s));
    const sptr = Value.initOwned(.{ .extptr = sopq }, Size.ptr);

    const slen = Value.int(@as(usize, s.len));
    var args = [_]Value{ sptr, slen };
    const res = try self.function(fromStringFun.get().fun, &args);
    return res;
}

fn exprs(self: *Self, es: []*ast.Expr) ![]TypeVal {
    var args = try self.alCurStack.?.alloc(TypeVal, es.len);
    for (es, 0..) |a, i| {
        args[i] = .{ .v = try self.expr(a), .t = a.t };
    }

    return args;
}

fn expr(self: *Self, e: *ast.Expr) Err!Value {
    const sal = self.alCurStack.?;
    switch (e.e) {
        .Intrinsic => |intr| {
            switch (intr.intr.ty) {
                .cast => {
                    var v = (try self.expr(intr.args[0]));
                    const nusz = self.sizeOf(e.t);
                    // NOTE(09.06.26): maybe make casting part of Value's interface?
                    std.debug.assert(Value.isSizeSmol(nusz) and Value.isSizeSmol(v.size)); // for now..?
                    v.size = nusz;
                    return v;
                },
                .undefined => {
                    const buf = try Value.initOwnedAlloc(self.sizeOf(e.t), sal);
                    return buf;
                },
                .panic => {
                    const sval = try self.expr(intr.args[0]);
                    const cstr = sval.get().cstr;
                    const len = std.mem.len(cstr);
                    var s: []u8 = undefined;
                    s.ptr = cstr;
                    s.len = len;
                    @panic(s);
                },
                .@"size-of" => {
                    return Value.int(self.sizeOf(intr.args[0].t).size);
                },
                .@"offset-ptr" => {
                    // Should I make offset-ptr relative to type (C) or not (basado)?
                    // Nahhhhh
                    const ogPtr = try self.expr(intr.args[0]);
                    const amount = try self.expr(intr.args[1]);

                    std.debug.assert(ogPtr.smol());
                    std.debug.assert(amount.smol());
                    var ptr = ogPtr.get();
                    if (amount.get().int > 0) { // if pointer is null, @ptrFromInt produces an error. Offseting the null pointer by 0 is correct tho.
                        ptr.ptr = @ptrFromInt(@intFromPtr(ptr.ptr) + @as(usize, @intCast(amount.get().int)));
                    } else if (amount.get().int < 0) {
                        ptr.ptr = @ptrFromInt(@intFromPtr(ptr.ptr) - @as(usize, @intCast(-amount.get().int)));
                    }
                    return Value.initOwned(ptr, Size.ptr);
                },

                .argv => return Value.initOwned(.{ .extptr = @ptrCast(self.progArgs.ptr) }, Size.ptr),
                .argc => return Value.int(self.progArgs.len),

                .memeq => {
                    var l = try self.expr(intr.args[0]);
                    var r = try self.expr(intr.args[1]);

                    std.debug.assert(l.size.size == r.size.size);
                    return Value.bul(std.mem.eql(u8, l.getSlice(), r.getSlice()));
                },

                .errno => {
                    return Value.int(std.c._errno().*);
                },

                .@"register-signal" => {
                    const i = (try self.expr(intr.args[0])).get().i32;
                    const fun = (try self.expr(intr.args[1]));
                    try self.registerSignal(i, fun, intr.args[1].t);
                    return Value.enoom(0); // unit
                },

                .@"size-f64" => {
                    const i = (try self.expr(intr.args[0])).get().size;
                    return Value.float(@floatFromInt(i));
                },

                .@"f64-i64-floor" => {
                    const i = (try self.expr(intr.args[0])).get().float;
                    return Value.int(@as(i64, @intFromFloat(i)));
                },

                .@"i32-i64" => {
                    const i = (try self.expr(intr.args[0])).get().i32;
                    return Value.int(@as(i64, @intCast(i)));
                },

                .@"u32-bit-and" => {
                    const l = (try self.expr(intr.args[0])).get().u32;
                    const r = (try self.expr(intr.args[1])).get().u32;
                    return Value.int(l & r);
                },
                .@"u32-bit-or" => {
                    const l = (try self.expr(intr.args[0])).get().u32;
                    const r = (try self.expr(intr.args[1])).get().u32;
                    return Value.int(l | r);
                },
                .@"u32-bit-neg" => {
                    const i = (try self.expr(intr.args[0])).get().u32;
                    return Value.int(~i);
                },

                // TODO: generalize/automate.
                .@"i64-add", .@"i64-sub", .@"i64-mul", .@"i64-div" => {
                    const l = (try self.expr(intr.args[0])).get().int;
                    const r = (try self.expr(intr.args[1])).get().int;

                    return Value.int(switch (intr.intr.ty) {
                        .@"i64-add" => l + r,
                        .@"i64-sub" => l - r,
                        .@"i64-mul" => l * r,
                        .@"i64-div" => @divTrunc(l, r),
                        else => unreachable,
                    });
                },
                .@"u64-add", .@"u64-sub", .@"u64-mul", .@"u64-div" => {
                    const l = (try self.expr(intr.args[0])).get().u64;
                    const r = (try self.expr(intr.args[1])).get().u64;

                    return Value.int(switch (intr.intr.ty) {
                        .@"u64-add" => l + r,
                        .@"u64-sub" => l - r,
                        .@"u64-mul" => l * r,
                        .@"u64-div" => @divTrunc(l, r),
                        else => unreachable,
                    });
                },

                .@"i32-add", .@"i32-sub", .@"i32-mul", .@"i32-div" => {
                    const l = (try self.expr(intr.args[0])).get().i32;
                    const r = (try self.expr(intr.args[1])).get().i32;

                    return Value.int(switch (intr.intr.ty) {
                        .@"i64-add", .@"i32-add" => l + r,
                        .@"i64-sub", .@"i32-sub" => l - r,
                        .@"i64-mul", .@"i32-mul" => l * r,
                        .@"i64-div", .@"i32-div" => @divTrunc(l, r),
                        else => unreachable,
                    });
                },
                .@"u32-add", .@"u32-sub", .@"u32-mul", .@"u32-div" => {
                    const l = (try self.expr(intr.args[0])).get().u32;
                    const r = (try self.expr(intr.args[1])).get().u32;

                    return Value.int(switch (intr.intr.ty) {
                        .@"u32-add" => l + r,
                        .@"u32-sub" => l - r,
                        .@"u32-mul" => l * r,
                        .@"u32-div" => @divTrunc(l, r),
                        else => unreachable,
                    });
                },
                .@"u8-add", .@"u8-sub", .@"u8-mul", .@"u8-div" => {
                    const l = (try self.expr(intr.args[0])).get().u8;
                    const r = (try self.expr(intr.args[1])).get().u8;

                    return Value.int(switch (intr.intr.ty) {
                        .@"u8-add" => l + r,
                        .@"u8-sub" => l - r,
                        .@"u8-mul" => l * r,
                        .@"u8-div" => @divTrunc(l, r),
                        else => unreachable,
                    });
                },
                .@"size-add", .@"size-sub", .@"size-mul", .@"size-div" => {
                    const l = (try self.expr(intr.args[0])).get().size;
                    const r = (try self.expr(intr.args[1])).get().size;

                    return Value.initOwned(.{ .size = switch (intr.intr.ty) {
                        .@"size-add" => l + r,
                        .@"size-sub" => l - r,
                        .@"size-mul" => l * r,
                        .@"size-div" => @divTrunc(l, r),
                        else => unreachable,
                    } }, Size.of(usize));
                },
                .@"f64-add", .@"f64-sub", .@"f64-mul", .@"f64-div" => {
                    const l = (try self.expr(intr.args[0])).get().float;
                    const r = (try self.expr(intr.args[1])).get().float;

                    return Value.float(switch (intr.intr.ty) {
                        .@"f64-add" => l + r,
                        .@"f64-sub" => l - r,
                        .@"f64-mul" => l * r,
                        .@"f64-div" => l / r,
                        else => unreachable,
                    });
                },

                .@"i64-cmp" => {
                    const l = (try self.expr(intr.args[0])).get().int;
                    const r = (try self.expr(intr.args[1])).get().int;
                    if (l < r) {
                        return Value.enoom(0);
                    } else if (l == r) {
                        return Value.enoom(1);
                    } else {
                        return Value.enoom(2);
                    }
                },
                .@"u64-cmp" => {
                    const l = (try self.expr(intr.args[0])).get().u64;
                    const r = (try self.expr(intr.args[1])).get().u64;
                    if (l < r) {
                        return Value.enoom(0);
                    } else if (l == r) {
                        return Value.enoom(1);
                    } else {
                        return Value.enoom(2);
                    }
                },
                .@"i32-cmp" => {
                    const l = (try self.expr(intr.args[0])).get().i32;
                    const r = (try self.expr(intr.args[1])).get().i32;
                    if (l < r) {
                        return Value.enoom(0);
                    } else if (l == r) {
                        return Value.enoom(1);
                    } else {
                        return Value.enoom(2);
                    }
                },
                .@"u32-cmp" => {
                    const l = (try self.expr(intr.args[0])).get().u32;
                    const r = (try self.expr(intr.args[1])).get().u32;
                    if (l < r) {
                        return Value.enoom(0);
                    } else if (l == r) {
                        return Value.enoom(1);
                    } else {
                        return Value.enoom(2);
                    }
                },
                .@"u8-cmp" => {
                    const l = (try self.expr(intr.args[0])).get().u8;
                    const r = (try self.expr(intr.args[1])).get().u8;
                    if (l < r) {
                        return Value.enoom(0);
                    } else if (l == r) {
                        return Value.enoom(1);
                    } else {
                        return Value.enoom(2);
                    }
                },
                .@"size-cmp" => {
                    const l = (try self.expr(intr.args[0])).get().size;
                    const r = (try self.expr(intr.args[1])).get().size;
                    if (l < r) {
                        return Value.enoom(0);
                    } else if (l == r) {
                        return Value.enoom(1);
                    } else {
                        return Value.enoom(2);
                    }
                },
                .@"f64-cmp" => {
                    const l = (try self.expr(intr.args[0])).get().float;
                    const r = (try self.expr(intr.args[1])).get().float;

                    if (l < r) {
                        return Value.enoom(0);
                    } else if (l == r) {
                        return Value.enoom(1);
                    } else {
                        return Value.enoom(2);
                    }
                },
            }
        },
        .Int => |x| {
            const int = Value.int(x.int);
            const instFun = try self.getAndUnboxInstFunRef(x.ref);

            var args = [_]Value{int};
            const ret = try self.function(instFun.get().fun, &args);
            return ret;
        },
        .ConstSize => |sz| {
            return Value.int(sz);
        },
        .Float => |x| {
            return Value.float(x);
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

                .Equals, .NotEquals => |ref| {
                    const l = try self.expr(op.l);
                    const r = try self.expr(op.r);
                    // const size = self.sizeOf(op.l.t).size;
                    // const eqResult = std.mem.eql(u8, common.byteSlice(l.ref, size), common.byteSlice(r.ref, size));
                    // const boolVal = try self.boolValue(if (op.op == .Equals) eqResult else !eqResult);
                    const instFun = try self.getAndUnboxInstFunRef(ref);

                    var args = [_]Value{ l, r };
                    const eqResult = try self.function(instFun.get().fun, &args);

                    return switch (op.op) {
                        .Equals => eqResult,
                        .NotEquals => Value.bul(!isTrue(eqResult)),
                        else => unreachable,
                    };
                },

                // these ones don't short circuit, so we can simplify our structure.
                else => {
                    const l = try self.expr(op.l);
                    const r = try self.expr(op.r);
                    return switch (op.op) {
                        // .Equals => try self.boolValue(l.get().int == r.ref.int), // TEMP!!!

                        .Plus,
                        .Minus,
                        .Times,
                        .Divide,
                        .LessThan,
                        .LessEqualThan,
                        .GreaterThan,
                        .GreaterEqualThan,
                        => |ref| {
                            const instFun = try self.getAndUnboxInstFunRef(ref);

                            var args = [_]Value{ l, r };
                            const ret = try self.function(instFun.get().fun, &args);

                            const realRet = switch (op.op) {
                                .LessThan => Value.bul(ret.get().tag == 0),
                                .LessEqualThan => Value.bul(ret.get().tag != 2),
                                .GreaterThan => Value.bul(ret.get().tag == 2),
                                .GreaterEqualThan => Value.bul(ret.get().tag != 0),
                                else => ret,
                            };

                            return realRet;
                        },

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
                .TNum => |tnum| {
                    return self.getTNum(tnum);
                },
                .Var => |vv| {
                    return self.getVar(vv.v);
                },

                .Fun => |fun| {
                    // XXX(07.06.26): this is technically incorrect for functions that escape (since we should get the match from the union itself (which makes Match in variables kinda dumb???)).
                    //  - however it works, because we get the snapshot (including the tymap stack!) and store it together with the function.
                    return try self.initFunction(fun, try self.typeContext.mapMatch(self.tymap, v.match)); // mapMatch fixes some mapping issues due to 6_t01 test. NOTE: check if it's needed for ClassFun? Couldn't trigger it manually.
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
                    return Value.initOwned(.{ .extptr = nanbox(fun, .ExternalFunction) }, Size.ptr);
                },
            }
        },
        .Str => |slit| {
            const s: *anyopaque = @ptrCast(try self.evaluateString(slit));
            return Value.initOwned(.{ .extptr = s }, Size.ptr);
        },
        .Call => |c| {
            const fun = try self.expr(c.callee);
            const args = try self.exprs(c.args);
            return try self.call(fun, args, c.callee.t);
        },
        .Con => |con| {
            if (con.tys.len == 0) {
                return try Value.soleTag(con.tagValue, self.sizeOf(e.t), sal);
            } else {
                return Value.initOwned(.{ .confun = nanbox(con, .ConstructorFunction) }, Size.ptr);
            }
        },

        .UnOp => |uop| {
            const v = try self.expr(uop.e);
            return switch (uop.op) {
                .Deref => Value.initLValueFromRef(v.get().ptr, self.sizeOf(e.t)),
                .Ref => try v.ref(self.scope.allocator()),
                .Access => |mem| v.fieldOfValue(self.getFieldFromType(@constCast(&v).getRefToMemory(), uop.e.t, mem), self.sizeOf(e.t)), // NOTE(07.06.26): constCast - we know we won't modify the v itself, so it's okay.
                .Update => |upd| b: {
                    var vv = try v.copyTo(sal);
                    for (upd) |field| {
                        const vp = self.getFieldFromType(vv.getRefToMemory(), uop.e.t, field.field);
                        const fieldval = try self.expr(field.value);
                        const sz = self.sizeOf(field.value.t);
                        @memcpy(vp.slice(sz.size), @constCast(&fieldval).getSlice()); // NOTE(07.06.26): constCast - it's okay, we're copying FROM it.
                    }

                    break :b vv;
                },
                .As => v,
                .Not => Value.bul(!isTrue(v)),
                .Negate => |inst| b: {
                    const instFun = try self.getAndUnboxInstFunRef(inst);
                    var args = [_]Value{v};
                    const res = try self.function(instFun.get().fun, &args);
                    break :b res;
                },
                .ElementAccess => |ea| b: {
                    const idx = try self.expr(ea.index);
                    const instFun = try self.getAndUnboxInstFunRef(ea.access);
                    var args = [_]Value{ v, idx };
                    const res = try self.function(instFun.get().fun, &args);
                    break :b res;
                },
            };
        },

        .AnonymousRecord => |recs| {
            // THIS ASSUMES THAT RECORDS GET THAT `setType` TREATMENT
            // ALSO, COPYPASTA FROM `initRecord`. MAYBE WE CAN GENERALIZE IT SOMEHOW?
            var buf = try Value.initOwnedAlloc(self.sizeOf(e.t), sal);
            var stream = std.io.fixedBufferStream(buf.getSlice());
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
                            for (fields) |afield| {
                                const field = afield.rec;
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
                .TyVar => |tyv| {
                    if (self.typeContext.getFieldsForTVar(tyv)) |tyvs| {
                        if (!tyvs.total) unreachable;

                        // order fields according to type (SLOW)
                        for (tyvs.fields) |field| {
                            for (recs) |rec| {
                                if (common.streq(field.field, rec.field)) {
                                    _ = try self.writeExpr(w, rec.value);
                                    break;
                                }
                            } else unreachable;
                        }
                    } else {
                        unreachable;
                    }
                },
                else => unreachable,
            }

            return buf;
        },

        .NamedRecord => |nrec| {

            // THIS ASSUMES THAT RECORDS GET THAT `setType` TREATMENT
            // ALSO, COPYPASTA FROM `initRecord`. MAYBE WE CAN GENERALIZE IT SOMEHOW?
            // SECOND GRADE COPYPASTA FROM `AnonymousRecord`
            var buf = try Value.initOwnedAlloc(self.sizeOf(e.t), sal);
            var stream = std.io.fixedBufferStream(buf.getSlice());
            const w = stream.writer();

            const fields = nrec.data.stuff.recs;

            // order fields according to type (SLOW)
            for (fields) |afield| {
                const field = afield.rec;
                for (nrec.fields) |rec| {
                    if (common.streq(field.field, rec.field)) {
                        _ = try self.writeExpr(w, rec.value);
                        break;
                    }
                } else unreachable;
            }

            return buf;
        },

        .Lam => |*l| {
            const ptr = nanbox(try common.allocOne(self.arena, SmolValue.Lam{
                .lam = l,
                .env = try self.initEnvSnapshot(l.env, null),
            }), .Lambda);
            return Value.initOwned(.{ .lam = ptr }, Size.ptr);
        },

        .StaticArray => |arr| {
            if (arr.len == 0) {
                // I think this is correct, since we ain't gonna be accessing any elements.
                // size should also be zero!
                return Value.initOwned(.{ .ref = @ptrFromInt(0xabcdabcdabcdabcd) }, Size{});
            }

            const elemSize = self.sizeOf(arr[0].t);
            var array = try Value.initOwnedAlloc(elemSize.times(arr.len), sal);
            for (arr, 0..) |elem, i| {
                const ret = try self.expr(elem);
                @memcpy(array.getSlice()[i * elemSize.size .. (i + 1) * elemSize.size], @constCast(&ret).getSlice());
            }

            return array;
        },

        .IfElse => |ifelse| {
            const cond = try self.expr(ifelse.cond);
            if (isTrue(cond)) {
                return try self.expr(ifelse.ifTrue);
            } else {
                for (ifelse.ifOthers) |elif| {
                    if (isTrue(try self.expr(elif.cond))) {
                        return try self.expr(elif.then);
                    }
                }
                return try self.expr(ifelse.ifFalse);
            }
        },

        .CaseExpr => |sw| {
            var switchOn = try self.expr(sw.switchOn);
            for (sw.cases) |case| {
                switch (case) {
                    .Expr => |cexpr| {
                        if (try self.tryDeconstruct(cexpr.decon, switchOn.getRefToMemory())) {
                            return try self.expr(cexpr.expr);
                        }
                    },
                    .Case => unreachable, // TODO!
                }
            } else {
                const l = sw.switchOn.l;
                var hadNewline = false;
                const ctx = ast.Ctx.init(&hadNewline, self.typeContext);
                (errr.Error.ErrCtx{
                    .module = l.module,
                    .c = ctx,
                }).atLocation(l, .{ .label = .{"miau"} });
                return error.CaseNotMatched;
            }
        },
    }

    unreachable;
}

const TypeVal = struct {
    t: ast.Type,
    v: Value,
};

// TODO: nocheckin we have to make sure we un-lvalue the arguments passed to the function to make sure we don't modify the og args. Should we do it here or at callsite? check for getRef()
fn call(self: *Self, fun: Value, cargs: []TypeVal, cfunTy: ast.Type) !Value {
    const ptrAndfunType = nunbox(fun.get().extptr); // funny, but in this case we *know* the inner value is a pointer, which just don't know what to. So, we use extptr (kind of incorrectly) as a "general pointer".
    const ptrToFunPtr: *align(1) const RawValue = @ptrCast(&ptrAndfunType.ptr); // and later get the address, so we can do the .<correct-type>
    const funType = ptrAndfunType.fty;

    const sal = self.alCurStack.?;
    switch (funType) {
        .ExternalFunction => {
            const funTys = self.typeContext.getType(cfunTy).Fun;
            const ffiFunPtr: *const fn (...) callconv(.C) i64 = @ptrCast(ptrToFunPtr.smol.extptr);

            var func: ffi.Function = undefined;
            const paramFFITypes = try sal.alloc(*ffi.Type, funTys.args.len);
            // defer self.arena.free(paramFFITypes); // lifetime signature

            const args = try sal.alloc(*anyopaque, funTys.args.len);
            // defer self.arena.free(args); // lifetime signature

            for (args, paramFFITypes, cargs) |*arg, *param, *exp| {
                const v = &exp.v;
                std.debug.assert(v.smol()); // TEMP(07.06.26): this is temporary. we can't really pass anything other yet to ffi.
                arg.* = @ptrCast(v.getRefToMemory());
                param.* = try self.sizeOfFFI(exp.t);
            }

            defer {
                for (paramFFITypes) |param| {
                    self.freeFFIType(param);
                }
            }

            const ret = try self.sizeOfFFI(funTys.ret);
            defer self.freeFFIType(ret);

            try func.prepare(ffi.Abi.default, @intCast(args.len), paramFFITypes.ptr, ret);

            var result = try Value.initOwnedAlloc(self.sizeOf(funTys.ret), sal);

            func.call(ffiFunPtr, args.ptr, result.getRefToMemory());

            return result;
        },

        .LocalFunction => {
            return try self.function_(ptrToFunPtr.smol.fun, TypeVal, cargs, struct {
                fn mapFn(ref: TypeVal) Value {
                    return ref.v;
                }
            }.mapFn);
        },

        .ConstructorFunction => {
            const cretTy = self.getType(cfunTy).Fun.ret;
            return self.initRecord(ptrToFunPtr.smol.confun, cargs, cretTy);
        },

        .Lambda => {
            const lamAndEnv = ptrToFunPtr.smol.lam;

            // begin new scope
            const oldScope = self.scope;
            var scope = Scope.init(oldScope, self.alBase);
            self.scope = &scope;
            defer {
                self.scope = oldScope;
                scope.deinit();
            }

            // also, don't forget the new tymap!
            const env = lamAndEnv.env;

            // this is also copypasta :)
            std.debug.assert(env.lastPtr.* == null); // lambda CANNOT recurse like a function, so lastPtr should always be null.
            const oldTyMap = self.tymap;
            env.lastPtr.* = self.tymap;
            self.tymap = env.tymaps;
            defer {
                self.tymap = oldTyMap;
                env.lastPtr.* = null;
            }

            // COPYPASTA WARNING
            const lam = lamAndEnv.lam;
            for (env.vars) |ee| {
                switch (ee) {
                    .Snap => |eee| try self.putVar(eee.v, eee.vv),
                    .AssocID => |id| {
                        const efun = self.tymap.tryGetFunctionByID(id).?;
                        const efunv = try self.initFunction(efun.fun, efun.m);
                        try self.putVar(efun.fun.name, efunv);
                    },
                }
            }

            for (lam.params, cargs) |decon, *a| {
                if (!try self.tryDeconstruct(decon.d, a.v.getRefToMemory())) {
                    return error.CaseNotMatched;
                }
            }

            switch (lam.body) {
                .Expr => |exp| {
                    const ret = try self.expr(exp);

                    // BUG(return-decon-ref): Remember to copy the value before returning - when returning a deconstructed value, it's possible to return a ref - in this case, we must copy it.
                    return ret.copyTo(sal); // a return value must not be an lvalue!
                },
                .Body => |bod| {
                    // COPYPASTA
                    self.stmts(bod.stmts) catch |err| switch (err) {
                        error.Return => {
                            return try self.returnValue.copyTo(sal);
                        },
                        error.Break => {
                            std.debug.print("TRIED TO BREAK OUT OF LAMBDA FUNCTION\n", .{});
                            return error.Bruh;
                        },
                        else => return err,
                    };

                    // return statements

                    unreachable;
                },
            }
        },

        .None => unreachable,
    }
}

// TODO(07.06.26): I forgot about this. also make it use the sizer version.
// this can also return a value?
fn getFieldFromType(self: *Self, v: RawValueRef, t: ast.Type, mem: Str) RawValueRef {
    switch (self.getType(t)) {
        .Anon => |fields| {
            return self.getFieldFromFields(v, fields, mem);
        },
        .TyVar => |tyv| {
            if (self.typeContext.getFieldsForTVar(tyv)) |tyvs| {
                if (!tyvs.total) unreachable;
                return self.getFieldFromFields(v, tyvs.fields, mem);
            } else {
                unreachable;
            }
        },
        .Con => |con| {
            // map it like function or sizeOf for cons!
            const oldTyMap = self.tymap;

            // NOTE: COPYPASTA
            // FIXES INFINITE LOOP for functions that operate on datatypes which have outer tvars.
            // VERY HACKY.
            // basically, when we're in a function which defines the tvars, the c.outerApplication has the tvar itself as its value
            // so if we don't get the value of the tvar first, we get an infinite loop.
            // maybe there is a better way which does not require allocation?
            const outerApplication = self.alBase.alloc(ast.TypeOrNum, con.outerApplication.len) catch unreachable; // TEMP
            defer self.alBase.free(outerApplication);
            for (0..outerApplication.len) |i| {
                const app = con.outerApplication[i];
                switch (app) {
                    .Type => |appt| {
                        outerApplication[i] = .{
                            .Type = switch (self.typeContext.getType(appt)) {
                                .TVar => |tv| oldTyMap.mapTVar(tv) orelse appt, // very bad!! xddd
                                else => appt,
                            },
                        };
                    },
                    .Num => |appnum| {
                        outerApplication[i] = .{
                            .Num = switch (self.typeContext.getNum(appnum)) {
                                .TNum => |tnum| oldTyMap.mapTNum(tnum) orelse appnum,
                                else => appnum,
                            },
                        };
                    },
                }
            }
            const outerTVScheme = ast.Scheme{
                .tvars = con.type.outerTVars,
                .envVars = &.{},
                .associations = &.{},
            };
            const outerTVMatch = ast.Match{
                .tvars = outerApplication,
                .envVars = &.{},
                .assocs = &.{},
                .scheme = outerTVScheme,
            };
            const outerTVMap = TypeMap{
                .prev = oldTyMap,
                .scheme = &outerTVScheme,
                .match = &outerTVMatch,
            };

            var tymap = TypeMap{
                .prev = &outerTVMap,
                .scheme = &con.type.scheme,
                .match = con.application,
            };
            self.tymap = &tymap;
            defer self.tymap = oldTyMap;

            switch (con.type.stuff) {
                .recs => |recs| return self.getFieldFromFields_(v, ast.Data.DecRecord, recs, mem, ast.Data.DecRecord.mapRecord),
                .cons => unreachable,
            }
        },

        else => unreachable,
    }
}

fn getFieldFromFields(self: *Self, v: RawValueRef, fields: []ast.Record, mem: Str) RawValueRef {
    return self.getFieldFromFields_(v, ast.Record, fields, mem, common.id(ast.Record));
}

fn getFieldFromFields_(self: *Self, v: RawValueRef, comptime T: type, fields: []T, mem: Str, comptime acc: fn (T) ast.Record) RawValueRef {
    var size: usize = 0;
    for (fields) |afield| {
        const field = acc(afield);
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

fn quickCallInst(self: *Self, assocRef: ast.InstFunInst, args: []const Value) !Value {
    const instFun = try self.getAndUnboxInstFunRef(assocRef);
    const res = try self.function(instFun.get().fun, args);
    return res;
}

fn getAndUnboxInstFunRef(self: *Self, assocRef: ast.InstFunInst) !Value {
    const instFunBoxed = switch (assocRef.*.?) {
        .Id => |uid| b: {
            const instfun = self.tymap.tryGetFunctionByID(uid).?;
            break :b try self.initFunction(instfun.fun, instfun.m);
        },
        .InstFun => |instfun| try self.initFunction(instfun.fun, instfun.m),
    };

    std.debug.assert(instFunBoxed.size.size == Size.ptr.size);

    const ubfun = nunbox(instFunBoxed.get().fun);
    std.debug.assert(ubfun.fty == .LocalFunction);
    const instFunUnboxed = Value.initOwned(.{ .fun = ubfun.ptr }, instFunBoxed.size);
    return instFunUnboxed;
}

fn initFunction(self: *Self, fun: *ast.Function, m: *const ast.Match) !Value {
    // NOTE(07.06.26): what should I do with this?
    const funThing = nanbox(try common.allocOne(self.arena, SmolValue.Fun{
        .fun = fun,
        .env = self.scope.getFun(fun.name),
        .match = m,
    }), .LocalFunction);
    return Value.initOwned(.{ .fun = funThing }, Size.ptr);
}

fn function(self: *Self, funAndEnv: *SmolValue.Fun, args: []const Value) Err!Value {
    return try self.function_(funAndEnv, Value, args, common.id(Value));
}
fn function_(self: *Self, funAndEnv: *SmolValue.Fun, comptime Arg: type, args: []const Arg, comptime mapFun: fn (Arg) Value) Err!Value {
    // begin new scope
    const oldScope = self.scope;
    var scope = Scope.init(oldScope, self.alBase);
    self.scope = &scope;
    defer {
        self.scope = oldScope;
        scope.deinit();
    }

    // first setup the environment type mapping.
    // TOOD: here + self.function(). probably should factor it out?
    // but it's using defer, so not really. but the lastPtr setting is sussy.
    const env = funAndEnv.env;
    const oldTyMap = self.tymap;
    defer self.tymap = oldTyMap;

    // in recursive functions, we might apply the same copy twice. In this case, we don't want to apply it again, because this will produce an infinite loop AND it's already accessible, so we should not re-add it.
    const alreadyApplied = env.lastPtr.* != null;
    if (!alreadyApplied) {
        self.tymap = env.tymaps;
        env.lastPtr.* = oldTyMap;
    }
    defer if (!alreadyApplied) {
        // previous defer takes care of it.
        env.lastPtr.* = null;
    };

    // also new typemap yo.
    const match = funAndEnv.match;
    self.tymap = &try TypeMap.initMap(match, self.typeContext, self.tymap);
    // defer self.tymap = oldTyMap;
    // no need to defer here. Previous defer will reset.

    const fun = funAndEnv.fun;
    for (env.vars) |e| {
        switch (e) {
            .Snap => |ee| try self.putVar(ee.v, ee.vv),
            .AssocID => |id| {
                const efun = self.tymap.tryGetFunctionByID(id).?;
                const efunv = try self.initFunction(efun.fun, efun.m);
                try self.putVar(efun.fun.name, efunv);
            },
        }
    }

    for (fun.params, args) |decon, ar| {
        const a = mapFun(ar);
        var av = try a.copyWithValue(scope.allocator()); // we can pass in a "ref", so make sure to copy the actual value.
        if (!try self.tryDeconstruct(decon.d, av.getRefToMemory())) {
            return error.CaseNotMatched;
        }
    }

    for (match.scheme.tvars, match.tvars) |tvOrNum, mtvOrNum| {
        switch (tvOrNum) {
            .TVar => {},
            .TNum => |tnum| {
                try self.putTNum(
                    tnum,
                    switch (self.typeContext.getNum(mtvOrNum.Num)) {
                        .Literal => |lit| lit,
                        .TNum => |tnum2| self.getTNum(tnum2).get().size,
                        .Unknown => unreachable,
                    },
                );
            },
        }
    }

    self.stmts(fun.body) catch |err| switch (err) {
        error.Return => {
            return try self.returnValue.copyTo(self.alCurStack.?);
        },
        error.Break => {
            std.debug.print("TRIED TO BREAK OUT OF FUNCTION\n", .{});
            return error.Bruh;
        },
        else => return err,
    };

    unreachable; // NOTE: Return should be automatically added while parsing bruv ㅠㅠㅠ
}

fn evaluateString(self: *Self, s: Str) ![:0]u8 {
    const res = try self.constStrings.getOrPut(s.ptr);
    if (res.found_existing) {
        return res.value_ptr.*;
    } else {
        res.value_ptr.* = try self.arena.dupeZ(u8, s);
        return res.value_ptr.*;
    }
}

const ExprStack = struct {
    arena: std.heap.ArenaAllocator,
    oldStackAllocator: ?std.mem.Allocator,
};
fn beginStack(self: *Self, es: *ExprStack) void {
    if (ArenaAlloc) return;
    es.*.arena = std.heap.ArenaAllocator.init(self.alBase);
    es.*.oldStackAllocator = self.alCurStack;
    self.alCurStack = es.arena.allocator();
}
fn restoreStack(self: *Self, es: *ExprStack) void {
    if (ArenaAlloc) return;
    es.arena.deinit();
    self.alCurStack = es.oldStackAllocator;
}

const RawValueRef = *align(1) RawValue; // basically, because we don't know *where* the basic data might be, we must assume its offset can be whatever (imagine: struct with  three chars and getting a reference to one of them)
const RawValueConstRef = *align(1) const RawValue;

// shit name: basically the value + metadata.
const Value = struct {
    val: union(enum) {
        // should I differentiate Stack and Smol?
        // technically, when I get members of stack structs, the inner part can be smaller. and if it's not an lvalue, we can just copy it inside. but is it needed??? ah shid, whatever. I can just copy it, whatever  (although it IS funny.... we are already reinterpreting bytes tbh...)
        // VERDICT: if offseting inside a small sized value, just copy it inside the value.
        Owned: SmolValue,
        LValue: *align(1) RawValue,
    },
    size: sizer.Size,

    fn isLValue(self: *const @This()) bool {
        return switch (self.val) {
            .LValue => true,
            else => false,
        };
    }

    fn isSizeSmol(sz: Size) bool {
        return sz.size <= @sizeOf(SmolValue);
    }
    fn smol(self: *const @This()) bool {
        std.debug.assert(@sizeOf(SmolValue) <= 8);
        return isSizeSmol(self.size);
    }

    // transisitions:
    // LValue -> LValue
    // Owned(&Smol.val) -> LValue
    // Owned(Stack.val) -> LValue
    // ----
    // LValue.val& -> Owned(Smol)
    // LValue.val -> Owned(Stack)
    // ----
    // &(LValue): Lvalue.val -> Smol
    // &(Stack): alloc(stack only); Stack.val -> Smol
    // &(Smol): alloc(smolvalue); Ptr(Smol.val) -> Smol
    // ----
    // LValue&: (reference  (SmolValue*) to a smol value which is a pointer): { Owned { ref: LValue&.ptr } }
    // Stack/Smol&: (must be a "smol" value, cuz it's a pointer!): { Ownder { ref: Owned.val.ptr }}
    // ----
    // LValue.a: initfromref()  (should also create lvalue)
    // Stack.a: initfromref(): (should create either stack or smol value)
    // Smol.a: initfromref(): (same as stack, although we know it's gonna be smol)

    fn lvalue(refd: *@This()) Value {
        const r = refd.getRefToMemory();
        return Value{ .val = .{ .LValue = r }, .size = refd.size };
    }

    fn get(self: @This()) SmolValue {
        switch (self.val) {
            .Owned => |own| return own,
            .LValue => |lv| {
                if (self.smol()) {
                    return lv.smol;
                } else {
                    return SmolValue{ .ref = lv };
                }
            },
        }
    }

    fn getRefToMemory(self: *@This()) RawValueRef {
        switch (self.val) {
            .LValue => |lv| return lv,
            .Owned => |*own| {
                if (self.smol()) {
                    return @ptrCast(own);
                } else {
                    return own.ref;
                }
            },
        }
    }

    fn getConstRefToMemory(self: *const @This()) RawValueConstRef {
        return @constCast(@constCast(self).getRefToMemory());
    }

    // PTR

    // this one allocates cuz it's tricky to get right.
    fn ref(self: @This(), al: std.mem.Allocator) !Value {
        switch (self.val) {
            // NOTE(06.07.26): is this correct? when it's an lvalue, we keep the same thing, but when it's owned, we... copy it?
            //    is it because one might not be single expression-local? (a scope allocator instead of an expr. allocator?)
            .LValue => |lv| return Value.initOwned(.{ .ptr = lv }, Size.ptr),
            .Owned => {
                const sz = self.size;
                const space = try al.alloc(u8, sz.size);
                @memcpy(space, self.getConstRefToMemory().constSlice(sz.size));
                return Value.initOwned(.{ .ptr = @ptrCast(space) }, Size.ptr);
            },
        }
    }

    // for use in Interpreter function calls and such. does not allocate memory n stuff.
    fn ptrToOwned(self: *@This()) Value {
        return Value.initOwned(.{ .ptr = self.getRefToMemory() }, Size.ptr);
    }

    fn ptrFromRef(r: RawValueRef) Value {
        return Value.initOwned(.{ .ptr = r }, Size.ptr);
    }

    // DEREF
    fn deref(self: @This(), nuSize: Size) Value {
        std.debug.assert(self.smol());
        switch (self.val) {
            .LValue => |lv| return Value.initLValueFromRef(lv.smol.ptr, nuSize),
            .Owned => |own| return Value.initOwnedFromRef(own.ptr, nuSize),
        }
    }

    // FIELD OF

    fn fieldOfValue(og: *const @This(), refd: RawValueRef, nuSize: Size) Value {
        if (og.size.size == nuSize.size) return og.*; // smol optimization for single field structs.

        std.debug.assert(nuSize.size < og.size.size);
        return switch (og.val) {
            .LValue => Value.initLValueFromRef(refd, nuSize),

            .Owned => Value.initOwnedFromRef(refd, nuSize),
        };
    }

    // MISC
    //
    fn getRefFromLValue(self: *const @This()) RawValueRef {
        std.debug.assert(self.isLValue());
        return Value.getRefToMemory(@constCast(self));
    }

    fn getSlice(self: *@This()) []u8 {
        const p: [*]u8 = @ptrCast(self.getRefToMemory());
        var sl: []u8 = undefined;
        sl.ptr = p;
        sl.len = self.size.size;
        return sl;
    }

    // if it's an lvalue, keep it lvalue. if it's stack, keep it stack. if it's smol, keep it smol.

    fn copyToRef(v: *const @This(), r: RawValueRef) void {
        @memcpy(r.slice(v.size.size), v.getConstRefToMemory().constSlice(v.size.size));
    }

    // note: if smol, does not allocate anything
    fn copyTo(v: *const @This(), al: std.mem.Allocator) !Value {
        var nu = try Value.initOwnedAlloc(v.size, al);
        v.copyToRef(nu.getRefToMemory());
        std.debug.assert(!nu.isLValue());
        return nu;
    }

    fn copyWithValue(self: *const @This(), al: std.mem.Allocator) !*Value {
        const val = try self.copyTo(al);
        return try common.allocOne(al, val);
    }

    fn copyToValue(self: *const @This(), space: *Value) !*Value {
        space.* = self;
        unreachable;
    }

    fn copyToLValue(self: *const @This(), lval: *const @This()) void {
        std.debug.assert(lval.isLValue());
        std.debug.assert(self.size.size == lval.size.size);

        @memcpy(lval.val.LValue.slice(self.size.size), (@constCast(self)).getSlice());
    }

    // HACK HACK HACK
    fn isMaybeNone(self: *const @This()) bool {
        if (self.smol()) {
            return self.get().tag == 0; // probably not possible. (it IS possible, Maybe I32)
        } else {
            return self.get().ref.adt.tag == 0;
        }
    }

    // constructors
    fn initOwned(val: SmolValue, sz: Size) @This() {
        const v = @This(){
            .val = .{ .Owned = val },
            .size = sz,
        };
        return v;
    }

    fn initOwnedFromRef(r: RawValueRef, sz: Size) @This() {
        if (Value.isSizeSmol(sz)) {
            const bytes = r.constSlice(sz.size);
            var val = Value.initOwned(.{ .size = 0 }, sz);
            std.debug.assert(val.smol());
            @memcpy(val.getSlice(), bytes);
            return val;
        } else {
            return Value.initOwned(.{ .ref = r }, sz);
        }
    }

    fn initLValueFromRef(r: RawValueRef, sz: Size) @This() {
        return Value{ .val = .{ .LValue = r }, .size = sz };
    }

    fn initOwnedAlloc(sz: Size, al: std.mem.Allocator) !@This() {
        if (Value.isSizeSmol(sz)) {
            return .{
                .val = .{ .Owned = .{ .size = 0 } }, // zeroes out automatically just in case.
                .size = sz,
            };
        } else {
            const mem = try al.alloc(u8, sz.size);
            return .{
                .val = .{ .Owned = .{ .ref = @alignCast(@ptrCast(mem.ptr)) } },
                .size = sz,
            };
        }
    }

    fn int(i: anytype) Value {
        // var ty = @TypeOf(i);
        // comptime if (Size.of(ty).size == 0) {
        //     @panic("what");
        // };
        const size = Size.ofType(@TypeOf(i));
        std.debug.assert(Value.isSizeSmol(size));

        if (i < 0) {
            return Value.initOwned(.{ .int = @intCast(i) }, size);
        } else {
            return Value.initOwned(.{ .size = @intCast(i) }, size);
        }
    }

    // basic enum-tag thing.
    fn enoom(i: sizer.Tag) Value {
        return int(i);
    }

    fn float(f: f64) Value {
        return Value.initOwned(.{ .float = f }, Size.ofType(f64));
    }

    fn bul(b: bool) Value {
        return Value.initOwned(.{ .u32 = if (b) 1 else 0 }, Size.tag);
    }

    fn soleTag(e: sizer.Tag, size: Size, al: std.mem.Allocator) !Value {
        var val = try initOwnedAlloc(size, al);
        if (val.smol()) {
            val.val.Owned.tag = e;
        } else {
            val.val.Owned.ref.adt.tag = e;
        }
        return val;
    }
};

const SmolValue = extern union {
    int: i64,
    i32: i32,
    u32: u32,
    u64: u64,
    u8: u8,
    size: usize,
    float: f64,
    char: u8,
    extptr: *anyopaque,
    ptr: RawValueRef,
    ref: RawValueRef,
    stack: RawValueRef,
    refbytes: [*]u8,
    fun: *Fun,
    confun: *ast.Con,
    lam: *const Lam,
    tag: u32, // for enum-like data structures
    cstr: [*:0]u8,

    const Lam = struct {
        lam: *const ast.Expr.Lam,
        env: EnvSnapshot,
    };

    const Fun = struct {
        fun: *ast.Function,
        env: EnvSnapshot,
        match: *const ast.Match,
    };

    // if (@sizeOf(SmolValue) > @sizeOf(*anyopaque)) @panic("too big :)"),
};

// values n shit
// note, that we must preserve the inner representation for compatibility with external functions.
// note about alignments: in C structs the alignments are variable (because we might represent different structs), so everything must be align(1)
const RawValue = extern union {
    smol: SmolValue,
    record: Flexible, // one constructor / record type
    adt: extern struct {
        tag: u32,
        data: Flexible,
    },

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

    fn constSlice(self: *align(1) const @This(), sz: usize) []const u8 {
        var sl: []const u8 = undefined;
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

const EnvSnapshot = struct {
    const VarSnapshot = union(enum) {
        // i forgot what it was gegegeg
        Snap: struct { v: ast.Var, vv: Value },
        AssocID: ast.Association.ID,
    };

    vars: []VarSnapshot,

    // very hacky :)
    // TODO: think about how to preserve typing context.
    tymaps: *TypeMap,
    lastPtr: *?*const TypeMap,
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

// fn copyValue(self: *Self, vt: RawValueRef, t: ast.Type) !RawValueRef {
//     // TODO: how do we retrieve functionType??
//     //  that points to a deeper problem of storing functions in datatypes... bruh.
//     //   1. Ignore for now. Just set it to null and just don't put functions in datatypes.
//     //   1.5. Ignore for now. Just store extra data in the packed union on second field. Incorrect offsets for C code tho, so you may not pass structs with external functions.
//     //   2. it's possible to solve this by packing shit into pointer's high bits. nan boxing. Now we can pass structs with functions in C. Obviously, C code trying to call our function will fail, because it's interpreted.
//     //   3. complete and utter possibility. instead of differentiating pointers, save the interpreter context SOMEWHERE, then this function would somehow retrieve it and run.
//     //      Problem 1: How to access context? We may not be able to provide it (eg. atexit) So, the interpreter state must be at some known location.
//     //      Problem 2: How to know which function it should execute? We might use global state if we solve Problem 1, but maybe there is a better way? Like, I need to differentiate the calls somehow. Slightly different pointer?

//     // currently at 2!
//     const sz = self.sizeOf(t);
//     const memptr: []u8 = try self.arena.alloc(u8, sz.size);
//     @memcpy(memptr[0..sz.size], @as([*]u8, @ptrCast(vt))[0..sz.size]);
//     const vptr: RawValueRef = @alignCast(@ptrCast(memptr.ptr));
//     return vptr;
// }

fn valArrayGet(ptr: *anyopaque, elemSize: usize, i: usize) RawValueRef {
    return @ptrCast(@as([*]u8, @ptrCast(ptr)) + elemSize * i);
}

fn initRecord(self: *Self, c: *ast.Con, args: []TypeVal, t: ast.Type) !Value {
    // alignment:
    // https://youtu.be/E0QhZ6tNoR  <= "alignment" is actually a place where values can live.
    // I get it, but why (in the video example) the trailing padding is aligned to 8? because of last member?
    // I think it gets padded to the largest struct.
    //   (watch out: it's incomplete, because from some Zig issue I've seen, i128 padding might be 8)
    //  nested structs do not create "big alignments". If the struct's max alignment was 8, it gets carred to the outer struct.
    var buf = try Value.initOwnedAlloc(self.sizeOf(t), self.alCurStack.?);
    var stream = std.io.fixedBufferStream(buf.getSlice());
    var w = stream.writer();

    var maxAlignment: usize = 1;
    if (c.data.structureType() == .ADT) {
        try w.writeInt(u32, c.tagValue, endianness);
        maxAlignment = 4;
    }

    // remember: check bytes written with `w.context.items.len`
    for (args) |a| {
        const alignment = try self.writeVal(w, a);

        maxAlignment = @max(maxAlignment, alignment);
    }

    // write ending padding (from experiments it's based on max padding.)
    try pad(w, maxAlignment);

    return buf;
}

fn writeExpr(self: *Self, w: anytype, a: *ast.Expr) !usize {
    return try self.writeVal(w, .{ .v = try self.expr(a), .t = a.t });
}

fn writeVal(self: *Self, w: anytype, a: TypeVal) !usize {
    const ty = a.t;
    const sz = self.sizeOf(ty);
    try pad(w, sz.alignment);
    var v = a.v;
    try w.writeAll(v.getSlice());

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

fn sizeOfFFI(self: *Self, t: ast.Type) !*ffi.Type {
    switch (self.getType(t)) {
        .Con => |c| {
            if (c.type.eq(self.prelude.defined(.Ptr))) {
                return ffi.types.pointer;
            }

            if (c.type.eq(self.prelude.defined(.I32))) {
                return ffi.types.sint32;
            }

            if (c.type.eq(self.prelude.defined(.Size))) {
                return ffi.types.ulong;
            }

            if (c.type.eq(self.prelude.defined(.F64))) {
                return ffi.types.double;
            }

            if (c.type.eq(self.prelude.defined(.ConstStr))) {
                return ffi.types.pointer;
            }

            if (ast.Annotation.find(c.type.annotations, "ctype")) |ann| {
                // bruh. maybe I should just put void in prelude?
                if (common.streq(ann.params[0], "void")) {
                    return ffi.types.void;
                }

                if (common.streq(ann.params[0], "void*")) {
                    return ffi.types.pointer;
                }
            }

            if (ast.Annotation.find(c.type.annotations, "cstruct")) |_| {
                // allocate a new type bruh.
                var x: *ffi.Type = try common.allocOne(self.alBase, std.mem.zeroes(ffi.Type));
                x.id = .@"struct";

                std.debug.assert(c.type.scheme.isEmpty()); // we don't care about polymorphic stuff now.
                switch (c.type.stuff) {
                    .cons => @panic(c.type.name), // not yet needed.
                    .recs => |recs| {
                        x.elements = try self.alBase.allocSentinel(?*ffi.Type, recs.len, null);
                        for (recs, 0..) |rec, i| {
                            x.elements.?[i] = try self.sizeOfFFI(rec.rec.t);
                        }
                    },
                }
                return x;
            }

            // TEMP TEMP TEMP
            {
                if (common.streq(c.type.name, "U8")) {
                    return ffi.types.uint8;
                }

                if (common.streq(c.type.name, "I64")) {
                    return ffi.types.sint64;
                }

                if (common.streq(c.type.name, "U32")) {
                    return ffi.types.uint32;
                }

                if (common.streq(c.type.name, "U64")) {
                    return ffi.types.uint64;
                }

                if (common.streq(c.type.name, "AsciiChar")) {
                    return ffi.types.schar;
                }

                if (common.streq(c.type.name, "WChar")) {
                    return ffi.types.uint32; // on tha leenuxes
                }

                if (common.streq(c.type.name, "Char")) {
                    @panic("tried to ffi Char (but the definition changed.)");
                }

                if (common.streq(c.type.name, "CInt")) {
                    return ffi.types.sint;
                }

                if (common.streq(c.type.name, "Bool")) {
                    return ffi.types.sint;
                }

                if (common.streq(c.type.name, "ExecVpArgv")) {
                    return ffi.types.pointer;
                }
            }

            @panic(c.type.name);
        },
        else => unreachable,
    }
}

fn freeFFIType(self: *Self, ffiType: *ffi.Type) void {
    if (ffiType.id == .@"struct") {
        const elems = ffiType.elements.?;
        var i: usize = 0;
        while (elems[i]) |elem| : (i += 1) {
            self.freeFFIType(elem);
        }

        self.alBase.free(ffiType.elements.?[0 .. i + 1]);
    }
}

// calculates total size of the record (including tag)
//  size includes alignment!
//  VERY SLOW, BECAUSE IT RECALCULATES ALIGNMENT EACH TIME.
//  BUG: works for Ints only accidentally, since i64 and ptr have the same size. FIXIT!
const Sizes = sizer.Size;
fn sizeOf(self: *Self, t: ast.Type) Sizes {
    var ts = sizer.TypeSize.init(self.typeContext, &self.prelude, self.tymap, self.alBase);
    return ts.sizeOf(t);
}

fn getType(self: *const Self, ogt: ast.Type) ast.TypeF(ast.Type) {
    return switch (self.typeContext.getType(ogt)) {
        .TVar => |tv| self.getType(self.tymap.mapTVar(tv) orelse {
            std.debug.print("TVAR WHICH WAS NOT FOUND: {s}{}\n", .{ tv.name, tv.uid });
            unreachable;
        }),
        else => |t| t,
    };
}

fn getVar(self: *Self, vr: ast.Var) Value {
    const vl = self.scope.getVar(vr);
    return vl.lvalue(); // when it's a var, it should be owned, when it was a ref, it should be lvalue.
}

fn getTNum(self: *Self, tnum: ast.TNum) Value {
    const v = self.scope.getVar(tnum.asVar());
    std.debug.assert(v.smol());
    return v.*;
}

fn putVar(self: *Self, v: ast.Var, value: Value) !void {
    const valspace = try self.scope.vars.getOrPut(v);
    if (valspace.found_existing) {
        value.copyToRef(valspace.value_ptr.*.getRefToMemory());
    } else {
        const nuval = try value.copyWithValue(self.scope.allocator());
        std.debug.assert(!nuval.isLValue());
        valspace.value_ptr.* = nuval;
    }
}

fn putTNum(self: *Self, tnum: ast.TNum, i: usize) !void {
    try self.putVar(tnum.asVar(), Value.int(i));
}

fn putRef(self: *Self, v: ast.Var, value: Value) !void {
    std.debug.assert(value.isLValue());
    const valspace = try self.scope.vars.getOrPut(v);
    if (valspace.found_existing) {
        valspace.value_ptr.*.* = value;
    } else {
        const alval = try common.allocOne(self.scope.allocator(), value);
        try self.scope.vars.put(v, alval);
    }
}

const Scope = struct {
    prev: ?*@This(),
    vars: Vars,
    funs: Funs,
    scopeLocalAllocator: if (!ArenaAlloc) std.heap.ArenaAllocator else std.mem.Allocator,

    const Vars = std.HashMap(ast.Var, *Value, ast.Var.comparator(), std.hash_map.default_max_load_percentage);

    const Funs = std.HashMap(ast.Var, EnvSnapshot, ast.Var.comparator(), std.hash_map.default_max_load_percentage);

    fn init(prev: ?*@This(), alBase: std.mem.Allocator) @This() {
        return .{
            .prev = prev,
            .vars = Vars.init(alBase),
            .funs = Funs.init(alBase),
            .scopeLocalAllocator = if (!ArenaAlloc) std.heap.ArenaAllocator.init(alBase) else alBase,
        };
    }

    fn deinit(self: *@This()) void {
        self.vars.deinit();
        self.funs.deinit();
        if (!ArenaAlloc) self.scopeLocalAllocator.deinit();
    }

    fn getVar(self: *const @This(), v: ast.Var) *Value {
        return self.vars.get(v) orelse (self.prev orelse unreachable).getVar(v);
    }

    fn getFun(self: *const @This(), v: ast.Var) EnvSnapshot {
        return self.funs.get(v) orelse (self.prev orelse unreachable).getFun(v);
    }

    fn allocator(self: *@This()) std.mem.Allocator {
        return if (!ArenaAlloc) self.scopeLocalAllocator.allocator() else self.scopeLocalAllocator;
    }
};

fn registerSignal(self: *Self, sig: i32, fun: Value, funty: ast.Type) !void {
    try self.signalHandlers.put(sig, .{
        .funty = funty,
        .funval = fun,
    });
    try posix.sigaction(@intCast(sig), &.{
        .handler = .{ .handler = sighandler },
        .mask = posix.empty_sigset,
        .flags = 0,
    }, null);
}

fn sighandler(sig: c_int) callconv(.C) void {
    const self = sigself.?;
    const fun = self.signalHandlers.get(sig).?;
    const argty = self.getType(fun.funty).Fun.args[0];
    const sigparamref = Value.int(sig);
    const sigval: TypeVal = .{ .t = argty, .v = sigparamref };
    var params = [_]TypeVal{sigval};

    // NOTE: this is bad - we should not be allocating here, because it's on a separate thread and might break.
    // this problem is similar to how can I present threads n shii.
    // set up temporary stack for dis.
    var defaultStmtExprStack: ExprStack = undefined;
    self.beginStack(&defaultStmtExprStack);
    defer self.restoreStack(&defaultStmtExprStack);

    _ = self.call(fun.funval, &params, fun.funty) catch unreachable;
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
fn isTrue(v: Value) bool {
    return v.get().tag > 0;
}

// kek
const Err = RealErr || Runtime;
const RealErr = error{
    ExpectDyLibAnnotation,
    FailedToFindExternalFunction,

    CaseNotMatched,

    OutOfMemory,

    Bruh,
} || std.DynLib.Error || ffi.Error || error{OperationNotSupported};
const Runtime = error{
    Return,
    Break, // unused right now, just here to show ya.
};
