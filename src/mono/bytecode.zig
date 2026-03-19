const ast = @import("../ast.zig");
const mono = @import("../mono.zig");
const common = @import("../common.zig");
const std = @import("std");
const GenError = mono.GenError;
const Self = mono.Mono(@This());
const TypeContext = @import("../TypeContext.zig");
const sizer = @import("../sizer.zig");
const Struct = sizer.Size;
const TypeSize = sizer.TypeSize;
pub const Mono = Self;

al: std.mem.Allocator,
functions: std.ArrayList(Chunk),
cur: Chunk,
vars: Vars,
constructors: Constructors,

const Vars = std.HashMap(ast.Var, u32, ast.Var.comparator(), std.hash_map.default_max_load_percentage);
const Constructors = std.HashMap(UniqueCon, *const Chunk, UniqueCon.Comparator, std.hash_map.default_max_load_percentage);
const UniqueCon = struct {
    con: *ast.Con,
    t: ast.Type,

    const Comparator = struct {
        typeContext: *const TypeContext,

        pub fn eql(ctx: @This(), a: UniqueCon, b: UniqueCon) bool {
            return a.con.tagValue == b.con.tagValue and a.t.tyEq(b.t, ctx.typeContext);
        }

        pub fn hash(ctx: @This(), k: UniqueCon) u64 {
            _ = ctx;
            _ = k;
            // TODO: pointless doing it by len, because the Match Sets will be instantiations of the same scheme. do real hash.
            // ALSO OBV TEMP, because we want to test equality.
            return 0;
        }
    };
};
pub fn init(al: std.mem.Allocator, tyc: *const TypeContext) @This() {
    return .{
        .al = al,
        .cur = Chunk.init(al),
        .vars = Vars.init(al),
        .functions = std.ArrayList(Chunk).init(al),
        .constructors = Constructors.initContext(al, .{ .typeContext = tyc }),
    };
}

pub fn genFunction(self: *Self, fun: *ast.Function, m: *const ast.Match) GenError!void { // TODO: params
    _ = fun; // autofix
    std.debug.print("FUNCTION\n", .{});
    const oldM = self.match;
    defer self.match = oldM;
    self.match = m;

    const al = self.backend.al;

    const b = self.backend;
    const outerChunk = b.cur;
    defer self.backend.cur = outerChunk;
    b.cur = Chunk.init(al);

    // TODO: define function
    unreachable;

    // { // this is the same copypasta for every function, need to rethink the api...
    //     self.ctx.indent += 1;
    //     defer self.ctx.indent -= 1;

    //     const oldUseScope = self.useScope;
    //     defer self.useScope = oldUseScope;
    //     var curUseScope = self.newUseScope();
    //     self.useScope = &curUseScope;

    //     try self.monoScope(fun.body);
    // }

    // // TODO: define env
    // unreachable;
}

// this is the "public api" part
pub fn genStmt(self: *Self, stmt: *ast.Stmt) GenError!void {
    const b = self.backend;
    const c = &b.cur;
    try c.stmtLabels.append(.{ .op = @intCast(c.code.items.len), .stmt = stmt });
    switch (stmt.*) {
        // gets eliminated beforehand.
        .Function => unreachable,
        .Instance => unreachable,

        .VarDec => |vd| {
            const sz = self.sizeOf(vd.varValue.t);
            try c.locals.append(@intCast(sz.size));
            const idx: u8 = @intCast(c.locals.items.len - 1);
            try b.vars.put(vd.varDef, idx);

            try storeVar(self, vd.varValue, idx, sz);
        },
        .VarMut => |mut| {
            std.debug.assert(mut.accessors.len == 0); // TEMP

            const idx = b.vars.get(mut.varRef).?;
            const sz = self.sizeOf(mut.varValue.t);
            try storeVar(self, mut.varValue, idx, sz);
        },

        .Return => |expr| {
            _ = try genExpr(self, expr);

            try c.appendOp(.Ret, .{});
        },
        .If => |ifelse| {
            std.debug.assert((try genExpr(self, ifelse.cond)) == .Stack);
            var skipCase = try c.jumpIfFalse();
            try self.monoScope(ifelse.bTrue);

            var endJumpPoints = std.ArrayList(Chunk.JumpPoint).init(b.al);
            for (ifelse.bOthers) |elif| {
                try endJumpPoints.append(try c.jump()); // end of true/all the elifs
                c.finishJumpHere(skipCase);

                std.debug.assert((try genExpr(self, elif.cond)) == .Stack);
                skipCase = try c.jumpIfFalse();
                try self.monoScope(elif.body);
            }

            if (ifelse.bElse) |stmts| {
                try endJumpPoints.append(try c.jump()); // end of true/all the elifs
                c.finishJumpHere(skipCase);

                try self.monoScope(stmts);
            } else {
                c.finishJumpHere(skipCase);
            }

            for (endJumpPoints.items) |jp| {
                c.finishJumpHere(jp);
            }
        },
        .Pass => {
            _ = c.stmtLabels.pop();
        },

        .Expr => |expr| {
            const ownership = try genExpr(self, expr);
            if (ownership == .Owned) {
                try c.appendOp(.Free, .{@intCast(self.sizeOf(expr.t).size)});
            } else {
                try c.appendOp(.Pop, .{});
            }
        },

        .Switch => |sw| {
            const caseExprOwn = try genExpr(self, sw.switchOn);
            const sz = self.sizeOf(sw.switchOn.t);

            var outjmps = std.ArrayList(Chunk.JumpPoint).init(b.al);
            defer outjmps.deinit();

            var skipJump: ?Chunk.JumpPoint = null;
            for (sw.cases, 0..) |case, i| {
                if (skipJump) |sj| {
                    c.finishJumpHere(sj);
                }

                try c.appendOp(.Dupe, .{}); // c c
                try deconstruction(self, case.decon); // c Bool
                skipJump = try c.jumpIfFalse(); // c

                // putting it here, otherwise there will be elements left on the stack when returning.
                if (caseExprOwn == .Owned) {
                    try c.appendOp(.Free, .{@intCast(sz.size)});
                } else {
                    try c.appendOp(.Pop, .{});
                }

                try self.monoScope(case.body);

                if (i != sw.cases.len - 1) {
                    try outjmps.append(try c.jump());
                }
            }

            // if (skipJumps.items.len > 0) {
            //     for (skipJumps.items) |sj| {
            //         c.finishJumpHere(sj);
            //     }
            //     if (caseExprOwn == .Owned) {
            //         try c.appendOp(.Free, .{@intCast(sz.size)});
            //     } else {
            //         try c.appendOp(.Pop, .{});
            //     }
            // }

            for (outjmps.items) |oj| {
                c.finishJumpHere(oj);
            }
        },

        else => {
            unreachable;
        }, //TEMP
    }
}

// c -> Bool
fn deconstruction(self: *Self, d: *const ast.Decon) !void {
    const b = self.backend;
    const c = &b.cur;
    switch (d.d) {
        .None => {
            try c.appendOp(.Pop, .{});
            try c.boolValue(true);
            return;
        },
        .Num => |num| {
            const num_idx = try c.allocLiteral(.{ .I64 = num });
            try c.appendOp(.Lit, .{num_idx}); // c l
            try c.appendOp(.EqI64, .{}); // b
        },
        .Con => |con| {
            const sz = self.sizeOf(d.t);
            const onStack = StackValue.isOnStack(sz.size);
            var size = sizer.Size{};
            var jps = std.ArrayList(Chunk.JumpPoint).init(b.al);
            if (con.con.data.structureType() != .RecordLike) {
                try c.appendOp(.Dupe, .{}); // c c

                const num_idx = try c.allocLiteral(.{ .I64 = con.con.tagValue });
                if (onStack) {
                    try c.appendOp(.Lit, .{num_idx}); // c c t
                } else {
                    try c.appendOp(.CopyToStack, .{sizer.Size.tag.size}); // c s
                    try c.appendOp(.Lit, .{num_idx}); // c s t
                }

                // c c/s t
                try c.appendOp(.EqI64, .{}); // c b
                try jps.append(try c.jumpIfFalse());

                _ = size.add(sizer.Size.tag);
            }

            for (con.decons) |cd| {
                const dsz = self.sizeOf(cd.t);
                const off = size.add(dsz);
                try c.appendOp(.Dupe, .{}); // c c
                const deconOnStack = StackValue.isOnStack(dsz.size);
                if (onStack) {
                    if (deconOnStack) {
                        try c.appendOp(.Zero, .{}); // c c z
                        try c.appendOp(.Swap, .{}); // c z c
                        try c.appendOp(.CopyStackToStack, .{ @intCast(off), 0, @intCast(dsz.size) }); // c z
                    } else {
                        unreachable; // TODO: unless we're talking about pointers.
                    }
                } else {
                    if (deconOnStack) {
                        try c.offset(@intCast(off)); // c o
                        try c.appendOp(.CopyToStack, .{@intCast(dsz.size)}); // c so
                    } else {
                        try c.offset(@intCast(off)); // c o
                    }
                }

                // c so/o/z
                try deconstruction(self, cd); // c b

                try jps.append(try c.jumpIfFalse());
            }

            try c.boolValue(true);
            const jmpover = try c.jump();
            for (jps.items) |jp| {
                c.finishJumpHere(jp);
            }
            try c.boolValue(false);
            c.finishJumpHere(jmpover);

            try c.appendOp(.Swap, .{});
            try c.appendOp(.Pop, .{});
        },
        else => unreachable,
    }
}

fn storeVar(self: *Self, e: *ast.Expr, localId: usize, sz: Struct) !void {
    const b = self.backend;
    const c = &b.cur;
    const idx: u8 = @intCast(localId);

    if (StackValue.isOnStack(sz.size)) {
        std.debug.assert((try genExpr(self, e)) == .Stack);
        try c.appendOp(.StoreVar, .{idx});
    } else {
        try c.appendOp(.RefVar, .{idx});
        const ownership = try genExpr(self, e);
        if (ownership == .Unowned) {
            try c.appendOp(.Copy, .{@intCast(sz.size)});
        } else if (ownership == .Owned) {
            try c.appendOp(.Dupe, .{});
            try c.appendOp(.SwapN, .{1});
            try c.appendOp(.Swap, .{});
            try c.appendOp(.Copy, .{@intCast(sz.size)});
            try c.appendOp(.Free, .{@intCast(sz.size)});
        } else unreachable;
    }
}

const Ownership = enum {
    Owned,
    Unowned,
    Stack,
};
const Res = struct {
    ownership: Ownership,
    lvalueness: i32,

    fn isLValue(self: *const @This()) bool {
        return self.lvalueness <= 0;
    }
};
fn genExpr(self: *Self, expr: *ast.Expr) !Ownership {
    const b = self.backend;
    const c = &b.cur;
    switch (expr.e) {
        .Int => |i| {
            const const_idx = try c.allocLiteral(.{ .I64 = i });
            try c.appendOp(.Lit, .{const_idx});
            return .Stack;
        },
        .Intrinsic => |intr| {
            for (intr.args) |arg| {
                std.debug.assert((try genExpr(self, arg)) == .Stack);
            }

            switch (intr.intr.ty) {
                .@"i64-add" => {
                    try c.appendOp(.AddI64, .{});
                    return .Stack;
                },
                .undefined => {
                    const sz = self.sizeOf(expr.t).size;
                    if (StackValue.isOnStack(sz)) {
                        try c.appendOp(.Zero, .{});
                        return .Stack;
                    } else {
                        try c.appendOp(.Alloc, .{@intCast(sz)});
                        return .Owned;
                    }
                },
                else => unreachable,
            }
        },
        .NamedRecord => |rec| {
            const recordSize = self.sizeOf(expr.t).size;
            if (recordSize > 255) {
                @panic("struct too large (todo)");
            }

            const onStack = StackValue.isOnStack(recordSize);
            if (onStack) {
                try c.appendOp(.Zero, .{});
            } else {
                try c.appendOp(.Alloc, .{@intCast(recordSize)});
            }

            var size = sizer.Size{};
            for (rec.fields) |field| {
                const sz = self.sizeOf(field.value.t);
                const off = size.add(sz);

                // second arg - addr
                if (!onStack) {
                    try c.appendOp(.Dupe, .{});
                    try c.offset(@intCast(off));
                }

                // first arg - expr
                const ownership = try genExpr(self, field.value);
                if (sz.size > 255) {
                    @panic("too large (todo)");
                }

                if (onStack) {
                    switch (ownership) {
                        .Stack => try c.appendOp(.CopyStackToStack, .{ 0, @intCast(off), @intCast(sz.size) }),
                        .Unowned => unreachable,
                        .Owned => unreachable,
                    }
                } else {
                    switch (ownership) {
                        .Stack => try c.appendOp(.CopyFromStack, .{@intCast(sz.size)}),
                        .Unowned => try c.appendOp(.Copy, .{@intCast(sz.size)}),
                        .Owned => {
                            try c.appendOp(.Dupe, .{});
                            try c.appendOp(.SwapN, .{1}); // alloc maddr maddr  alloc
                            try c.appendOp(.Swap, .{}); // alloc maddr alloc maddr
                            try c.appendOp(.Copy, .{@intCast(sz.size)}); // alloc maddr
                            try c.appendOp(.Free, .{@intCast(sz.size)}); // alloc
                        },
                    }
                }
            }

            return if (onStack) .Stack else .Owned;
        },
        .UnOp => |uop| {
            const ownership = try genExpr(self, uop.e);
            switch (uop.op) {
                .Access => |fieldName| {
                    // FUNNY
                    // allocate a local, copy to it and free that previous memory

                    const off = self.getFieldOffsetFromType(uop.e.t, fieldName);
                    const sz = self.sizeOf(expr.t).size;
                    const ogsz = self.sizeOf(uop.e.t).size;
                    std.debug.assert(sz <= ogsz);
                    const onStack = StackValue.isOnStack(ogsz);

                    if (onStack) {
                        try c.appendOp(.Zero, .{});
                        try c.appendOp(.CopyStackToStack, .{ @intCast(off), 0, @intCast(sz) });
                        return .Stack;
                    } else if (ownership == .Owned) {
                        if (StackValue.isOnStack(sz)) {
                            try c.appendOp(.Dupe, .{});
                            try c.offset(@intCast(off));
                            try c.appendOp(.CopyToStack, .{@intCast(sz)});
                            try c.appendOp(.Swap, .{}); // <val> addr
                            try c.appendOp(.Free, .{@intCast(ogsz)}); // <val>
                            return .Stack;
                        } else { // addr
                            try c.appendOp(.Dupe, .{});
                            try c.offset(@intCast(off));
                            try c.appendOp(.Alloc, .{@intCast(sz)}); // addr off alloc
                            try c.appendOp(.Dupe, .{}); // addr off alloc alloc
                            try c.appendOp(.SwapN, .{1}); // addr alloc alloc off
                            try c.appendOp(.Copy, .{@intCast(sz)}); // addr alloc
                            try c.appendOp(.Swap, .{});
                            try c.appendOp(.Free, .{@intCast(ogsz)}); // alloc
                            return .Owned;
                        }
                    } else if (ownership == .Unowned) {
                        try c.offset(@intCast(off));
                        if (StackValue.isOnStack(sz)) {
                            try c.appendOp(.CopyToStack, .{@intCast(sz)});
                            return .Stack;
                        } else {
                            return .Unowned;
                        }
                    } else {
                        unreachable;
                    }

                    unreachable;
                },
                .As => return ownership,
                else => unreachable,
            }
        },
        .Var => |v| {
            const idx = b.vars.get(v.v.Var).?;
            const sz = self.sizeOf(expr.t).size;
            if (StackValue.isOnStack(sz)) {
                try c.appendOp(.LoadVar, .{@intCast(idx)});
                return .Stack;
            } else {
                try c.appendOp(.RefVar, .{@intCast(idx)});
                return .Unowned;
            }
        },
        .Con => |con| {
            if (con.tys.len == 0) {
                const const_idx = try c.allocLiteral(.{ .I64 = con.tagValue }); // technically should be .Tag, but displays better.

                const sz = self.sizeOf(expr.t);
                if (StackValue.isOnStack(sz.size)) {
                    try c.appendOp(.Lit, .{const_idx});
                    return .Stack;
                } else {
                    try c.appendOp(.Alloc, .{@intCast(sz.size)});
                    try c.appendOp(.Dupe, .{});
                    try c.appendOp(.Lit, .{const_idx});
                    try c.appendOp(.CopyFromStack, .{sizer.Size.tag.size});
                    return .Owned;
                }
            } else {
                const funty = self.typeContext.getType(expr.t).Fun;
                const ret = funty.ret;
                const pv = try b.constructors.getOrPut(.{ .con = con, .t = ret });
                if (!pv.found_existing) {
                    const cc: *Chunk = try common.allocOne(b.al, Chunk.init(b.al));
                    const sz = self.sizeOf(ret).size;
                    const onStack = StackValue.isOnStack(sz);

                    if (onStack) {
                        try cc.appendOp(.Zero, .{});
                    } else {
                        try cc.appendOp(.Alloc, .{@intCast(sz)});
                    }

                    // allocate tag
                    var size = sizer.Size{};
                    if (con.data.structureType() == .ADT) {
                        const const_idx = try cc.allocLiteral(.{ .I64 = con.tagValue });

                        if (!onStack) {
                            try cc.appendOp(.Dupe, .{});
                        }
                        try cc.appendOp(.Lit, .{const_idx});

                        if (!onStack) {
                            try cc.appendOp(.CopyFromStack, .{@intCast(sizer.Size.tag.size)});
                        } else {
                            try cc.appendOp(.CopyStackToStack, .{ 0, 0, @intCast(sizer.Size.tag.size) });
                        }

                        _ = size.add(sizer.Size.tag);
                    }

                    // allocate args.
                    // TODO copypasta
                    for (funty.args, 0..) |argt, i| {
                        const argsz = self.sizeOf(argt);
                        const off = size.add(argsz);

                        if (onStack) {
                            try cc.dupeN(@intCast(i)); // args... acc
                            if (StackValue.isOnStack(argsz.size)) {
                                try cc.appendOp(.CopyStackToStack, .{ 0, @intCast(off), @intCast(argsz.size) });
                            } else {
                                unreachable;
                            }
                        } else {
                            // args... alloc
                            try cc.appendOp(.Dupe, .{});
                            try cc.offset(@intCast(off)); // args... alloc off
                            try cc.dupeN(@intCast(i + 1));

                            if (StackValue.isOnStack(argsz.size)) {
                                try cc.appendOp(.CopyFromStack, .{@intCast(argsz.size)});
                            } else {
                                try cc.appendOp(.Copy, .{@intCast(argsz.size)});
                            }
                        }
                    }

                    const finalSize = size.finish();
                    std.debug.assert(finalSize.size == sz);

                    try cc.appendOp(.Ret, .{});

                    // assume arguments on stack, unowned.
                    pv.value_ptr.* = cc;
                }

                // TODO: tag pointer n shiii.
                try c.constants.append(.{ .Chunk = pv.value_ptr.* });
                const const_idx: u8 = @intCast(c.constants.items.len - 1);
                try c.appendOp(.Lit, .{const_idx});
                return .Stack;
            }
        },
        .Call => |call| {
            // TODO: evaluate function first
            var owns = std.ArrayList(?usize).init(b.al);
            defer owns.deinit();

            // set up callstack
            for (call.args) |arg| {
                const argown = try genExpr(self, arg);
                if (argown == .Owned) {
                    // try c.appendOp(.Dupe, .{});
                    // try c.appendOp(.Rot, .{@intCast(i)});
                    try owns.append(self.sizeOf(arg.t).size);
                } else {
                    try owns.append(null);
                }
            }

            const funown = try genExpr(self, call.callee);
            if (funown == .Owned) {
                try c.appendOp(.Dupe, .{});
                try c.appendOp(.Rot, .{@intCast(call.args.len)});
            }

            // call
            try c.appendOp(.Call, .{});
            if (owns.items.len > 0) {
                try c.appendOp(.Rot, .{@intCast(owns.items.len - 1)});
            }

            var i = owns.items.len;
            while (i > 0) {
                i -= 1;
                const msz = owns.items[i];
                if (msz) |sz| {
                    try c.appendOp(.Free, .{@intCast(sz)});
                } else {
                    try c.appendOp(.Pop, .{});
                }
            }

            // assume the result is always owned IF IT'S NOT ON THE STACK.
            const retsz = self.sizeOf(expr.t);
            if (StackValue.isOnStack(retsz.size)) {
                return .Stack;
            } else {
                return .Owned;
            }
        },
        else => unreachable,
    }
}

pub fn genEnvCompletion(self: *Self, incompleteEnv: ast.Function.FunApp, completedEnv: ast.Function.FunApp) !void {
    self.ctx.print(.{ "||", incompleteEnv.fun.name, " ", incompleteEnv.m, "\n^^", completedEnv.fun.name, " ", completedEnv.m, "\n" });
    unreachable;
}

//////////////

//////////////

const Chunk = struct {
    code: std.ArrayList(u8),
    stmtLabels: std.ArrayList(Label),
    constants: std.ArrayList(StackValue.Mem),
    numOps: u32,

    locals: std.ArrayList(u32),

    // trvthsave
    truth: ?u8,
    falsiness: ?u8,

    const Label = struct { op: u32, stmt: *ast.Stmt };
    fn init(al: std.mem.Allocator) @This() {
        return .{
            .code = std.ArrayList(u8).init(al),
            .constants = std.ArrayList(StackValue.Mem).init(al),
            .numOps = 0,
            .locals = std.ArrayList(u32).init(al),
            .stmtLabels = std.ArrayList(Label).init(al),

            .truth = null,
            .falsiness = null,
        };
    }

    fn appendOp(self: *@This(), comptime op: Op, args: [op.argNum()]u8) !void {
        try self.code.append(@intFromEnum(op));
        try self.code.appendSlice(args[0..]);
        self.numOps += 1;
    }

    fn offset(self: *@This(), off: u8) !void {
        if (off > 0) {
            try self.appendOp(.Offset, .{off});
        }
    }

    // dupes an element on nth position on the stack.
    fn dupeN(self: *@This(), n: u8) !void {
        if (n == 0) {
            try self.appendOp(.Dupe, .{});
        } else {
            try self.appendOp(.SwapN, .{n});
            try self.appendOp(.Dupe, .{});
            try self.appendOp(.SwapN, .{n + 1});
            try self.appendOp(.Swap, .{});
        }
    }

    fn allocLiteral(self: *@This(), lit: StackValue.Mem) !u8 {
        try self.constants.append(lit);
        const const_idx: u8 = @intCast(self.constants.items.len - 1);
        return const_idx;
    }

    fn boolValue(self: *@This(), b: bool) !void {
        const idx = bb: {
            if (b) {
                if (self.truth) |tidx| {
                    break :bb tidx;
                } else {
                    const tid = try self.allocLiteral(.{ .I64 = @intFromBool(b) });
                    self.truth = tid;
                    break :bb tid;
                }
            } else {
                if (self.falsiness) |fidx| {
                    break :bb fidx;
                } else {
                    const fid = try self.allocLiteral(.{ .I64 = @intFromBool(b) });
                    self.falsiness = fid;
                    break :bb fid;
                }
            }
        };
        try self.appendOp(.Lit, .{idx});
    }

    const JumpPoint = usize;
    fn jumpIfFalse(self: *@This()) !JumpPoint {
        try self.appendOp(.JmpIfFalse, .{0});
        return self.code.items.len;
    }

    fn jump(self: *@This()) !JumpPoint {
        try self.appendOp(.Jmp, .{0});
        return self.code.items.len;
    }

    fn finishJumpHere(self: *const @This(), jp: JumpPoint) void {
        const cur = self.code.items.len;
        const diff: u8 = @intCast(cur - jp);
        self.code.items[jp - 1] = diff;

        // NOTE: we must make sure the are instructions left.
    }

    pub fn exec(self: *const @This(), stack: *Stack, al: std.mem.Allocator) !void {
        const locals = try al.alloc(Local, self.locals.items.len);
        for (self.locals.items, locals) |sz, *l| {
            l.* = .{ .ptr = null, .size = sz };
        }
        defer {
            for (locals) |loc| {
                loc.deinit(al);
            }
            al.free(locals);
        }

        var i: u32 = 0;
        while (i < self.code.items.len) {
            const op: Op = @enumFromInt(self.code.items[i]);
            const args = self.code.items;

            switch (op) {
                .Lit => {
                    const idx = args[i + 1];
                    i += 1;
                    const v = self.constants.items[idx];
                    try stack.append(.{ .Mem = v });
                },
                .Zero => {
                    try stack.append(.{ .Mem = .{ .I64 = 0 } });
                },
                .LoadVar => {
                    const bs = Op.LoadVar.bytes(args, &i);
                    const idx = bs[0];
                    try stack.append(.{ .Mem = locals[idx].forceValue() });
                },
                .StoreVar => {
                    const idx = args[i + 1];
                    i += 1;
                    const lp = try locals[idx].getValuePtr(al);
                    lp.* = stack.pop().Mem;
                },
                .RefVar => {
                    const lidx = Op.RefVar.bytes(args, &i)[0];
                    const lp = try locals[lidx].getPtr(al);
                    try stack.append(.{ .Struct = lp.ptr });
                },
                .Ret => {
                    std.debug.assert(stack.items.len > 0);
                    return;
                },
                .Alloc => {
                    const bs = Op.Alloc.bytes(args, &i);
                    const sz = bs[0];
                    const ptr: AlignedRef = try al.alignedAlloc(u8, @alignOf(StackValue.Mem), sz);
                    try stack.append(.{ .Struct = ptr.ptr });
                },
                .Free => {
                    const sz = Op.Free.bytes(args, &i)[0];
                    var p: AlignedRef = undefined;
                    p.ptr = @alignCast(stack.pop().Struct);
                    p.len = sz;
                    al.free(p);
                },
                .Dupe => {
                    try stack.append(stack.getLast());
                },
                .Pop => {
                    _ = stack.pop();
                },
                .Swap => {
                    const lower = &stack.items[stack.items.len - 2];
                    const lowerVal = lower.*;
                    lower.* = stack.getLast();
                    stack.items[stack.items.len - 1] = lowerVal;
                },
                .SwapN => {
                    const n = Op.SwapN.bytes(args, &i)[0];
                    const top = stack.pop();
                    const inner = &stack.items[stack.items.len - 1 - n];
                    const other = inner.*;
                    inner.* = top;
                    try stack.append(other);
                },
                .Rot => {
                    const n = Op.Rot.bytes(args, &i)[0];
                    const top = stack.pop();
                    const rotI = stack.items.len - 1 - n;
                    try stack.insert(rotI, top);
                },
                .Offset => {
                    const off = Op.Offset.bytes(args, &i)[0];
                    try stack.append(.{ .Struct = stack.pop().Struct[off..] });
                },
                .Copy => {
                    const sz = Op.Copy.bytes(args, &i)[0];
                    const val = stack.pop().Struct;
                    const addr = stack.pop().Struct;
                    @memcpy(common.byteSlice(addr, sz), val);
                },
                .CopyFromStack => {
                    const sz = Op.CopyFromStack.bytes(args, &i)[0];
                    const val = stack.pop().Mem;
                    const addr = stack.pop().Struct;
                    @memcpy(common.byteSlice(addr, sz), (&val.Mem)[0..sz]);
                },
                .CopyToStack => {
                    const sz = Op.CopyToStack.bytes(args, &i)[0];
                    const addr = stack.pop().Struct;
                    var v = StackValue.init(.{ .I64 = 0 });
                    @memcpy(v.Mem.Mem[0..sz], addr);
                    try stack.append(v);
                },

                .CopyStackToStack => {
                    const bs = Op.CopyStackToStack.bytes(args, &i);
                    const offsrc = bs[0];
                    const offdest = bs[1];
                    const sz = bs[2];
                    const src = stack.pop();
                    const dest = &stack.items[stack.items.len - 1];
                    @memcpy(dest.Mem.Mem[offdest .. offdest + sz], src.Mem.Mem[offsrc .. offsrc + sz]);
                },

                .Jmp => {
                    const off = Op.Jmp.bytes(args, &i)[0];
                    i += off;
                },
                .JmpIfFalse => {
                    const off = Op.JmpIfFalse.bytes(args, &i)[0];
                    const cond = stack.pop().Mem.I64;
                    if (cond == 0) {
                        i += off;
                    }
                },
                .Call => {
                    const fun = stack.pop().Mem.Chunk;
                    const prevStackSize = stack.items.len;
                    try fun.exec(stack, al);
                    const curStackSize = stack.items.len;
                    std.debug.assert(curStackSize == prevStackSize + 1);
                },

                .EqI64 => {
                    try stack.append(StackValue.init(.{
                        .I64 = @intFromBool(stack.pop().Mem.I64 == stack.pop().Mem.I64),
                    }));
                },

                .AddI64 => {
                    try stack.append(StackValue.init(.{ .I64 = stack.pop().Mem.I64 + stack.pop().Mem.I64 }));
                },
            }

            i += 1;
        }

        // assume return 0. should we generate code for this? (we should do it at the end of parsing tho OR initialModule)
        std.debug.assert(stack.items.len == 0);
    }

    pub fn print(self: *const @This(), cc: ast.Ctx) void {
        var c = cc;
        c.print("num:\n");
        {
            c.indent += 1;
            defer c.indent -= 1;

            c.print(.{ "ops: ", self.numOps, "\n" });
            c.print(.{ "constants: ", self.constants.items.len, "\n" });
            c.print(.{ "locals: ", self.locals.items.len, "\n" });
        }

        c.print("code:\n");
        {
            c.indent += 1;
            defer c.indent -= 1;

            var i: u32 = 0;
            var li: u32 = 0;
            while (i < self.code.items.len) {
                const op: Op = @enumFromInt(self.code.items[i]);
                const args = self.code.items;
                op.print(c, args, &i);

                if (li < self.stmtLabels.items.len and i >= self.stmtLabels.items[li].op) {
                    c.print("   ");
                    self.stmtLabels.items[li].stmt.print(c);
                    li += 1;
                } else {
                    c.print("\n");
                }

                i += 1;
            }
        }

        c.print("constants:\n");
        {
            c.indent += 1;
            defer c.indent -= 1;

            for (self.constants.items, 0..) |constant, i| {
                c.print(.{ i, ": ", constant.I64, "\n" });
            }
        }
    }
};

const AlignedRef = []align(@sizeOf(StackValue.Mem)) u8;
const Local = struct {
    ptr: ?[*]align(@sizeOf(StackValue.Mem)) u8,
    size: u32,

    pub fn getPtr(self: *@This(), al: std.mem.Allocator) ![]align(@sizeOf(StackValue.Mem)) u8 {
        if (self.ptr) |ptr| {
            var s: []align(@sizeOf(StackValue.Mem)) u8 = undefined;
            s.ptr = ptr;
            s.len = @max(self.size, @sizeOf(StackValue.Mem));
            return s;
        } else {
            const s = try al.alignedAlloc(u8, @sizeOf(StackValue.Mem), self.size);
            self.ptr = s.ptr;
            return s;
        }
    }

    pub fn getValuePtr(self: *@This(), al: std.mem.Allocator) !*StackValue.Mem {
        std.debug.assert(self.size <= @sizeOf(StackValue.Mem));
        const p = try self.getPtr(al);
        return @ptrCast(p.ptr);
    }

    pub fn forceValue(self: *const @This()) StackValue.Mem {
        const p = self.ptr.?;
        const memptr: *StackValue.Mem = @ptrCast(p);
        return memptr.*;
    }

    fn deinit(self: *const @This(), al: std.mem.Allocator) void {
        var p: []align(@sizeOf(StackValue.Mem)) u8 = undefined;
        if (self.ptr) |ptr| {
            p.ptr = ptr;
            p.len = self.size;
            al.free(p);
        }
    }
};
// comptime {
//     if (@sizeOf(StackValue) != 8) @compileError(std.fmt.comptimePrint("current size: {}", .{@sizeOf(StackValue)}));
// }

const StackValue = union {
    Struct: [*]u8,
    Mem: Mem,

    const Mem = extern union {
        Ptr: *anyopaque,
        I64: i64,
        // Tag: sizer.Tag,  // we use the same equality as for ints.
        Chunk: *const Chunk,
        Mem: [Size]u8,
    };
    const Size = @max(@sizeOf(*anyopaque), @sizeOf(i64));
    comptime {
        if (@sizeOf(Mem) != Size) @compileError("sheeesh");
    }
    fn isOnStack(s: usize) bool {
        return s <= Size;
    }

    fn init(mem: Mem) @This() {
        return .{ .Mem = mem };
    }
};

const Op = enum(u8) {
    Ret,

    Lit,
    Zero,

    LoadVar,
    StoreVar,
    RefVar,

    Alloc,
    Free,

    Dupe,
    Pop,
    Swap,
    SwapN,
    Rot,
    Offset,
    Copy,
    CopyFromStack,
    CopyToStack,
    CopyStackToStack,

    Jmp,
    JmpIfFalse,
    Call,

    EqI64,

    // intrinsics
    //  (currently, each intrinsic will have a unique instruction - we'll see if it's enough)
    AddI64,

    fn print(op: @This(), c: ast.Ctx, args: []u8, i: *u32) void {
        c.print(.{op.name()});
        for (0..op.argNum()) |ai| {
            c.print(.{ " ", args[i.* + ai + 1] });
        }

        i.* += op.argNum();
    }

    fn bytes(comptime op: @This(), args: []u8, i: *u32) [op.argNum()]u8 {
        var arg: [op.argNum()]u8 = undefined;
        for (0..op.argNum()) |ai| {
            arg[ai] = args[i.* + ai + 1];
        }

        i.* += op.argNum();
        return arg;
    }

    fn name(op: @This()) []const u8 {
        return switch (op) {
            .Ret => "ret",
            .Lit => "lit",
            .Zero => "zero",
            .LoadVar => "load-var",
            .StoreVar => "store-var",
            .RefVar => "ref-var",
            .Alloc => "alloc",
            .Free => "free",
            .Dupe => "dupe",
            .Pop => "pop",
            .Swap => "swap",
            .SwapN => "swap-n",
            .Rot => "rot",
            .Offset => "offset",
            .Copy => "copy",
            .CopyFromStack => "copy-from-stack",
            .CopyToStack => "copy-to-stack",
            .CopyStackToStack => "copy-stack-to-stack",
            .Jmp => "jmp",
            .JmpIfFalse => "jmp-if-false",
            .Call => "call",
            .EqI64 => "eq-i64",
            .AddI64 => "add-i64",
        };
    }

    fn argNum(op: @This()) u32 {
        return switch (op) {
            .Ret => 0,
            .Lit => 1,
            .Zero => 0,
            .LoadVar => 1,
            .StoreVar => 1,
            .RefVar => 1,
            .Alloc => 1,
            .Free => 1,
            .Dupe => 0,
            .Pop => 0,
            .Swap => 0,
            .SwapN => 1,
            .Rot => 1,
            .Offset => 1,
            .Copy => 1,
            .CopyFromStack => 1,
            .CopyToStack => 1,
            .CopyStackToStack => 3, // offsrc offdest sz
            .Jmp => 1,
            .JmpIfFalse => 1,
            .Call => 0,
            .EqI64 => 0,
            .AddI64 => 0,
        };
    }
};

const Stack = std.ArrayList(StackValue);
pub fn exec(c: *const Chunk, al: std.mem.Allocator) !i64 {
    var stack = Stack.init(al);
    defer stack.deinit();
    try c.exec(&stack, al);

    const ret = if (stack.items.len == 0) 0 else stack.pop().Mem.I64;
    std.debug.assert(stack.items.len == 0);

    return ret;
}

pub fn print(self: *const @This(), c: ast.Ctx) void {
    var conIt = self.constructors.iterator();
    while (conIt.next()) |con| {
        c.print(.{ con.key_ptr.con.name, " ", con.key_ptr.t, "\n" });
        {
            var cc = c;
            cc.indent += 1;
            defer cc.indent -= 1;

            con.value_ptr.*.print(cc);
        }
    }
    self.cur.print(c);
}
