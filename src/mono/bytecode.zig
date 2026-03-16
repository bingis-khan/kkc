const ast = @import("../ast.zig");
const mono = @import("../mono.zig");
const common = @import("../common.zig");
const std = @import("std");
const GenError = mono.GenError;
const Self = mono.Mono(@This());
// const TypeMap = @import("../TypeMap.zig").TypeMap;
pub const Mono = Self;

al: std.mem.Allocator,
functions: std.ArrayList(Chunk),
cur: Chunk,
vars: Vars,

const Vars = std.HashMap(ast.Var, u32, ast.Var.comparator(), std.hash_map.default_max_load_percentage);
pub fn init(al: std.mem.Allocator) @This() {
    return .{
        .al = al,
        .cur = Chunk.init(al),
        .vars = Vars.init(al),
        .functions = std.ArrayList(Chunk).init(al),
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

        // .Expr => |expr| {
        //     try self.genExpr(expr);
        //     self.ctx.print("\n");
        // },

        else => {
            unreachable;
        }, //TEMP
    }
}

fn storeVar(self: *Self, e: *ast.Expr, localId: usize, sz: Self.Sizes) !void {
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
            try c.appendOp(.Drop, .{@intCast(sz.size)});
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
            try c.constants.append(.{ .I64 = i });
            const const_idx: u8 = @intCast(c.constants.items.len - 1);
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
                else => unreachable,
            }
        },
        .NamedRecord => |rec| {
            const size = self.sizeOf(expr.t).size;
            if (size > 255) {
                @panic("struct too large (todo)");
            }

            const onStack = StackValue.isOnStack(size);

            if (!onStack) {
                try c.appendOp(.Alloc, .{@intCast(size)});
            }

            var off: usize = 0;
            var maxAlignment: usize = 1;
            for (rec.fields) |field| {
                const sz = self.sizeOf(field.value.t);
                if (!onStack) {
                    // second arg - addr
                    try c.appendOp(.Dupe, .{});
                    try c.offset(@intCast(off));

                    // first arg - expr
                    const ownership = try genExpr(self, field.value);
                    if (sz.size > 255) {
                        @panic("too large (todo)");
                    }

                    switch (ownership) {
                        .Stack => try c.appendOp(.CopyFromStack, .{@intCast(sz.size)}),
                        .Unowned => try c.appendOp(.Copy, .{@intCast(sz.size)}),
                        .Owned => {
                            try c.appendOp(.Dupe, .{});
                            try c.appendOp(.SwapN, .{1}); // alloc maddr maddr  alloc
                            try c.appendOp(.Swap, .{}); // alloc maddr alloc maddr
                            try c.appendOp(.Copy, .{@intCast(sz.size)}); // alloc maddr
                            try c.appendOp(.Drop, .{@intCast(sz.size)}); // alloc
                        },
                    }
                } else {
                    unreachable;
                }

                const padding = mono.calculatePadding(off, sz.alignment);
                off += padding + sz.size;
                maxAlignment = @max(maxAlignment, sz.alignment);
            }

            return .Owned; // alloc
        },
        .UnOp => |uop| {
            const ownership = try genExpr(self, uop.e);
            switch (uop.op) {
                .Access => |fieldName| {
                    // FUNNY
                    // allocate a local, copy to it and free that previous memory
                    if (ownership == .Stack) {
                        unreachable; // TODO
                    }

                    const off = self.getFieldOffsetFromType(uop.e.t, fieldName);
                    const sz = self.sizeOf(expr.t).size;
                    const ogsz = self.sizeOf(uop.e.t).size;

                    if (ownership == .Owned) {
                        if (StackValue.isOnStack(sz)) {
                            try c.appendOp(.Dupe, .{});
                            try c.offset(@intCast(off));
                            try c.appendOp(.CopyToStack, .{@intCast(sz)});
                            try c.appendOp(.Swap, .{}); // <val> addr
                            try c.appendOp(.Drop, .{@intCast(ogsz)}); // <val>
                            return .Stack;
                        } else { // addr
                            try c.appendOp(.Dupe, .{});
                            try c.offset(@intCast(off));
                            try c.appendOp(.Alloc, .{@intCast(sz)}); // addr off alloc
                            try c.appendOp(.Dupe, .{}); // addr off alloc alloc
                            try c.appendOp(.SwapN, .{1}); // addr alloc alloc off
                            try c.appendOp(.Copy, .{@intCast(sz)}); // addr alloc
                            try c.appendOp(.Swap, .{});
                            try c.appendOp(.Drop, .{@intCast(ogsz)}); // alloc
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
            std.debug.assert(con.tys.len == 0); // TEMP

            try c.constants.append(.{ .I64 = con.tagValue });
            const const_idx: u8 = @intCast(c.constants.items.len - 1);
            try c.appendOp(.Lit, .{const_idx});
            return .Stack;
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

    const Label = struct { op: u32, stmt: *ast.Stmt };
    fn init(al: std.mem.Allocator) @This() {
        return .{
            .code = std.ArrayList(u8).init(al),
            .constants = std.ArrayList(StackValue.Mem).init(al),
            .numOps = 0,
            .locals = std.ArrayList(u32).init(al),
            .stmtLabels = std.ArrayList(Label).init(al),
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

    pub fn exec(self: *const @This(), al: std.mem.Allocator) !i64 {
        const locals = try al.alloc(Local, self.locals.items.len);
        defer {
            for (locals) |loc| {
                loc.deinit(al);
            }
            al.free(locals);
        }

        for (self.locals.items, locals) |sz, *l| {
            l.* = .{ .ptr = null, .size = sz };
        }

        var stack = std.ArrayList(StackValue).init(al);
        defer stack.deinit();

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
                    const ret = stack.pop().Mem.I64;
                    std.debug.assert(stack.items.len == 0);
                    return ret;
                },
                .Alloc => {
                    const bs = Op.Alloc.bytes(args, &i);
                    const sz = bs[0];
                    const ptr: AlignedRef = try al.alignedAlloc(u8, @alignOf(StackValue.Mem), sz);
                    try stack.append(.{ .Struct = ptr.ptr });
                },
                .Drop => {
                    const sz = Op.Drop.bytes(args, &i)[0];
                    var p: AlignedRef = undefined;
                    p.ptr = @alignCast(stack.pop().Struct);
                    p.len = sz;
                    al.free(p);
                },
                .Dupe => {
                    try stack.append(stack.getLast());
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
                    @memcpy(common.byteSlice(addr, sz), &val.Mem);
                },
                .CopyToStack => {
                    const sz = Op.CopyToStack.bytes(args, &i)[0];
                    const addr = stack.pop().Struct;
                    var v = StackValue.init(undefined);
                    @memcpy(v.Mem.Mem[0..sz], addr);
                    try stack.append(v);
                },

                .Jmp => {
                    const off = Op.JmpIfFalse.bytes(args, &i)[0];
                    i += off;
                },
                .JmpIfFalse => {
                    const off = Op.JmpIfFalse.bytes(args, &i)[0];
                    const cond = stack.pop().Mem.Tag;
                    if (cond == 0) {
                        i += off;
                    }
                },

                .AddI64 => {
                    try stack.append(StackValue.init(.{ .I64 = stack.pop().Mem.I64 + stack.pop().Mem.I64 }));
                },
            }

            i += 1;
        }

        // assume return 0. should we generate code for this? (we should do it at the end of parsing tho OR initialModule)
        std.debug.assert(stack.items.len == 0);
        return 0;
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
        Tag: u32,
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

    LoadVar,
    StoreVar,
    RefVar,

    Alloc,
    Drop,

    Dupe,
    Swap,
    SwapN,
    Offset,
    Copy,
    CopyFromStack,
    CopyToStack,

    Jmp,
    JmpIfFalse,

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
            .LoadVar => "load-var",
            .StoreVar => "store-var",
            .RefVar => "ref-var",
            .Alloc => "alloc",
            .Drop => "drop",
            .Dupe => "dupe",
            .Swap => "swap",
            .SwapN => "swap-n",
            .Offset => "offset",
            .Copy => "copy",
            .CopyFromStack => "copy-from-stack",
            .CopyToStack => "copy-to-stack",
            .Jmp => "jmp",
            .JmpIfFalse => "jmp-if-false",
            .AddI64 => "add-i64",
        };
    }

    fn argNum(op: @This()) u32 {
        return switch (op) {
            .Ret => 0,
            .Lit => 1,
            .LoadVar => 1,
            .StoreVar => 1,
            .RefVar => 1,
            .Alloc => 1,
            .Drop => 1,
            .Dupe => 0,
            .Swap => 0,
            .SwapN => 1,
            .Offset => 1,
            .Copy => 1,
            .CopyFromStack => 1,
            .CopyToStack => 1,
            .Jmp => 1,
            .JmpIfFalse => 1,
            .AddI64 => 0,
        };
    }
};
