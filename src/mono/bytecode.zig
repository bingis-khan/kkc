const ast = @import("../ast.zig");
const mono = @import("../mono.zig");
const common = @import("../common.zig");
const std = @import("std");
const GenError = mono.GenError;
const Self = mono.Mono(@This());
// const TypeMap = @import("../TypeMap.zig").TypeMap;
pub const Mono = Self;

cur: Chunk,
vars: Vars,

const Vars = std.HashMap(ast.Var, u32, ast.Var.comparator(), std.hash_map.default_max_load_percentage);
pub fn init(al: std.mem.Allocator) @This() {
    return .{
        .cur = Chunk.init(al),
        .vars = Vars.init(al),
    };
}

pub fn genFunction(self: *Self, fun: *ast.Function, m: *const ast.Match) GenError!void { // TODO: params
    const oldM = self.ctx.mapTypes;
    self.ctx.mapTypes = m;
    defer self.ctx.mapTypes = oldM;

    // TODO: define function

    { // this is the same copypasta for every function, need to rethink the api...
        self.ctx.indent += 1;
        defer self.ctx.indent -= 1;

        const oldUseScope = self.useScope;
        defer self.useScope = oldUseScope;
        var curUseScope = self.newUseScope();
        self.useScope = &curUseScope;

        try self.monoScope(fun.body);
    }

    // TODO: define env
    unreachable;
}

// this is the "public api" part
pub fn genStmt(self: *Self, stmt: *ast.Stmt) !void {
    const b = self.backend;
    const c = &b.cur;
    switch (stmt.*) {
        // gets eliminated beforehand.
        .Function => unreachable,
        .Instance => unreachable,

        .VarDec => |vd| {
            const sz = self.sizeOf(vd.varValue.t);
            try c.locals.append(@intCast(sz.size));
            const idx: u8 = @intCast(c.locals.items.len - 1);
            try b.vars.put(vd.varDef, idx);

            if (StackValue.isOnStack(sz.size)) {
                try genExpr(self, vd.varValue);
                try c.appendOp(.StoreVar, .{idx});
            } else {
                try c.appendOp(.RefVar, .{idx});
                try genExpr(self, vd.varValue);
                try c.appendOp(.Copy, .{@intCast(sz.size)});
            }
        },

        .Return => |expr| {
            try genExpr(self, expr);

            try c.appendOp(.Ret, .{});
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

fn genExpr(self: *Self, expr: *ast.Expr) !void {
    const b = self.backend;
    const c = &b.cur;
    switch (expr.e) {
        .Int => |i| {
            try c.constants.append(.{ .I64 = i });
            const const_idx: u8 = @intCast(c.constants.items.len - 1);
            try c.appendOp(.Lit, .{const_idx});
        },
        .Intrinsic => |intr| {
            for (intr.args) |arg| {
                try genExpr(self, arg);
            }

            switch (intr.intr.ty) {
                .@"i64-add" => try c.appendOp(.AddI64, .{}),
                else => unreachable,
            }
        },
        .NamedRecord => |rec| {
            const size = self.sizeOf(expr.t).size;
            if (size > 255) {
                @panic("struct too large (todo)");
            }

            try c.appendOp(.Alloc, .{@intCast(size)});

            var off: usize = 0;
            var maxAlignment: usize = 1;
            for (rec.fields) |field| {
                // second arg - addr
                try c.appendOp(.Dupe, .{});
                if (off != 0) {
                    try c.appendOp(.Offset, .{@intCast(off)});
                }

                // first arg - expr
                try genExpr(self, field.value);

                const sz = self.sizeOf(field.value.t);
                if (sz.size > 255) {
                    @panic("too large (todo)");
                }

                // if fits in unio
                if (sz.size <= StackValue.Size) {
                    try c.appendOp(.CopyFromStack, .{@intCast(sz.size)});
                } else {
                    try c.appendOp(.Copy, .{@intCast(sz.size)});
                }

                const padding = mono.calculatePadding(off, sz.alignment);
                off += padding + sz.size;
                maxAlignment = @max(maxAlignment, sz.alignment);
            }
        },
        .UnOp => |uop| {
            try genExpr(self, uop.e);
            switch (uop.op) {
                .Access => |fieldName| {
                    const off = self.getFieldOffsetFromType(uop.e.t, fieldName);
                    try c.appendOp(.Offset, .{@intCast(off)});
                    const sz = self.sizeOf(expr.t).size;
                    if (StackValue.isOnStack(sz)) {
                        try c.appendOp(.CopyToStack, .{@intCast(sz)});
                    }
                },
                else => unreachable,
            }
        },
        .Var => |v| {
            const idx = b.vars.get(v.v.Var).?;
            const sz = self.sizeOf(expr.t).size;
            if (StackValue.isOnStack(sz)) {
                try c.appendOp(.LoadVar, .{@intCast(idx)});
            } else {
                try c.appendOp(.RefVar, .{@intCast(idx)});
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
    constants: std.ArrayList(StackValue.Mem),
    numOps: u32,

    locals: std.ArrayList(u32),

    fn init(al: std.mem.Allocator) @This() {
        return .{
            .code = std.ArrayList(u8).init(al),
            .constants = std.ArrayList(StackValue.Mem).init(al),
            .numOps = 0,
            .locals = std.ArrayList(u32).init(al),
        };
    }

    fn appendOp(self: *@This(), comptime op: Op, args: [op.argNum()]u8) !void {
        try self.code.append(@intFromEnum(op));
        try self.code.appendSlice(args[0..]);
        self.numOps += 1;
    }

    pub fn exec(self: *const @This(), al: std.mem.Allocator) !i64 {
        const locals = try al.alloc(Local, self.locals.items.len);
        for (self.locals.items, locals) |sz, *l| {
            l.* = .{ .ptr = null, .size = sz };
        }

        var stack = std.ArrayList(StackValue).init(al);

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
                    return stack.pop().Mem.I64;
                },
                .Alloc => {
                    const bs = Op.Alloc.bytes(args, &i);
                    const sz = bs[0];
                    const ptr = try al.alignedAlloc(u8, @alignOf(StackValue.Mem), sz);
                    try stack.append(.{ .Struct = ptr.ptr });
                },
                .Dupe => {
                    try stack.append(stack.getLast());
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

                .AddI64 => {
                    try stack.append(StackValue.init(.{ .I64 = stack.pop().Mem.I64 + stack.pop().Mem.I64 }));
                },
            }

            i += 1;
        }

        // assume return 0. should we generate code for this? (we should do it at the end of parsing tho OR initialModule)
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
            while (i < self.code.items.len) {
                const op: Op = @enumFromInt(self.code.items[i]);
                const args = self.code.items;
                op.print(c, args, &i);

                i += 1;
                c.print("\n");
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
    Dupe,
    Offset,
    Copy,
    CopyFromStack,
    CopyToStack,

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
            .Dupe => "dupe",
            .Offset => "offset",
            .Copy => "copy",
            .CopyFromStack => "copy-from-stack",
            .CopyToStack => "copy-to-stack",
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
            .Dupe => 0,
            .Offset => 1,
            .Copy => 1,
            .CopyFromStack => 1,
            .CopyToStack => 1,
            .AddI64 => 0,
        };
    }
};
