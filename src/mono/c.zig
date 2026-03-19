const ast = @import("../ast.zig");
const mono = @import("../mono.zig");
const common = @import("../common.zig");
const std = @import("std");
const GenError = mono.GenError;
const TypeContext = @import("../TypeContext.zig");
const sizer = @import("../sizer.zig");
const Struct = sizer.Size;
const TypeSize = sizer.TypeSize;
const Str = common.Str;
const Set = @import("../Set.zig").Set;
const TypeMap = @import("../TypeMap.zig").TypeMap;
const UniqueGen = @import("../UniqueGen.zig");
const Unique = UniqueGen.Unique;

///
const Self = mono.Mono(@This());
pub const Mono = Self;

// out: Writer,
tymap: *const TypeMap,
imports: Set(Str, std.hash_map.StringContext),
functionsGenerated: FunctionsGenerated,
parts: std.ArrayList(CW),
cur: CW,
al: std.mem.Allocator,
tempgen: UniqueGen,

const FunctionsGenerated = std.HashMap(EnvApp, Unique, EnvApp.Comparator, std.hash_map.default_max_load_percentage);
pub fn init(al: std.mem.Allocator, tc: *const TypeContext) @This() {
    var c = @This(){
        .cur = CW.init(al),
        .al = al,
        .imports = Set(Str, std.hash_map.StringContext).init(al),
        .functionsGenerated = FunctionsGenerated.initContext(al, .{ .typeContext = tc }),
        .parts = std.ArrayList(CW).init(al),
        .tymap = &TypeMap.Empty,
        .tempgen = UniqueGen.init(),
    };

    c.cur.currentIndent += 1;
    return c;
}

pub fn writeTo(self: *const @This(), writer: anytype) anyerror!void {
    var importIt = self.imports.iterator();
    while (importIt.next()) |importName| {
        try writer.print("#include <{s}>\n", .{importName.*});
    }

    for (self.parts.items) |fun| {
        try fun.writeTo(writer);
        try writer.print("\n", .{});
    }

    try writer.print("int main() {{\n", .{});
    try self.cur.writeTo(writer);
    try writer.print("}}\n", .{});
}

pub fn genFunction(self: *Self, fun: *ast.Function, m: *const ast.Match) GenError!void { // TODO: params
    const tymap = TypeMap{
        .match = m,
        .prev = self.backend.tymap,
        .scheme = &fun.scheme,
    };

    const oldTyMap = self.backend.tymap;
    self.backend.tymap = &tymap;
    defer self.backend.tymap = oldTyMap;

    const nuId = self.backend.tempgen.newUnique();

    var env = fun.env.monoInsts;
    const isEnvEmpty = env.empty();
    if (!isEnvEmpty) {
        // generate env
        {
            const oldCW = self.backend.cur;
            self.backend.cur = CW.init(self.backend.al);
            defer self.backend.cur = oldCW;

            var e = startLine(self);
            try e.p(.{"struct"});
            try e.j(.{ "env", nuId });
            try e.beginBody();

            var envIt = env.iterator();
            while (envIt.next()) |inst| {
                var i = startLine(self);
                try i.p(.{inst.*});
                try i.finishStmt();
            }

            try endBodyAndFinishStmt(self);
            try self.backend.parts.append(self.backend.cur);
        }

        // generate env inst
        {
            var ei = startLine(self);
            try ei.p(.{"struct"});
            try ei.j(.{ "env", nuId });
            try ei.j(.{ "envinst", nuId });
            try ei.p("=");
            try ei.beginBody();

            var envIt = env.iterator();
            while (envIt.next()) |inst| {
                var i = startLine(self);
                switch (inst.v) {
                    .Var => |v| {
                        try i.j(.{ ".", v });
                        try i.p("=");
                        try i.j(.{v});
                    },
                    else => unreachable,
                }

                try i.j(",");
                try i.finish();
            }

            try endBodyAndFinishStmt(self);
        }

        // now the total
        {
            const oldCW = self.backend.cur;
            self.backend.cur = CW.init(self.backend.al);
            defer self.backend.cur = oldCW;

            var e = startLine(self);
            try e.p(.{"struct"});
            try e.j(.{ "funenv", nuId });
            try e.beginBody();

            var ifun = startLine(self);
            var paramTys = std.ArrayList(ast.TyRef).init(self.backend.al);
            defer paramTys.deinit();

            for (fun.params) |p| {
                try paramTys.append(p.t);
            }

            try ifun.definition(fun.ret, Tuple(.{
                "(*fun)",
                "(",
                Join(.{ "struct env", nuId }),
                PrependAll(", ", paramTys.items),
                ")",
            }));
            try ifun.finishStmt();

            var ienv = startLine(self);
            try ienv.p(.{"struct"});
            try ienv.j(.{ "env", nuId });
            try ienv.p("env");
            try ienv.finishStmt();

            try endBodyAndFinishStmt(self);
            try self.backend.parts.append(self.backend.cur);
        }
    }

    // decl
    const oldCW = self.backend.cur;
    self.backend.cur = CW.init(self.backend.al);
    defer self.backend.cur = oldCW;

    // prepare paramteres for deconstruction.
    const Parameter = struct {
        v: union(enum) {
            NormalVar: ast.Var,
            TempVar: Temp,
            Env: Unique, // funny! it should only happend in the beginning.
        },
        t: ast.Type,

        pub fn write(s: @This(), stmt: *Stmt) anyerror!void {
            switch (s.v) {
                .NormalVar => |v| try stmt.definition(s.t, v),
                .TempVar => |t| try stmt.definition(s.t, t),
                .Env => |nuid| {
                    try stmt.p(.{"struct"});
                    try stmt.j(.{ "env", nuid });
                    try stmt.p(.{"env"});
                },
            }
        }
    };
    var params = std.ArrayList(Parameter).init(self.backend.al);

    if (!isEnvEmpty) {
        try params.append(.{ .v = .{ .Env = nuId }, .t = undefined });
    }

    for (fun.params) |param| {
        switch (param.d) {
            .Var => |v| try params.append(.{
                .v = .{ .NormalVar = v },
                .t = param.t,
            }),
            else => try params.append(.{
                .v = .{ .TempVar = self.backend.temp() },
                .t = param.t,
            }),
        }
    }

    var d = startLine(self);
    try d.p(.{"static"});
    try d.definition(fun.ret, Tuple(.{ ast.Var{ .name = fun.name.name, .uid = nuId }, "(", SepBy(", ", params.items), ")" }));
    try d.beginBody();

    try self.monoScope(fun.body);

    try endBodyAndFinish(self);

    // add the function to places.
    try self.backend.parts.append(self.backend.cur);
    const insertion = try self.backend.functionsGenerated.getOrPut(.{ .env = fun.env, .m = m });
    std.debug.assert(!insertion.found_existing);
    insertion.value_ptr.* = nuId;
}

pub fn genStmt(self: *Self, stmt: *ast.Stmt) GenError!void {
    switch (stmt.*) {
        .VarDec => |vd| {
            var s = startLine(self);

            try s.definition(vd.varValue.t, vd.varDef);
            try s.p("=");
            try s.genExpr(vd.varValue);

            try s.finishStmt();
        },
        .VarMut => |vm| {
            var s = startLine(self);
            try s.p(.{vm.varRef});
            if (vm.accessors.len > 0) unreachable; // TODO
            try s.p("=");
            try s.genExpr(vm.varValue);
            try s.finishStmt();
        },
        .Return => |ret| {
            var s = startLine(self);

            try s.p("return");
            try s.genExpr(ret);

            try s.finishStmt();
        },
        .If => |ifstmt| {
            try ifElseStmt(self, ifstmt.cond, ifstmt.bTrue, ifstmt.bOthers, ifstmt.bElse);
        },
        .While => |wh| {
            var s = startLine(self);
            try s.p("while (");
            try s.genExpr(wh.cond);
            try s.p(")");
            try s.beginBody();

            try self.monoScope(wh.body);

            var rem = try endBody(self);
            try rem.finish();
        },
        .Expr => |expr| {
            var s = startLine(self);
            try s.genExpr(expr);
            try s.finishStmt();
        },
        else => unreachable,
    }
}

fn ifElseStmt(self: *Self, cond: *ast.Expr, ifTrue: []*ast.Stmt, elifs: []ast.Stmt.Elif, els: ?[]*ast.Stmt) !void {
    var s = startLine(self);

    try s.p("if (");
    try s.genExpr(cond);
    try s.p(")");

    try s.beginBody();
    try self.monoScope(ifTrue);
    var e = try endBody(self);

    if (elifs.len > 0) {
        try e.p("else");
        try e.beginBody();

        const elif = elifs[0];
        try ifElseStmt(self, elif.cond, elif.body, elifs[1..], els);

        var last = try endBody(self);
        try last.finish();
    } else if (els) |bElse| {
        try e.p("else");
        try e.beginBody();

        try self.monoScope(bElse);

        var last = try endBody(self);
        try last.finish();
    } else {
        try e.finish();
    }
}

pub fn genEnvCompletion(self: *Self, incompleteEnv: ast.Function.FunApp, completedEnv: ast.Function.FunApp) !void {
    self.ctx.print(.{ "||", incompleteEnv.fun.name, " ", incompleteEnv.m, "\n^^", completedEnv.fun.name, " ", completedEnv.m, "\n" });
    unreachable;
}

//////

fn startLine(self: *Self) Stmt {
    return Stmt.init(self);
}

fn endBody(self: *Self) !Stmt {
    self.backend.cur.currentIndent -= 1;
    var line = startLine(self);
    try line.p("}");
    return line;
}

fn endBodyAndFinish(self: *Self) !void {
    var l = try endBody(self);
    try l.finish();
}

fn endBodyAndFinishStmt(self: *Self) !void {
    var l = try endBody(self);
    try l.finishStmt();
}

////

const Stmt = struct {
    buf: std.ArrayList(u8),
    ctx: *Self,
    indent: u32,
    spaced: bool = false,

    fn init(ctx: *Self) @This() {
        return .{
            .buf = std.ArrayList(u8).init(ctx.backend.al),
            .ctx = ctx,
            .indent = ctx.backend.cur.currentIndent,
        };
    }

    fn deinit(self: *@This()) void {
        self.buf.deinit();
    }

    fn finishStmt(stmt: *@This()) GenError!void {
        const cw = &stmt.ctx.backend.cur;
        try stmt.buf.append(';');

        try cw.buf.append(.{
            .indent = stmt.indent,
            .line = stmt.buf.items,
        });
    }

    fn finish(stmt: *@This()) GenError!void {
        const cw = &stmt.ctx.backend.cur;

        try cw.buf.append(.{
            .indent = stmt.indent,
            .line = stmt.buf.items,
        });
    }

    fn beginBody(stmt: *@This()) anyerror!void {
        try stmt.p("{");
        try stmt.finish();
        stmt.ctx.backend.cur.currentIndent += 1;
    }

    fn genExpr(stmt: *@This(), expr: *ast.Expr) GenError!void {
        switch (expr.e) {
            .Int => |x| try stmt.p(x),
            .Con => |c| {
                if (c.tys.len == 0) {
                    const t = stmt.ctx.typeContext.getType(expr.t).Con;
                    _ = try datatype(stmt.ctx, c.data, t.application);
                    if (ast.Annotation.find(c.anns, "clit")) |ann| {
                        try stmt.p(ann.params[0]);
                    } else {
                        unreachable;
                    }
                } else {
                    unreachable;
                }
            },
            .Var => |v| switch (v.v) {
                .Var => |vv| {
                    switch (v.locality) {
                        .Local => try stmt.p(.{vv}),
                        .External => {
                            try stmt.j(.{ "env.", vv });
                        },
                    }
                },
                .Fun => |fun| {
                    const nuId = stmt.ctx.backend.functionsGenerated.get(.{ .env = fun.env, .m = v.match }).?;

                    if (fun.env.monoInsts.empty()) {
                        try stmt.p(.{ast.Var{ .name = fun.name.name, .uid = nuId }});
                    } else {
                        try stmt.p(.{"(struct"});
                        try stmt.j(.{
                            "funenv",
                            nuId,
                            "){ .fun = ",
                            ast.Var{ .name = fun.name.name, .uid = nuId },
                            ", .env = envinst",
                            nuId,
                            " }",
                        });
                    }
                },
                else => unreachable,
            },
            .Call => |call| {
                const funTy = getType(stmt.ctx, call.callee.t).Fun;
                const env = getEnv(stmt.ctx, funTy.env).env;

                if (env.monoInsts.empty()) {
                    try genExpr(stmt, call.callee);

                    try stmt.p("(");
                    if (call.args.len > 0) {
                        try genExpr(stmt, call.args[0]);
                    }

                    for (call.args[1..]) |arg| {
                        try stmt.j(", ");
                        try genExpr(stmt, arg);
                    }
                    try stmt.p(")");
                } else {
                    const t = try stmt.tempExpr(call.callee);
                    try stmt.j(.{ t, ".fun" });

                    try stmt.p("(");
                    try stmt.j(.{ t, ".env" });

                    for (call.args) |arg| {
                        try stmt.j(", ");
                        try genExpr(stmt, arg);
                    }
                    try stmt.p(")");
                }
            },
            else => unreachable,
        }
    }

    fn tempExpr(stmt: *@This(), expr: *ast.Expr) anyerror!Temp {
        const t = temp(stmt.ctx.backend);
        var s = startLine(stmt.ctx);
        try s.definition(expr.t, t);
        try s.p("=");
        try s.genExpr(expr);
        try s.finishStmt();

        return t;
    }

    fn definition(stmt: *@This(), t: ast.Type, v: anytype) GenError!void {
        try stmt.definitionBegin(t);
        try stmt.p(.{v});
        try stmt.definitionEnd(t);
    }

    fn definitionBegin(stmt: *@This(), t: ast.Type) GenError!void {
        switch (getType(stmt.ctx, t)) {
            .Con => |con| {
                try stmt.p(.{try datatype(stmt.ctx, con.type, con.application)});
            },
            .Fun => |fun| {
                const envm = getEnv(stmt.ctx, fun.env);
                const env = envm.env;
                if (env.monoInsts.empty()) {
                    try stmt.definitionBegin(fun.ret);
                    try stmt.p(.{"(*"});
                } else {
                    const nuId = stmt.ctx.backend.functionsGenerated.get(.{ .env = env, .m = envm.match }).?;
                    try stmt.p(.{"struct"});
                    try stmt.j(.{ "funenv", nuId });
                }
            },
            .TVar => unreachable,
            else => unreachable,
        }
    }

    fn definitionEnd(stmt: *@This(), t: ast.Type) GenError!void {
        switch (getType(stmt.ctx, t)) {
            .Fun => |fun| {
                const envm = getEnv(stmt.ctx, fun.env);
                const env = envm.env;
                if (env.monoInsts.empty()) {
                    try stmt.p(.{ ")(", SepBy(", ", fun.args), ")" });
                }
                try stmt.definitionEnd(fun.ret);
            },
            else => {},
        }
    }

    // prints stuff and separates by space.
    fn generalPrint(self: *@This(), args: anytype, comptime fun: anytype) !void {
        switch (@typeInfo(@TypeOf(args))) {
            .Struct => {
                const fields = @typeInfo(@TypeOf(args)).Struct.fields;
                inline for (fields) |field| {
                    const arg = @field(args, field.name);
                    try fun(self, arg);
                }
            },

            else => {
                // assume it's a single arg!
                try fun(self, args);
            },
        }
    }

    fn p(self: *@This(), args: anytype) !void {
        try self.generalPrint(args, printSpaced);
    }

    fn printSpaced(self: *@This(), args: anytype) anyerror!void {
        if (self.spaced) {
            try self.buf.append(' ');
        }

        try printArg(self, args);
        self.spaced = true;
    }

    fn j(self: *@This(), args: anytype) !void {
        if (self.spaced) {
            try self.buf.append(' ');
        }
        self.spaced = false;

        try self.generalPrint(args, printArg);
        self.spaced = true;
    }

    fn printArg(self: *@This(), arg: anytype) anyerror!void {
        const argTy = @TypeOf(arg);
        if (argTy == ast.Var) {
            const v = @as(ast.Var, arg);
            try self.j(.{ sanitize(v.name), v.uid });
        } //
        else if (argTy == Sanitize) {
            const name = (@as(Sanitize, arg)).unsanitary;
            const writer = self.buf.writer();
            for (name) |c| {
                switch (c) {
                    '-' => try writer.print("_dash_", .{}),
                    '_' => try writer.print("__", .{}),
                    '\'' => try writer.print("_prime_", .{}),
                    else => try writer.writeByte(c),
                }
            }
        } //
        else if (argTy == Temp) {
            const t = (@as(Temp, arg));
            try self.j(.{ "temp", t.id });
        } //
        else if (argTy == ast.Type) {
            const t = (@as(ast.Type, arg));
            try self.definition(t, .{});
        } //
        else if (argTy == @TypeOf(.{})) {
            return;
        } //
        else if (argTy == ast.EnvVar) {
            const inst = (@as(ast.EnvVar, arg));

            const tymap = TypeMap{
                .prev = self.ctx.backend.tymap,
                .scheme = &inst.m.scheme,
                .match = inst.m,
            };

            const oldTyMap = self.ctx.backend.tymap;
            self.ctx.backend.tymap = &tymap;
            defer self.ctx.backend.tymap = oldTyMap;

            switch (inst.v) {
                .Var => |v| try self.definition(inst.t, v),
                else => unreachable,
            }

            // try self.definition(
            //     inst.t,
            // );
        } //
        else {
            if (comptime std.meta.hasMethod(argTy, "write")) { // distinct method name to ast.Ctx
                try arg.write(self);
            } else {
                switch (@typeInfo(argTy)) {
                    .Int, .Float, .ComptimeInt, .ComptimeFloat => try self.buf.writer().print("{}", .{arg}),
                    else => try self.buf.appendSlice(@as(Str, arg)), // IF THIS CAST FAILS, IT MEANS YOU MUST ADD `pub` TO YOUR `fn print()`
                }
            }
        }
    }

    const TabSize = 4;
    fn beginLine(self: *@This()) !void {
        for (0..self.currentIndent) |_| {
            try self.p(" " ** TabSize);
        }
    }
};

fn getType(self: *const Self, ogt: ast.Type) ast.TypeF(ast.Type) {
    return switch (self.typeContext.getType(ogt)) {
        .TVar => |tv| getType(self, self.backend.tymap.getTVar(tv) orelse unreachable),
        else => |t| t,
    };
}

fn getEnv(self: *const Self, ogenv: ast.EnvRef) TypeContext.Env {
    return self.backend.tymap.getEnv(ogenv, self.typeContext).?.env;
}

const Temp = struct {
    id: Unique,
};

fn temp(self: *@This()) Temp {
    return .{ .id = self.tempgen.newUnique() };
}

const CW = struct {
    buf: std.ArrayList(Line),
    currentIndent: u32,

    const Line = struct {
        indent: u32,
        line: Str,
    };

    fn init(al: std.mem.Allocator) @This() {
        return .{
            .currentIndent = 0,
            .buf = std.ArrayList(Line).init(al),
        };
    }

    fn writeTo(self: *const @This(), writer: anytype) anyerror!void {
        for (self.buf.items) |line| {
            // indent
            for (0..line.indent) |_| {
                try writer.print(" " ** CW.TabSize, .{});
            }

            try writer.writeAll(line.line);
            try writer.writeAll("\n");
        }
    }

    const TabSize = 4;

    fn print(self: *@This(), args: anytype) !void {
        switch (@typeInfo(@TypeOf(args))) {
            .Struct => {
                const fields = @typeInfo(@TypeOf(args)).Struct.fields;
                inline for (fields) |field| {
                    const arg = @field(args, field.name);
                    try self.printArg(arg, field.type);
                }
            },

            else => {
                // assume it's a single arg!
                try self.printArg(args);
            },
        }
    }

    fn printArg(self: *@This(), arg: anytype) !void {
        switch (@typeInfo(@TypeOf(arg))) {
            .Int, .Float, .ComptimeInt, .ComptimeFloat => try self.buf.writer().print("{}", arg),
            else => try self.buf.appendSlice(@as(Str, arg)), // IF THIS CAST FAILS, IT MEANS YOU MUST ADD `pub` TO YOUR `fn print()`
        }
    }
};

const Sanitize = struct { unsanitary: Str };
fn sanitize(self: Str) Sanitize {
    return .{ .unsanitary = self };
}

fn Tuple(tup: anytype) struct {
    tup: @TypeOf(tup),

    pub fn write(self: @This(), stmt: *Stmt) anyerror!void {
        try stmt.p(self.tup);
    }
} {
    return .{ .tup = tup };
}

fn Join(tup: anytype) struct {
    tup: @TypeOf(tup),

    pub fn write(self: @This(), stmt: *Stmt) anyerror!void {
        try stmt.j(self.tup);
    }
} {
    return .{ .tup = tup };
}

fn SepBy(sep: Str, args: anytype) struct {
    sep: Str,
    args: @TypeOf(args),

    pub fn write(self: @This(), stmt: *Stmt) anyerror!void {
        if (self.args.len > 0) {
            try stmt.p(.{self.args[0]});
        }

        for (self.args[1..]) |arg| {
            try stmt.p(self.sep);
            try stmt.p(.{arg});
        }
    }
} {
    return .{ .sep = sep, .args = args };
}

fn PrependAll(sep: Str, args: anytype) struct {
    sep: Str,
    args: @TypeOf(args),

    pub fn write(self: @This(), stmt: *Stmt) anyerror!void {
        for (self.args) |arg| {
            try stmt.p(self.sep);
            try stmt.p(.{arg});
        }
    }
} {
    return .{ .sep = sep, .args = args };
}

fn datatype(self: *Self, data: *const ast.Data, application: *const ast.Match) !Str {
    if (ast.Annotation.find(data.annotations, "cstdinclude")) |ann| {
        try self.backend.imports.insert(ann.params[0]);
    }

    if (ast.Annotation.find(data.annotations, "ctype")) |ann| {
        return ann.params[0];
    }

    _ = application;
    unreachable;
}

pub const EnvApp = struct {
    env: *ast.Env,
    m: *const ast.Match,

    // pub fn print(self: @This(), c: ast.Ctx) void {
    //     c.print(.{ self.env.name, self.m });
    // }

    pub const Comparator = struct {
        typeContext: *const TypeContext,

        pub fn eql(ctx: @This(), a: EnvApp, b: EnvApp) bool {
            return a.env.id == b.env.id and ast.Match.Comparator.eql(.{ .typeContext = ctx.typeContext }, a.m, b.m);
        }

        pub fn hash(ctx: @This(), k: EnvApp) u64 {
            return k.env.id * 497192 + ast.Match.Comparator.hash(.{ .typeContext = ctx.typeContext }, k.m);
        }
    };
};
