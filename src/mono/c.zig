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
typesGenerated: TypesGenerated,
parts: std.ArrayList(CW),
cur: CW,
al: std.mem.Allocator,
tempgen: UniqueGen,

const FunctionsGenerated = std.HashMap(EnvApp, Unique, EnvApp.Comparator, std.hash_map.default_max_load_percentage);
const TypesGenerated = std.HashMap(ast.TypeApplication, Unique, ast.TypeApplication.Comparator, std.hash_map.default_max_load_percentage);
pub fn init(al: std.mem.Allocator, tc: *const TypeContext) @This() {
    var c = @This(){
        .cur = CW.init(al),
        .al = al,
        .imports = Set(Str, std.hash_map.StringContext).init(al),
        .functionsGenerated = FunctionsGenerated.initContext(al, .{ .typeContext = tc }),
        .typesGenerated = TypesGenerated.initContext(al, .{ .typeContext = tc }),
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
    const isFunEnvEmpty = isEnvEmpty(fun.env);
    if (!isFunEnvEmpty) {
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
                    .Fun => |instfun| {
                        if (isEnvEmpty(instfun.env)) {
                            continue;
                        } else {
                            const funNuId = self.backend.functionsGenerated.get(.{ .env = instfun.env, .m = try self.typeContext.mapMatch(self.backend.tymap.match, inst.m) }).?;
                            try i.j(.{ ".envinst", funNuId });
                            try i.p(.{"="});
                            try i.j(.{ "envinst", funNuId });
                        }
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

    if (!isFunEnvEmpty) {
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
        .Function => unreachable,
        .Instance => unreachable,

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
        .Pass => {},
        .Switch => |sw| {
            var s = startLine(self);
            const cond = try s.tempExpr(sw.switchOn);

            const tyApp = getType(self, sw.switchOn.t).Con;
            switch (tyApp.type.structureType()) {
                .Opaque => { // TODO: also Ints...
                    var l = startLine(self);
                    try l.beginBody();
                    {
                        // basically, handle case x/_ stuff.
                        // itll always match the first case btw.
                        const case = sw.cases[0];
                        switch (case.decon.d) {
                            .None => {
                                try self.monoScope(case.body);
                            },
                            .Var => |v| {
                                var vardef = startLine(self);

                                try vardef.definition(case.decon.t, v);
                                try vardef.p("=");
                                try vardef.p(.{cond});

                                try vardef.finishStmt();
                                try self.monoScope(case.body);
                            },
                            else => unreachable, // actually unreachable?
                        }
                    }
                    try endBodyAndFinish(self);

                    unreachable;
                },
                .EnumLike => {
                    var l = startLine(self);
                    try l.p(.{ "switch", "(", cond, ")" });
                    try l.beginBody();

                    for (sw.cases) |case| {
                        switch (case.decon.d) {
                            .None => {
                                var defcase = startLine(self);
                                try defcase.p(.{"default:"});
                                try defcase.beginBody();
                                {
                                    try self.monoScope(case.body);

                                    var breakline = startLine(self);
                                    try breakline.p("break");
                                    try breakline.finishStmt();
                                }
                                try endBodyAndFinish(self);

                                break;
                            },
                            .Var => |v| {
                                var defcase = startLine(self);
                                try defcase.p(.{"default:"});
                                try defcase.beginBody();
                                {
                                    var vardefline = startLine(self);

                                    try vardefline.definition(case.decon.t, v);
                                    try vardefline.p("=");
                                    try vardefline.p(.{cond});

                                    try vardefline.finishStmt();
                                    try self.monoScope(case.body);

                                    var breakline = startLine(self);
                                    try breakline.p("break");
                                    try breakline.finishStmt();
                                }
                                try endBodyAndFinish(self);

                                break;
                            },
                            .Con => |c| {
                                var defcase = startLine(self);
                                try defcase.p(.{"case"});
                                try defcase.constructor(c.con, tyApp);
                                try defcase.p(.{":"});
                                try defcase.beginBody();
                                {
                                    try self.monoScope(case.body);

                                    var breakline = startLine(self);
                                    try breakline.p("break");
                                    try breakline.finishStmt();
                                }
                                try endBodyAndFinish(self);
                            },
                            else => unreachable,
                        }
                    }

                    try endBodyAndFinish(self);
                },

                .ADT, .RecordLike => {
                    var ifc = startLine(self);
                    try ifc.p(.{ "if", "(" });
                    const firstCase = &sw.cases[0];
                    const hadFirstCondition = try ifc.deconCondition(firstCase.decon, cond);
                    try ifc.p(.{")"});
                    try ifc.beginBody();
                    {
                        try deconAssignments(self, firstCase.decon, cond);
                        try self.monoScope(firstCase.body);
                    }
                    try endBodyAndFinish(self);

                    // this means the check will always succeed, so no need to compile other cases.
                    if (hadFirstCondition) {
                        for (sw.cases[1..]) |*case| {
                            var eifc = startLine(self);
                            try eifc.p(.{ "else", "if", "(" });
                            const hadCondition = try eifc.deconCondition(case.decon, cond);
                            try eifc.p(.{")"});
                            try eifc.beginBody();
                            {
                                try deconAssignments(self, case.decon, cond);
                                try self.monoScope(case.body);
                            }
                            try endBodyAndFinish(self);

                            // means that this check will always succeed, so we can skip other bodies.
                            if (!hadCondition) break;
                        }
                    }
                },
            }
        },
        else => unreachable,
    }
}

fn deconAssignments(self: *Self, decon: *const ast.Decon, caseVar: Temp) !void {
    const rootDP = DeconPath{ .Tip = caseVar };
    try deconAssignments_(self, decon, &rootDP);
}

const DeconPath = union(enum) {
    Tip: Temp,
    Concat: struct {
        path: union(enum) {
            Con: struct { con: *const ast.Con, field: usize, nuId: Unique },
            Field: *const ast.Decon.Field,
            // List: *const struct {},
        },
        next: *const DeconPath,
    },

    pub fn write(self: @This(), stmt: *Stmt) anyerror!void {
        switch (self) {
            .Tip => |t| try stmt.j(.{t}),
            .Concat => |concat| {
                try concat.next.write(stmt);
                switch (concat.path) {
                    .Con => |con| {
                        const dataType = con.con.data.structureType();
                        std.debug.assert(dataType != .Opaque);
                        std.debug.assert(dataType != .EnumLike);

                        switch (dataType) {
                            .RecordLike => {
                                try stmt.j(.{ ".", "field", con.field });
                            },
                            .ADT => {
                                try stmt.j(.{".stuff"});
                                try stmt.j(.{
                                    ".",
                                    sanitize(con.con.data.name),
                                    "_",
                                    con.nuId,
                                    "_",
                                    sanitize(con.con.name),
                                    "_s",
                                });
                                try stmt.j(.{ ".field", con.field });
                            },
                            else => unreachable, // actually unreachable
                        }
                    },
                    .Field => unreachable,
                }
            },
        }
    }
};
fn deconAssignments_(self: *Self, decon: *const ast.Decon, dp: *const DeconPath) !void {
    switch (decon.d) {
        .None => return,
        .Var => |v| {
            var l = startLine(self);
            try l.definition(decon.t, v);
            try l.p("=");
            try l.p(.{dp});
            try l.finishStmt();
        },
        .Num => unreachable,
        .Con => |con| {
            const tyApp = self.typeContext.getType(decon.t).Con;
            const nuId = self.backend.typesGenerated.get(tyApp).?;
            const dataType = con.con.data.structureType();

            switch (dataType) {
                .Opaque => unreachable,
                .EnumLike => return,
                .RecordLike => {
                    for (con.decons, 0..) |cd, i| {
                        const nextDP = DeconPath{
                            .Concat = .{
                                .path = .{
                                    .Con = .{
                                        .con = con.con,
                                        .field = i,
                                        .nuId = nuId,
                                    },
                                },
                                .next = dp,
                            },
                        };
                        try deconAssignments_(self, cd, &nextDP);
                    }
                },
                .ADT => {
                    for (con.decons, 0..) |cd, i| {
                        const nextDP = DeconPath{
                            .Concat = .{
                                .path = .{
                                    .Con = .{
                                        .con = con.con,
                                        .field = i,
                                        .nuId = nuId,
                                    },
                                },
                                .next = dp,
                            },
                        };
                        try deconAssignments_(self, cd, &nextDP);
                    }
                },
            }
        },
        .Record => unreachable,
        .List => unreachable,
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
        try stmt.p("(");
        switch (expr.e) {
            .Int => |x| try stmt.p(x),
            .Con => |c| {
                if (c.tys.len == 0) {
                    const t = stmt.ctx.typeContext.getType(expr.t).Con;
                    _ = try stmt.constructor(c, t);
                } else {
                    const dataTy = stmt.ctx.typeContext.getType(expr.t).Fun.ret;
                    const app = stmt.ctx.typeContext.getType(dataTy).Con;
                    _ = try stmt.constructor(c, app);
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
                    try stmt.function(fun, v.match, v.locality);
                },
                .ExternalFun => |efn| {
                    if (ast.Annotation.find(efn.anns, "cfunname")) |ann| {
                        try stmt.p(.{ann.params[0]});
                    } else {
                        try stmt.p(.{efn.name});
                    }

                    if (ast.Annotation.find(efn.anns, "cstdinclude")) |ann| {
                        try stmt.ctx.backend.imports.insert(ann.params[0]);
                    }
                },
                .ClassFun => |cfun| {
                    try stmt.instFun(cfun.ref);
                },
                else => unreachable,
            },
            .Call => |call| {
                const funTy = getType(stmt.ctx, call.callee.t).Fun;
                const env = getEnv(stmt.ctx, funTy.env).env;

                if (isEnvEmpty(env)) {
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
            .Str => |s| {
                const writer = stmt.buf.writer();

                if (stmt.spaced) {
                    try writer.writeByte(' ');
                }
                try writer.writeByte('"');
                try std.zig.stringEscape(s, "", .{}, stmt.buf.writer());
                try writer.writeByte('"');
            },
            .Intrinsic => |intr| {
                switch (intr.intr.ty) {
                    .@"i64-add" => {
                        try stmt.genExpr(intr.args[0]);
                        try stmt.p("+");
                        try stmt.genExpr(intr.args[1]);
                    },
                    else => unreachable,
                }
            },
            .NamedRecord => |rec| {
                const tyApp = getType(stmt.ctx, expr.t).Con;
                std.debug.assert(tyApp.type.eq(rec.data));
                const tyName = try datatype(stmt.ctx, tyApp);

                try stmt.j(.{ "(", tyName, ")" });
                try stmt.p("{");
                for (rec.fields, 0..) |field, i| {
                    if (i != 0) {
                        try stmt.j(",");
                    }
                    try stmt.j(.{ ".", field.field });
                    try stmt.p("=");
                    try stmt.genExpr(field.value);
                }
                try stmt.p("}");
            },
            .UnOp => |unop| {
                switch (unop.op) {
                    .Access => |mem| {
                        try stmt.genExpr(unop.e);
                        try stmt.j(.{ ".", mem });
                    },
                    .As => {
                        try stmt.genExpr(unop.e);
                    },
                    else => unreachable,
                }
            },
            .BinOp => |binop| {
                switch (binop.op) {
                    .Plus => |inst| {
                        try stmt.instFun(inst);
                        try stmt.p(.{"("});
                        try stmt.genExpr(binop.l);
                        try stmt.p(.{","});
                        try stmt.genExpr(binop.r);
                        try stmt.p(.{")"});
                    },
                    else => unreachable,
                }
            },
            else => unreachable,
        }
        try stmt.p(")");
    }

    fn instFun(stmt: *@This(), inst: ast.InstFunInst) !void {
        const aref = inst.*.?;
        switch (aref) {
            .InstFun => |ifun| {
                try stmt.function(ifun.fun, ifun.m, ifun.locality);
            },
            .Id => unreachable,
        }
    }

    fn function(stmt: *@This(), fun: *const ast.Function, m: *const ast.Match, locality: ast.Locality) !void {
        const nuId = stmt.ctx.backend.functionsGenerated.get(.{ .env = fun.env, .m = try stmt.ctx.typeContext.mapMatch(stmt.ctx.backend.tymap.match, m) }).?;

        if (isEnvEmpty(fun.env)) {
            try stmt.p(.{ast.Var{ .name = fun.name.name, .uid = nuId }});
        } else {
            try stmt.p(.{"(struct"});
            try stmt.j(.{
                "funenv",
                nuId,
                "){ .fun = ",
                ast.Var{ .name = fun.name.name, .uid = nuId },
                ", .env = ",
                if (locality == .Local) "" else "env.",
                "envinst",
                nuId,
                " }",
            });
        }
    }

    fn constructor(stmt: *@This(), con: *ast.Con, tyApp: ast.TypeApplication) !void {
        const tn = try datatype(stmt.ctx, tyApp);
        if (ast.Annotation.find(con.anns, "clit")) |ann| {
            try stmt.p(ann.params[0]);
        } else {
            const nuId = tn.Application.id;
            if (con.tys.len == 0 and con.data.structureType() == .ADT) {
                try stmt.j(.{ "(", "struct ", sanitize(con.data.name), "_", nuId, ")" });
                try stmt.p(.{ "{", ".tag", "=" });
                try stmt.j(.{
                    sanitize(con.data.name),
                    "_",
                    nuId,
                    "_",
                    sanitize(con.name),
                    "_t",
                });
                try stmt.p(.{"}"});
            } else {
                try stmt.j(.{ sanitize(con.data.name), "_", nuId, "_", sanitize(con.name) });
            }
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
                try stmt.p(.{try datatype(stmt.ctx, con)});
            },
            .Fun => |fun| {
                const envm = getEnv(stmt.ctx, fun.env);
                const env = envm.env;
                if (env.monoInsts.empty()) {
                    try stmt.definitionBegin(fun.ret);
                    try stmt.p(.{"(*"});
                } else {
                    const nuId = stmt.ctx.backend.functionsGenerated.get(.{ .env = env, .m = try stmt.ctx.typeContext.mapMatch(stmt.ctx.backend.tymap.match, envm.match) }).?;
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

    fn deconCondition(self: *@This(), decon: *const ast.Decon, condVar: Temp) !bool {
        var hadCondition = false;
        const dp = DeconPath{ .Tip = condVar };
        try self.deconCondition_(&hadCondition, decon, &dp);
        if (!hadCondition) {
            try self.ctx.backend.imports.insert("stdbool.h");
            try self.p("true");
        }
        return hadCondition;
    }

    fn deconCondition_(self: *@This(), hadCondition: *bool, decon: *const ast.Decon, dp: *const DeconPath) !void {
        switch (decon.d) {
            .None => return,
            .Var => return,
            .Num => unreachable,
            .Con => |con| {
                const tyApp = self.ctx.typeContext.getType(decon.t).Con;
                const nuId = self.ctx.backend.typesGenerated.get(tyApp).?;
                const dataType = con.con.data.structureType();

                switch (dataType) {
                    .Opaque => unreachable,
                    .EnumLike => {
                        try self.deconConnect(hadCondition);
                        try self.p(.{ dp, "==" });
                        try self.j(.{
                            sanitize(con.con.data.name),
                            "_",
                            nuId,
                            "_",
                            sanitize(con.con.name),
                        });
                    },
                    .RecordLike => {
                        for (con.decons, 0..) |cd, i| {
                            const nextDP = DeconPath{
                                .Concat = .{
                                    .path = .{
                                        .Con = .{
                                            .con = con.con,
                                            .field = i,
                                            .nuId = nuId,
                                        },
                                    },
                                    .next = dp,
                                },
                            };
                            try self.deconCondition_(hadCondition, cd, &nextDP);
                        }
                    },
                    .ADT => {
                        try self.deconConnect(hadCondition);
                        try self.j(.{ dp, ".tag" });
                        try self.p("==");
                        try self.j(.{
                            sanitize(con.con.data.name),
                            "_",
                            nuId,
                            "_",
                            sanitize(con.con.name),
                            "_t",
                        });

                        for (con.decons, 0..) |cd, i| {
                            const nextDP = DeconPath{
                                .Concat = .{
                                    .path = .{
                                        .Con = .{
                                            .con = con.con,
                                            .field = i,
                                            .nuId = nuId,
                                        },
                                    },
                                    .next = dp,
                                },
                            };
                            try self.deconCondition_(hadCondition, cd, &nextDP);
                        }
                    },
                }
            },
            .Record => unreachable,
            .List => unreachable,
        }
    }

    fn deconConnect(self: *@This(), hadCondition: *bool) !void {
        if (hadCondition.*) {
            try self.p("&&");
        } else {
            hadCondition.* = true;
        }
    }

    /////////////////////

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
        else if (argTy == TypeName) {
            const tyname = (@as(TypeName, arg));
            switch (tyname) {
                .Defined => |def| try self.j(def),
                .Application => |app| {
                    switch (app.data.structureType()) {
                        .EnumLike => try self.p("enum"),
                        else => try self.p("struct"),
                    }
                    try self.j(.{ app.data.name, "_", app.id });
                },
            }
        } //
        else if (argTy == @TypeOf(.{})) {
            return;
        } // used when generating the env struct.
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
                .Fun => |fun| {
                    if (isEnvEmpty(fun.env)) {
                        return; // nutting.
                    } else {
                        const nuId = self.ctx.backend.functionsGenerated.get(.{ .env = fun.env, .m = try self.ctx.typeContext.mapMatch(self.ctx.backend.tymap.match, inst.m) }).?;
                        try self.p(.{"struct"});
                        try self.j(.{ "env", nuId });
                        try self.j(.{ "envinst", nuId });
                    }
                },
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

fn isEnvEmpty(self: *const ast.Env) bool {
    var it = self.monoInsts.iterator();
    while (it.next()) |inst| {
        switch (inst.v) {
            .Var => return false,
            .Fun => |fun| {
                if (!isEnvEmpty(fun.env)) return false;
            },
            else => unreachable,
        }
    }

    return true;
}

///
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

const TypeName = union(enum) {
    Defined: Str,
    Application: struct { data: *const ast.Data, id: Unique },
};
fn datatype(self: *Self, tyApp: ast.TypeApplication) !TypeName {
    const data = tyApp.type;
    if (ast.Annotation.find(data.annotations, "cstdinclude")) |ann| {
        try self.backend.imports.insert(ann.params[0]);
    }

    if (ast.Annotation.find(data.annotations, "ctype")) |ann| {
        return .{ .Defined = ann.params[0] };
    }

    switch (data.stuff) {
        .cons => |cons| {
            switch (data.structureType()) {
                .Opaque => unreachable,
                .EnumLike => {
                    // TEMP: COPYPASTA
                    const gpr = try self.backend.typesGenerated.getOrPut(tyApp);
                    if (gpr.found_existing) {
                        return .{ .Application = .{ .data = data, .id = gpr.value_ptr.* } };
                    }

                    const oldTyMap = self.backend.tymap;
                    const outerTVScheme = ast.Scheme{
                        .tvars = data.outerTVars,
                        .envVars = &.{},
                        .associations = &.{},
                        .env = null,
                    };
                    const outerTVMatch = ast.Match{
                        .tvars = tyApp.outerApplication,
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
                        .scheme = &data.scheme,
                        .match = tyApp.application,
                    };
                    self.backend.tymap = &tymap;
                    defer self.backend.tymap = oldTyMap;

                    const nuId = self.backend.temp().id;
                    gpr.value_ptr.* = nuId;

                    // generate dat data definition.
                    {
                        const oldCW = self.backend.cur;
                        self.backend.cur = CW.init(self.backend.al);
                        defer self.backend.cur = oldCW;

                        // inst
                        var e = startLine(self);
                        try e.p(.{"enum"});
                        try e.j(.{ data.name, "_", nuId });
                        try e.beginBody();

                        for (cons) |con| {
                            var l = startLine(self);
                            try l.j(.{ sanitize(data.name), "_", nuId, "_", sanitize(con.name), "," });
                            try l.finish();
                        }

                        try endBodyAndFinishStmt(self);
                        try self.backend.parts.append(self.backend.cur);
                    }

                    return .{ .Application = .{ .data = data, .id = gpr.value_ptr.* } };
                },
                .RecordLike => {
                    const con = &cons[0];

                    // TEMP: COPYPASTA
                    const gpr = try self.backend.typesGenerated.getOrPut(tyApp);
                    if (gpr.found_existing) {
                        return .{ .Application = .{ .data = data, .id = gpr.value_ptr.* } };
                    }

                    const oldTyMap = self.backend.tymap;
                    const outerTVScheme = ast.Scheme{
                        .tvars = data.outerTVars,
                        .envVars = &.{},
                        .associations = &.{},
                        .env = null,
                    };
                    const outerTVMatch = ast.Match{
                        .tvars = tyApp.outerApplication,
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
                        .scheme = &data.scheme,
                        .match = tyApp.application,
                    };
                    self.backend.tymap = &tymap;
                    defer self.backend.tymap = oldTyMap;

                    const nuId = self.backend.temp().id;
                    gpr.value_ptr.* = nuId;

                    // generate dat data definition.
                    {
                        const oldCW = self.backend.cur;
                        self.backend.cur = CW.init(self.backend.al);
                        defer self.backend.cur = oldCW;

                        try genRecordStruct(self, nuId, con, .RecordLike);
                        try self.backend.parts.append(self.backend.cur);
                    }

                    // constructor
                    {
                        const oldCW = self.backend.cur;
                        self.backend.cur = CW.init(self.backend.al);
                        defer self.backend.cur = oldCW;

                        try genRecordConstructor(self, nuId, con, .RecordLike);
                        try self.backend.parts.append(self.backend.cur);
                    }

                    return .{ .Application = .{ .data = data, .id = gpr.value_ptr.* } };
                },
                .ADT => {
                    // TEMP: COPYPASTA
                    const gpr = try self.backend.typesGenerated.getOrPut(tyApp);
                    if (gpr.found_existing) {
                        return .{ .Application = .{ .data = data, .id = gpr.value_ptr.* } };
                    }

                    const oldTyMap = self.backend.tymap;
                    const outerTVScheme = ast.Scheme{
                        .tvars = data.outerTVars,
                        .envVars = &.{},
                        .associations = &.{},
                        .env = null,
                    };
                    const outerTVMatch = ast.Match{
                        .tvars = tyApp.outerApplication,
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
                        .scheme = &data.scheme,
                        .match = tyApp.application,
                    };
                    self.backend.tymap = &tymap;
                    defer self.backend.tymap = oldTyMap;

                    const nuId = self.backend.temp().id;
                    gpr.value_ptr.* = nuId;

                    // generate dat data definition.
                    {
                        const oldCW = self.backend.cur;
                        self.backend.cur = CW.init(self.backend.al);
                        defer self.backend.cur = oldCW;
                        var e = startLine(self);
                        try e.p(.{"struct"});
                        try e.j(.{ data.name, "_", nuId });
                        try e.beginBody();
                        {

                            // TAG
                            {
                                var tagline = startLine(self);
                                try tagline.p("enum");
                                try tagline.beginBody();
                                for (cons) |*con| {
                                    var thistagline = startLine(self);
                                    try thistagline.j(.{
                                        sanitize(data.name),
                                        "_",
                                        nuId,
                                        "_",
                                        sanitize(con.name),
                                        "_t",
                                        ",",
                                    });
                                    try thistagline.finish();
                                }

                                var endtag = try endBody(self);
                                try endtag.p(.{"tag"});
                                try endtag.finishStmt();
                            }

                            // UNION
                            {
                                var u = startLine(self);
                                try u.p(.{"union"});
                                try u.beginBody();
                                {
                                    for (cons) |*con| {
                                        if (con.tys.len == 0) continue;
                                        var thistagline = try genRecordStruct(self, nuId, con, .ADT);
                                        try thistagline.j(.{
                                            sanitize(data.name),
                                            "_",
                                            nuId,
                                            "_",
                                            sanitize(con.name),
                                            "_s",
                                        });
                                        try thistagline.finishStmt();
                                    }
                                }
                                var endu = try endBody(self);
                                try endu.p(.{"stuff"});
                                try endu.finishStmt();
                            }
                        }
                        try endBodyAndFinishStmt(self);
                        try self.backend.parts.append(self.backend.cur);
                    }

                    // make constructors
                    {
                        for (cons) |*con| {
                            if (con.tys.len == 0) continue;
                            const oldCW = self.backend.cur;
                            self.backend.cur = CW.init(self.backend.al);
                            defer self.backend.cur = oldCW;

                            try genRecordConstructor(self, nuId, con, .ADT);
                            try self.backend.parts.append(self.backend.cur);
                        }
                    }

                    return .{ .Application = .{ .data = data, .id = gpr.value_ptr.* } };
                },
            }
        },
        .recs => |fields| {
            const gpr = try self.backend.typesGenerated.getOrPut(.{ .type = data, .application = tyApp.application, .outerApplication = tyApp.outerApplication });
            if (gpr.found_existing) {
                return .{ .Application = .{ .data = data, .id = gpr.value_ptr.* } };
            }

            const oldTyMap = self.backend.tymap;
            const outerTVScheme = ast.Scheme{
                .tvars = data.outerTVars,
                .envVars = &.{},
                .associations = &.{},
                .env = null,
            };
            const outerTVMatch = ast.Match{
                .tvars = tyApp.outerApplication,
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
                .scheme = &data.scheme,
                .match = tyApp.application,
            };
            self.backend.tymap = &tymap;
            defer self.backend.tymap = oldTyMap;

            const nuId = self.backend.temp().id;
            gpr.value_ptr.* = nuId;

            // generate dat data definition.
            const oldCW = self.backend.cur;
            self.backend.cur = CW.init(self.backend.al);
            defer self.backend.cur = oldCW;

            var e = startLine(self);
            try e.p(.{"struct"});
            try e.j(.{ data.name, "_", nuId });
            try e.beginBody();

            for (fields) |field| {
                var i = startLine(self);
                try i.definition(field.t, field.field);
                try i.finishStmt();
            }

            try endBodyAndFinishStmt(self);
            try self.backend.parts.append(self.backend.cur);

            return .{ .Application = .{ .data = data, .id = gpr.value_ptr.* } };
        },
    }
}

fn genRecordStruct(self: *Self, nuId: Unique, con: *const ast.Con, comptime dataType: ast.Data.StructureType) !(if (dataType == .ADT) Stmt else void) {
    const data = con.data;

    std.debug.assert(data.structureType() == dataType);
    std.debug.assert(dataType != .Opaque and dataType != .EnumLike);

    // inst
    var e = startLine(self);
    try e.p(.{"struct"});
    if (dataType != .ADT) {
        try e.j(.{ data.name, "_", nuId });
    }
    try e.beginBody();

    for (con.tys, 0..) |t, i| {
        var l = startLine(self);
        try l.definition(t, Join(.{ "field", i }));
        try l.finishStmt();
    }

    if (dataType != .ADT) {
        try endBodyAndFinishStmt(self);
        return;
    } else {
        return try endBody(self);
    }
}

fn genRecordConstructor(self: *Self, nuId: Unique, con: *const ast.Con, comptime dataType: ast.Data.StructureType) !void {
    const data = con.data;

    std.debug.assert(data.structureType() == dataType);
    std.debug.assert(dataType != .Opaque and dataType != .EnumLike);

    // inst
    var e = startLine(self);
    try e.p(.{ "static", "struct" });
    try e.j(.{ sanitize(data.name), "_", nuId });
    try e.j(.{ sanitize(con.data.name), "_", nuId, "_", sanitize(con.name) });
    try e.j("(");
    for (con.tys, 0..) |t, i| {
        if (i != 0) {
            try e.p(",");
        }
        try e.definition(t, Join(.{ "field", i }));
    }
    try e.j(")");
    try e.beginBody();

    var l = startLine(self);
    try l.p(.{ "return", "(" });
    try l.p(.{"struct"});
    try l.j(.{ sanitize(data.name), "_", nuId });
    try l.p(.{")"});
    if (dataType == .ADT) {
        try l.p("{");
        try l.p(.{
            ".tag =",
        });
        try l.j(.{
            sanitize(data.name),
            "_",
            nuId,
            "_",
            sanitize(con.name),
            "_t",
            ",",
        });
        try l.p(.{".stuff = {"});
        try l.j(.{
            ".",
            sanitize(data.name),
            "_",
            nuId,
            "_",
            sanitize(con.name),
            "_s",
        });
        try l.p(.{"="});
    }
    try l.p("{");
    for (con.tys, 0..) |_, i| {
        if (i != 0) {
            try l.p(", ");
        }
        try l.j(.{ ".field", i, " = field", i });
    }
    try l.p(.{"}"});
    if (dataType == .ADT) {
        try l.p(.{ "}", "}" });
    }

    try l.finishStmt();
    try endBodyAndFinishStmt(self);
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
