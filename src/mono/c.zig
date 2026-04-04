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
const MatchLink = TypeContext.MatchLink;

const Debug = mono.Debug;

///
const Self = mono.Mono(@This());
pub const Mono = Self;

// out: Writer,
imports: Set(Str, std.hash_map.StringContext),
functionsGenerated: FunctionsGenerated,
envsGenerated: EnvsGenerated,
envInstsGenerated: EnvInstsGenerated,
typesGenerated: TypesGenerated,
anonsGenerated: AnonsGenerated,
parts: std.ArrayList(CW),
cur: CW,
al: std.mem.Allocator,
tempgen: UniqueGen,
aux: struct { // RETARDED
    i64cmp: bool = false,
    i32cmp: bool = false,
    u32cmp: bool = false,
    u8cmp: bool = false,
    sizecmp: bool = false,

    sighandler: bool = false,
},

const FunctionsGenerated = std.HashMap(EnvApp, ?FunGen, EnvApp.Comparator, std.hash_map.default_max_load_percentage);
const FunGen = struct {
    id: Unique,
    type: enum {
        Function,
        Recursive,
    },
};
const EnvsGenerated = std.HashMap(EnvApp, ?Unique, EnvApp.Comparator, std.hash_map.default_max_load_percentage);
const EnvInstsGenerated = Set(Unique, std.hash_map.AutoContext(Unique)); // Env's Unique => ()
const TypesGenerated = std.HashMap(ast.TypeApplication, Unique, ast.TypeApplication.Comparator, std.hash_map.default_max_load_percentage);
const AnonsGenerated = std.HashMap([]Field, Unique, struct {
    typeContext: *const TypeContext,

    pub fn eql(ctx: @This(), ls: []Field, rs: []Field) bool {
        return Field.anonEq(ls, rs, ctx.typeContext);
    }

    pub fn hash(ctx: @This(), k: []Field) u64 {
        _ = ctx;
        _ = k;
        return 0; // TESTIN EQUALITY
    }
}, std.hash_map.default_max_load_percentage);
pub fn init(al: std.mem.Allocator, tc: *const TypeContext) @This() {
    var c = @This(){
        .cur = CW.init(al),
        .al = al,
        .imports = Set(Str, std.hash_map.StringContext).init(al),
        .functionsGenerated = FunctionsGenerated.initContext(al, .{ .typeContext = tc }),
        .envsGenerated = EnvsGenerated.initContext(al, .{ .typeContext = tc }),
        .envInstsGenerated = EnvInstsGenerated.init(al),
        .typesGenerated = TypesGenerated.initContext(al, .{ .typeContext = tc }),
        .anonsGenerated = AnonsGenerated.initContext(al, .{ .typeContext = tc }),
        .parts = std.ArrayList(CW).init(al),
        .tempgen = UniqueGen.init(),
        .aux = .{},
    };

    c.cur.currentIndent += 1;
    return c;
}

pub fn writeTo(self: *const @This(), writer: anytype) anyerror!void {
    var importIt = self.imports.iterator();
    while (importIt.next()) |importName| {
        try writer.print("#include <{s}>\n", .{importName.*});
    }

    try writer.print("int _global_argc;\n", .{});
    try writer.print("const char** _global_argv;\n", .{});

    for (self.parts.items) |fun| {
        try fun.writeTo(writer);
        try writer.print("\n", .{});
    }

    try writer.print("int main(int argc, const char **argv) {{\n", .{});
    try writer.print("\t_global_argc = argc; _global_argv = argv;\n", .{});
    try self.cur.writeTo(writer);
    try writer.print("}}\n", .{});
}

pub fn genFunction(self: *Self, fun: *ast.Function) GenError!void { // TODO: params
    _ = try genFunctionForRealForReal(self, fun);
}

pub fn genFunctionForRealForReal(self: *Self, fun: *ast.Function) !FunGen {
    return try genFunctionForReal(self, fun.name.name, fun.params, fun.ret, fun.env, fun, .{ .Body = fun.body });
}

fn genFunctionForReal(self: *Self, name: Str, params: []ast.DeconBase, ret: ast.Type, env: *ast.Env, fun: ?*ast.Function, body: union(enum) { Expr: *ast.Expr, Body: []*ast.Stmt }) !FunGen {
    const isFunEnvEmpty = try isEnvEmpty(self, env);

    const m = self.tymap.match;

    if (Debug) {
        self.ctx.print(.{ name, ": " });
        var tm: ?*const TypeMap = self.tymap;
        while (tm) |ttm| {
            defer tm = ttm.prev;
            self.ctx.print(.{ ttm.match, " " });
        }
        self.ctx.print(.{"\n"});
    }
    const envapp = try EnvApp.fromEnv(self, .{ .env = env, .fun = fun }, m);
    {
        // self.ctx.print(.{ "fun: ", name, " ", m, "\n" });
        const gp = try self.backend.functionsGenerated.getOrPut(envapp);
        if (gp.found_existing) {
            // here if it's null, it means it's recursive
            return gp.value_ptr.*.?;
        }
        gp.value_ptr.* = null;
    }

    // const funt = FunInst{
    //     .args = funtParams,
    //     .ret = try self.typeContext.mapType(m, ret),
    //     .env = try self.typeContext.newEnv(.{
    //         .env = env,
    //         .fun = fun,
    //         .match = m,
    //         .level = env.level,
    //     }),
    // };

    const nuId = (try genEnvStruct(self, .{ .env = env, .fun = fun }, m)) orelse self.backend.tempgen.newUnique();
    try self.backend.functionsGenerated.put(envapp, .{ .id = nuId, .type = .Recursive });

    {
        // decl
        const oldCW = self.backend.cur;
        self.backend.cur = CW.init(self.backend.al);
        defer self.backend.cur = oldCW;

        var typarams = std.ArrayList(Parameter).init(self.backend.al);

        if (!isFunEnvEmpty) {
            try typarams.append(.{ .v = .{ .Env = nuId }, .t = undefined });
        }

        for (params) |param| {
            switch (param.d.d) {
                .Var => |v| try typarams.append(.{
                    .v = .{ .NormalVar = v },
                    .t = try self.typeContext.mapType(m, param.d.t),
                }),
                else => {
                    try typarams.append(.{
                        .v = .{ .RefVar = param.refvar },
                        .t = try self.typeContext.mapType(m, param.d.t),
                    });
                },
            }
        }

        var d = startLine(self);
        try d.p(.{"static"});
        try d.definition(ret, Tuple(.{ ast.Var{ .name = name, .uid = nuId }, "(", SepBy(", ", typarams.items), ")" }));
        try d.beginBody();
        {
            const paramMatch = if (isFunEnvEmpty) typarams.items else typarams.items[1..];

            skipCondition: {
                var condl = startLine(self);
                try condl.p(.{ "if", "(!(" });
                var hadCondition = false;
                for (params, paramMatch) |decon, param| {
                    switch (param.v) {
                        .RefVar => {
                            const dp = ast.Decon.PathM{ .Tip = .{ .t = decon.d.t, .v = decon.refvar } }; // NOCHECKIN KILL
                            try condl.deconCondition_(&hadCondition, decon.d, &dp);
                        },
                        else => continue,
                    }
                }
                if (!hadCondition) {
                    condl.deinit();
                    break :skipCondition;
                }
                try condl.p(.{"))"});
                try condl.beginBody();
                {
                    try genPanic(self, "pattern not matched to enter function"); // TODO: specify name
                }
                try endBodyAndFinish(self);
            }

            // assign params yo
            for (params, paramMatch) |decon, param| {
                switch (param.v) {
                    .RefVar => {
                        try deconAssignments(self, decon.d, decon.refvar);
                    },
                    else => continue,
                }
            }

            switch (body) {
                .Body => |bod| try self.monoScope(bod),
                .Expr => |expr| {
                    var retStmt = ast.Stmt{ .Return = expr };
                    var nubod: [1]*ast.Stmt = .{&retStmt};

                    try self.monoScope(&nubod);
                },
            }
        }
        try endBodyAndFinish(self);

        // add the function to places.
        try self.backend.parts.append(self.backend.cur);
        try self.backend.functionsGenerated.put(envapp, .{ .id = nuId, .type = .Function });
    }

    // generate env inst
    if (!isFunEnvEmpty) {
        var ei = startLine(self);
        try ei.p(.{"struct"});
        try ei.j(.{ "env", nuId });
        try ei.j(.{ "envinst", nuId });
        try ei.p("=");
        try ei.beginBody();

        var deduppedEnv = try env.deduplicatedEnvInsts(self.backend.al, self.tymap, self.typeContext);
        defer deduppedEnv.deinit();
        var envIt = deduppedEnv.iterator();
        while (envIt.next()) |inst| {
            var i = startLine(self);
            switch (inst.v) {
                .Var => |v| {
                    try i.j(.{ ".", v.v });
                    try i.p("=");
                    switch (inst.locality(env)) {
                        .Local => try i.localVarThatCouldPossiblyBeDeconed(v),
                        .External => {
                            try i.j(.{ "env.", v.v });
                        },
                    }
                },
                .Fun => |instfun| {
                    const mFunNuId = (try genEnvStruct(self, .{ .env = instfun.env, .fun = instfun }, inst.m));
                    if (mFunNuId) |funNuId| {
                        try i.j(.{ ".envinst", funNuId });
                        try i.p(.{"="});
                        switch (inst.locality(env)) {
                            .Local => try i.j(.{ "envinst", funNuId }),
                            .External => {
                                try i.j(.{ "env.", "envinst", funNuId });
                            },
                        }
                    } else continue;
                },
                .ClassFun => |cfun| {
                    switch (cfun.ref.*.?) {
                        .Id => |id| {
                            const funm = self.tymap.tryGetFunctionByID(id).?;
                            const instfun = funm.fun;
                            const mFunNuId = (try genEnvStruct(self, .{ .env = instfun.env, .fun = instfun }, funm.m));
                            if (mFunNuId) |funNuId| {
                                try i.j(.{ ".envinst", funNuId });
                                try i.p(.{"="});
                                switch (inst.locality(env)) {
                                    .Local => try i.j(.{ "envinst", funNuId }),
                                    .External => {
                                        try i.j(.{ "env.", "envinst", funNuId });
                                    },
                                }
                            } else continue;
                        },
                        .InstFun => unreachable,
                    }
                },
                else => unreachable,
            }

            try i.j(",");
            try i.finish();
        }

        try endBodyAndFinishStmt(self);
    }

    return .{ .id = nuId, .type = .Function };
}
// prepare paramteres for deconstruction.
const Parameter = struct {
    v: union(enum) {
        NormalVar: ast.Var,
        RefVar: ast.Var,
        Env: Unique, // funny! it should only happend in the beginning.
    },
    t: ast.Type,

    pub fn write(s: @This(), stmt: *Stmt) anyerror!void {
        switch (s.v) {
            .NormalVar => |v| try stmt.definition(s.t, v),
            .RefVar => |t| try stmt.definition(s.t, t),
            .Env => |nuid| {
                try stmt.p(.{"struct"});
                try stmt.j(.{ "env", nuid });
                try stmt.p(.{"env"});
            },
        }
    }
};

// generate the env struct
pub fn genEnvStruct(self: *Self, envfun: ast.EnvFun, um: *const ast.Match) !?Unique {
    const oldTymap = self.tymap;
    defer self.tymap = oldTymap;
    const m = try self.typeContext.mapMatch(self.tymap, um);
    self.tymap = &TypeMap.init(m, self.tymap);

    if (try isEnvEmpty(self, envfun.env)) {
        return null;
    }

    const envapp = try EnvApp.fromEnv(self, envfun, m);
    {
        const gp = try self.backend.envsGenerated.getOrPut(envapp);
        if (gp.found_existing) return gp.value_ptr.*.?;

        // NOTE: for recursive functions you should initialize them immediately
        gp.value_ptr.* = null; // safety null :face-blushing-emoji:
    }

    const nuId = self.backend.tempgen.newUnique();
    const env = envfun.env;
    {
        // generate env
        {
            const oldCW = self.backend.cur;
            self.backend.cur = CW.init(self.backend.al);
            defer self.backend.cur = oldCW;

            var e = startLine(self);
            try e.p(.{"struct"});
            try e.j(.{ "env", nuId });
            try e.beginBody();

            var deduppedEnv = try env.deduplicatedEnvInsts(self.backend.al, self.tymap, self.typeContext);
            defer deduppedEnv.deinit();
            var envIt = deduppedEnv.iterator();
            while (envIt.next()) |inst| {
                var i = startLine(self);
                try i.p(.{inst.*});
                try i.finishStmt();
            }

            try endBodyAndFinishStmt(self);
            try self.backend.parts.append(self.backend.cur);
        }
    }

    try self.backend.envsGenerated.put(envapp, nuId);
    return nuId;
}

// gen env inst struct if not generated AND create an instantiation in "current place"
pub fn genEnvInst(self: *Self, params: []ast.Type, ret: ast.Type, nuId: Unique) !void {
    if (self.backend.envInstsGenerated.contains(nuId)) return;

    // generate the env inst thing
    {
        const oldCW = self.backend.cur;
        self.backend.cur = CW.init(self.backend.al);
        defer self.backend.cur = oldCW;

        var e = startLine(self);
        try e.p(.{"struct"});
        try e.j(.{ "funenv", nuId });
        try e.beginBody();

        var ifun = startLine(self);

        try ifun.definition(ret, Tuple(.{
            "(*fun)",
            "(",
            Join(.{ "struct env", nuId }),
            PrependAll(", ", params),
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

    try self.backend.envInstsGenerated.insert(nuId);
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
            for (vm.accessors) |acc| {
                switch (acc.acc) {
                    .Deref => try s.p("(*"),
                    .Access => {},
                }
            }
            switch (vm.locality) {
                .Local => try s.localVarThatCouldPossiblyBeDeconed(vm.varRef),
                .External => try s.j(.{ "env.", vm.varRef.v }),
            }

            for (vm.accessors) |acc| {
                switch (acc.acc) {
                    .Deref => try s.p(")"),
                    .Access => |field| try s.j(.{ ".", try genField(self, field, acc.tBefore) }),
                }
            }
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
        .Break => {
            var s = startLine(self);
            try s.p("break");
            try s.finishStmt();
        },
        .Switch => |sw| {
            var s = startLine(self);
            try s.varExpr(sw.refvar, sw.switchOn);
            const cond = sw.refvar;

            const ty = (try getTypeMapped(self, sw.switchOn.t));
            const structType = switch (ty) {
                .Con => |con| con.type.structureType(),
                .Anon => .RecordLike,
                else => unreachable,
            };
            switch (structType) {
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
                                const tyApp = ty.Con;
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
        .For => |forstmt| {
            var condStmt = startLine(self);
            const tv = condStmt.ctx.backend.temp();
            try condStmt.definition(forstmt.iterTy, tv);
            try condStmt.p("=");
            try condStmt.instFun(forstmt.intoIterFun);
            try condStmt.j(.{"("});
            try condStmt.genExpr(forstmt.iter);
            try condStmt.j(.{")"});
            try condStmt.finishStmt();

            var whileLoop = startLine(self);
            try whileLoop.p(.{ "while", "(true)" });
            try whileLoop.beginBody();
            {
                const condvar = ast.Var{ .name = "_cond", .uid = self.backend.temp().id };

                {
                    var checkLine = startLine(self);
                    try checkLine.definition(forstmt.condTy, condvar);
                    try checkLine.p("=");
                    try checkLine.instFun(forstmt.nextFun);
                    try checkLine.j(.{ "(", "&", tv, ")" });
                    try checkLine.finishStmt();

                    var checkln = startLine(self);
                    try checkln.p(.{ "if", "(", condvar, ".tag", "==", "0", ")" });
                    try checkln.beginBody();
                    {
                        var breakln = startLine(self);
                        try breakln.p("break");
                        try breakln.finishStmt();
                    }
                    try endBodyAndFinish(self);
                }

                {
                    // deconstruct from Maybe
                    {
                        var assln = startLine(self);
                        try assln.definition(forstmt.decon.d.t, switch (forstmt.decon.d.d) {
                            .Var => |v| v,
                            else => forstmt.decon.refvar,
                        });
                        try assln.p("=");
                        const rvdp = ast.Decon.PathM{ .Tip = .{
                            .t = forstmt.condTy,
                            .v = condvar,
                        } };
                        const condTy = try getTypeMapped(self, forstmt.condTy);
                        const accdp = ast.Decon.PathM{ .Concat = .{
                            .next = &rvdp,
                            .path = .{
                                .Con = .{
                                    .con = &condTy.Con.type.stuff.cons[1],
                                    .field = 0,
                                    .t = (try datatype(self, condTy.Con)).Application.id,
                                },
                            },
                        } };
                        try assln.p(.{&accdp});
                        try assln.finishStmt();
                    }

                    // exit if condition not matched
                    {
                        var deconln = startLine(self);
                        try deconln.p(.{ "if", "(!(" });
                        var hadCondition = false;

                        const dp = ast.Decon.PathM{ .Tip = .{
                            .t = forstmt.decon.d.t,
                            .v = forstmt.decon.refvar,
                        } };
                        try deconln.deconCondition_(&hadCondition, forstmt.decon.d, &dp);
                        if (hadCondition) {
                            try deconln.p("))");
                            try deconln.beginBody();
                            {
                                // case failed.
                                try genPanic(self, "pattern not matched to enter for loop");
                            }
                            try endBodyAndFinish(self);
                        }
                    }

                    try self.monoScope(forstmt.body);
                }
            }
            try endBodyAndFinish(self);
        },
    }
}

// FOR NOW DEACTIVATE. When we add list deconstructions, we'll have to do tha thing.
fn deconAssignments(self: *Self, decon: *const ast.Decon, refvar: ast.Var) !void {
    _ = refvar; // autofix
    _ = decon; // autofix
    _ = self; // autofix
    // const rootDP = DeconPath{ .Tip = .{ .temp = caseVar, .t = decon.t } };
    // try deconAssignments_(self, decon, &rootDP);
}

const DeconPath = union(enum) {
    Tip: struct { temp: Temp, t: ast.Type },
    Concat: struct {
        path: Type,
        next: *const DeconPath,
    },

    const Type = union(enum) {
        Con: struct { con: *const ast.Con, field: usize, nuId: Unique },
        Field: *const ast.Decon.Field,
        Ptr,
        // List: *const struct {},

        fn tyBegin(self: @This(), stmt: *Stmt) !void {
            switch (self) {
                .Ptr => try stmt.p("(*"),
                .Con => {},
                .Field => {},
            }
        }

        fn tyEnd(self: @This(), stmt: *Stmt) !void {
            switch (self) {
                .Ptr => try stmt.p(")"),
                .Field => unreachable,
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
            }
        }
    };
};

fn deconPathTyBegin(dt: anytype, self: ast.Decon.PathF(dt).Type, stmt: *Stmt) !void {
    switch (self) {
        .Ptr => try stmt.p("(*"),
        .Con => {},
        .Field => {},
        .None => {},
    }
}

fn deconPathTyEnd(dt: anytype, self: ast.Decon.PathF(dt).Type, stmt: *Stmt) !void {
    switch (self) {
        .Ptr => try stmt.p(")"),
        .None => {},
        .Field => |field| try stmt.j(.{ ".", try genField(stmt.ctx, field.rec, field.t) }),
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
                        if (dt == .Poly) b: {
                            const tyApp = (try getTypeMapped(stmt.ctx, con.t)).Con;
                            const nuId = (try datatype(stmt.ctx, tyApp)).Application.id;
                            break :b nuId;
                        } else con.t,
                        "_",
                        sanitize(con.con.name),
                        "_s",
                    });
                    try stmt.j(.{ ".field", con.field });
                },
                else => unreachable, // actually unreachable
            }
        },
    }
}

fn writeDeconPath(dt: anytype, self: *const ast.Decon.PathF(dt), stmt: *Stmt) anyerror!void {
    switch (self.*) {
        .Tip => |t| try stmt.j(.{t.v}),
        .Concat => |concat| {
            try deconPathTyBegin(dt, concat.path, stmt);
            try writeDeconPath(dt, concat.next, stmt);
            try deconPathTyEnd(dt, concat.path, stmt);
        },
    }
}

fn deconAssignments_(self: *Self, decon: *const ast.Decon, dp: *const DeconPath) !void {
    switch (decon.d) {
        .None => return,
        .Var => |v| {
            if (try addRefPath(self, v, dp) == null) {
                var l = startLine(self);
                try l.definition(decon.t, v);
                try l.p("=");
                try l.p(.{dp});
                try l.finishStmt();
            }
        },
        .Num => unreachable,
        .Con => |con| {
            if (con.con.data.isPointer()) {
                const ptrdp = DeconPath{ .Concat = .{ .path = .Ptr, .next = dp } };
                try deconAssignments_(self, con.decons[0], &ptrdp);
                return;
            }

            const tyApp = (try getTypeMapped(self, decon.t)).Con;
            const nuId = (try datatype(self, tyApp)).Application.id;
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
        .Record => |fields| {
            for (fields) |field| {
                const nextDP = DeconPath{
                    .Concat = .{
                        .path = .{
                            .Field = field,
                        },
                        .next = dp,
                    },
                };
                try deconAssignments_(self, field.decon, &nextDP);
            }
        },
        .List => unreachable,
    }
}

// BAD!
fn addRefPath(self: *Self, v: ast.Var, dp: *const DeconPath) !?TransientDecon {
    switch (dp.*) {
        .Tip => return null,
        else => {},
    }

    const gp = try self.backend.varPtrDecons.getOrPut(v);
    if (gp.found_existing) {
        return gp.value_ptr.*;
    }

    var dps = std.ArrayList(DeconPath.Type).init(self.backend.al);
    var cdp = dp;
    const t = b: {
        while (true) {
            switch (cdp.*) {
                .Tip => |tt| break :b tt,
                .Concat => |c| {
                    try dps.append(c.path);
                    cdp = c.next;
                },
            }
        }
    };

    std.debug.assert(dps.items.len > 0);
    std.mem.reverse(DeconPath.Type, dps.items);

    gp.value_ptr.* = .{ .dp = dps.items, .t = t.t, .temp = t.temp };
    return gp.value_ptr.*;
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
    // if the function has an effectively completed env, just ignore it.
    if (try isEnvEmpty(self, completedEnv.fun.env)) {
        return;
    }

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
            .Int => |x| {
                try stmt.instFun(x.ref);
                try stmt.p(.{ "(", x.int, ")" });
            },
            .Con => |c| {
                if (c.tys.len == 0) {
                    const t = (try getTypeMapped(stmt.ctx, expr.t)).Con;
                    _ = try stmt.constructor(c, t);
                } else {
                    const dataTy = (try getType(stmt.ctx, expr.t)).Fun.ret;
                    const app = (try getTypeMapped(stmt.ctx, dataTy)).Con;
                    _ = try stmt.constructor(c, app);
                }
            },
            .Var => |v| switch (v.v) {
                .Var => |vv| {
                    switch (v.locality) {
                        .Local => {
                            try stmt.localVarThatCouldPossiblyBeDeconed(vv);
                        },
                        .External => {
                            try stmt.j(.{ "env.", vv.v });
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
                        try stmt.p(.{efn.name.name});
                    }

                    if (ast.Annotation.find(efn.anns, "cstdinclude")) |ann| {
                        try stmt.ctx.backend.imports.insert(ann.params[0]);
                    }
                },
                .ClassFun => |cfun| {
                    try stmt.instFun(cfun.ref);
                },
                .TNum => |tnum| {
                    const num = stmt.ctx.typeContext.getNum(stmt.ctx.tymap.mapTNum(tnum).?).Literal;
                    try stmt.p(.{num});
                },
            },
            .Call => |call| {
                const funTy = (try getType(stmt.ctx, call.callee.t)).Fun;
                const envm = getEnv(stmt.ctx, funTy.env);

                if (try isEnvEmptyT(stmt.ctx, &envm)) {
                    try genExpr(stmt, call.callee);

                    try stmt.p("(");
                    if (call.args.len > 0) {
                        try genExpr(stmt, call.args[0]);

                        for (call.args[1..]) |arg| {
                            try stmt.j(", ");
                            try genExpr(stmt, arg);
                        }
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
                try std.zig.stringEscape(s, "", .{}, stmt.buf.writer()); // TODO: escape '?' in strings to avoid trigraphs
                try writer.writeByte('"');
            },
            .Intrinsic => |intr| {
                switch (intr.intr.ty) {
                    .@"u32-add", .@"u8-add", .@"i64-add", .@"i32-add", .@"size-add" => {
                        try stmt.genExpr(intr.args[0]);
                        try stmt.p("+");
                        try stmt.genExpr(intr.args[1]);
                    },
                    .@"u32-sub", .@"u8-sub", .@"i64-sub", .@"i32-sub", .@"size-sub" => {
                        try stmt.genExpr(intr.args[0]);
                        try stmt.p("-");
                        try stmt.genExpr(intr.args[1]);
                    },
                    .@"u32-mul", .@"u8-mul", .@"i64-mul", .@"i32-mul", .@"size-mul" => {
                        try stmt.genExpr(intr.args[0]);
                        try stmt.p("*");
                        try stmt.genExpr(intr.args[1]);
                    },
                    .@"u32-div", .@"u8-div", .@"i64-div", .@"i32-div", .@"size-div" => {
                        try stmt.genExpr(intr.args[0]);
                        try stmt.p("/");
                        try stmt.genExpr(intr.args[1]);
                    },
                    .@"u32-cmp", .@"u8-cmp", .@"i64-cmp", .@"i32-cmp", .@"size-cmp" => {
                        // CRINGE
                        const it = intr.intr.ty;
                        const cmp = switch (it) {
                            .@"i64-cmp" => &stmt.ctx.backend.aux.i64cmp,
                            .@"i32-cmp" => &stmt.ctx.backend.aux.i32cmp,
                            .@"u32-cmp" => &stmt.ctx.backend.aux.u32cmp,
                            .@"u8-cmp" => &stmt.ctx.backend.aux.u8cmp,
                            .@"size-cmp" => &stmt.ctx.backend.aux.sizecmp,
                            else => unreachable,
                        };
                        const tyname = ast.Annotation.find(stmt.ctx.prelude.defined(switch (it) {
                            .@"i64-cmp" => .I64,
                            .@"i32-cmp" => .I32,
                            .@"size-cmp" => .Size,
                            .@"u8-cmp" => .U8,
                            .@"u32-cmp" => .U32,
                            else => unreachable,
                        }).annotations, "ctype").?.params[0];

                        if (!cmp.*) {
                            defer cmp.* = true;

                            const oldCW = stmt.ctx.backend.cur;
                            stmt.ctx.backend.cur = CW.init(stmt.ctx.backend.al);
                            defer stmt.ctx.backend.cur = oldCW;

                            var e = startLine(stmt.ctx);
                            try e.p(.{ "static", tyname });
                            try e.j(.{ "builtin_", tyname, "cmp (", tyname, " l, ", tyname, " r)" });
                            try e.beginBody();
                            {
                                var retl = startLine(stmt.ctx);
                                try retl.j("return (l < r) ? 0 : (l == r) ? 1 : 2");
                                try retl.finishStmt();
                            }
                            try endBodyAndFinish(stmt.ctx);
                            try stmt.ctx.backend.parts.append(stmt.ctx.backend.cur);
                        }
                        try stmt.j(.{ "builtin_", tyname, "cmp(" });
                        try stmt.genExpr(intr.args[0]);
                        try stmt.p(",");
                        try stmt.genExpr(intr.args[1]);
                        try stmt.p(")");
                    },
                    .cast => {
                        try stmt.j(.{ "(", expr.t, ")" });
                        try stmt.genExpr(intr.args[0]);
                    },
                    .undefined => {
                        const t = temp(stmt.ctx.backend);
                        var udl = startLine(stmt.ctx);
                        try udl.definition(expr.t, t);
                        try udl.finishStmt();

                        try stmt.p(.{t});
                    },
                    .argv => {
                        try stmt.p("_global_argv");
                    },
                    .argc => {
                        try stmt.p("_global_argc");
                    },
                    .memeq => {
                        try stmt.genExpr(intr.args[0]);
                        try stmt.p("==");
                        try stmt.genExpr(intr.args[1]);
                    },
                    .errno => unreachable,

                    .@"register-signal" => {
                        const regfunname = "_intr_register_signal";
                        if (!stmt.ctx.backend.aux.sighandler) {
                            defer stmt.ctx.backend.aux.sighandler = true;

                            const funTy = intr.args[1].t;
                            const sigarrname = "_intr_sigarr";

                            // create array of signals + envs.
                            {
                                const self = stmt.ctx;
                                const oldCW = self.backend.cur;
                                self.backend.cur = CW.init(self.backend.al);
                                defer self.backend.cur = oldCW;

                                var sigarrln = startLine(self);
                                try sigarrln.definition(funTy, Join(.{ sigarrname, "[32]" }));
                                try sigarrln.finishStmt();

                                try self.backend.parts.append(self.backend.cur);
                            }

                            const sighandlerfnname = "_intr_sighandle";

                            // generate call function
                            {
                                const self = stmt.ctx;
                                const oldCW = self.backend.cur;
                                self.backend.cur = CW.init(self.backend.al);
                                defer self.backend.cur = oldCW;

                                const sigv = ast.Var{ .name = "_intr_sig", .uid = self.backend.temp().id };
                                const typarams = [_]Parameter{
                                    Parameter{ .v = .{ .NormalVar = sigv }, .t = intr.args[0].t },
                                };
                                var regfun = startLine(self);
                                try regfun.p(.{ "static", "void", sighandlerfnname, "(", SepBy(", ", &typarams), ")" });
                                try regfun.beginBody();
                                {

                                    // COPYPASTA (I'm not yet sure how to handle expr calls)
                                    const sigFunTy = (try getType(self, funTy)).Fun;
                                    const envm = getEnv(self, sigFunTy.env);

                                    if (try isEnvEmptyT(self, &envm)) {
                                        var sigarrsetln = startLine(self);
                                        try sigarrsetln.p(.{ sigarrname, "[", sigv, "]" });
                                        try sigarrsetln.p(.{ "(", sigv, ")" });
                                        try sigarrsetln.finishStmt();
                                    } else {
                                        var sigarrsetln = startLine(self);
                                        const t = self.backend.temp();
                                        try sigarrsetln.definition(funTy, t);
                                        try sigarrsetln.p(.{ "=", sigarrname, "[", sigv, "]" });
                                        try sigarrsetln.finishStmt();

                                        var callln = startLine(self);
                                        try callln.j(.{ t, ".fun" });

                                        try callln.p("(");
                                        try callln.j(.{ t, ".env" });

                                        try callln.p(.{ ",", sigv, ")" });
                                        try callln.finishStmt();
                                    }
                                }
                                try endBodyAndFinish(self);

                                try self.backend.parts.append(self.backend.cur);
                            }

                            // generate register function
                            {
                                const self = stmt.ctx;
                                const oldCW = self.backend.cur;
                                self.backend.cur = CW.init(self.backend.al);
                                defer self.backend.cur = oldCW;

                                const sigv = ast.Var{ .name = "_intr_sig", .uid = self.backend.temp().id };
                                const funv = ast.Var{ .name = "_intr_fun", .uid = self.backend.temp().id };
                                const typarams = [_]Parameter{
                                    Parameter{ .v = .{ .NormalVar = sigv }, .t = intr.args[0].t },
                                    Parameter{ .v = .{ .NormalVar = funv }, .t = intr.args[1].t },
                                };
                                var regfun = startLine(self);
                                try regfun.p(.{"static"});
                                try regfun.definition(expr.t, Tuple(.{ regfunname, "(", SepBy(", ", &typarams), ")" }));
                                try regfun.beginBody();
                                {
                                    var sigarrsetln = startLine(self);
                                    try sigarrsetln.p(.{ sigarrname, "[", sigv, "]", "=", funv });
                                    try sigarrsetln.finishStmt();

                                    try self.backend.imports.insert("signal.h");
                                    var sigcallln = startLine(self);
                                    try sigcallln.p(.{ "signal(", sigv, ",", sighandlerfnname, ")" });
                                    try sigcallln.finishStmt();
                                }
                                try endBodyAndFinish(self);

                                try self.backend.parts.append(self.backend.cur);
                            }
                        }

                        try stmt.p(.{ regfunname, "(" });
                        try stmt.genExpr(intr.args[0]);
                        try stmt.p(",");
                        try stmt.genExpr(intr.args[1]);
                        try stmt.p(")");
                    },

                    .@"offset-ptr" => {
                        try stmt.j(.{ "(", expr.t, ")" });
                        try stmt.p("(");
                        {
                            try stmt.p("(");
                            {
                                try stmt.j(.{"(void*)"});
                                try stmt.genExpr(intr.args[0]);
                            }
                            try stmt.p(")");
                            try stmt.p("+");
                            try stmt.genExpr(intr.args[1]);
                        }
                        try stmt.p(")");
                    },
                    .@"size-of" => {
                        try stmt.p("sizeof(");
                        try stmt.genExpr(intr.args[0]);
                        try stmt.p(")");
                    },
                    .@"i32-i64" => {
                        try stmt.j(.{ "(", expr.t, ")" });
                        try stmt.genExpr(intr.args[0]);
                    },
                    .@"u32-bit-and" => {
                        try stmt.genExpr(intr.args[0]);
                        try stmt.p("&");
                        try stmt.genExpr(intr.args[1]);
                    },
                    .@"u32-bit-or" => {
                        try stmt.genExpr(intr.args[0]);
                        try stmt.p("|");
                        try stmt.genExpr(intr.args[1]);
                    },
                    .@"u32-bit-neg" => {
                        try stmt.p("~");
                        try stmt.genExpr(intr.args[0]);
                    },
                    else => unreachable,
                }
            },
            .NamedRecord => |rec| {
                const tyApp = (try getTypeMapped(stmt.ctx, expr.t)).Con;
                std.debug.assert(tyApp.type.eq(rec.data));
                const tyName = try datatype(stmt.ctx, tyApp);

                try stmt.j(.{ "(", tyName, ")" });
                try stmt.p("{");
                for (rec.fields, 0..) |field, i| {
                    if (i != 0) {
                        try stmt.j(",");
                    }
                    try stmt.j(.{ ".", try genField(stmt.ctx, field.field, expr.t) });
                    try stmt.p("=");
                    try stmt.genExpr(field.value);
                }
                try stmt.p("}");
            },
            .UnOp => |unop| {
                switch (unop.op) {
                    .Access => |mem| {
                        try stmt.genExpr(unop.e);
                        try stmt.j(.{ ".", try genField(stmt.ctx, mem, unop.e.t) });
                    },
                    .As => {
                        try stmt.genExpr(unop.e);
                    },
                    .Deref => {
                        try stmt.p("*");
                        try stmt.genExpr(unop.e);
                    },
                    .Ref => {
                        if (isLValue(unop.e)) {
                            try stmt.j(.{"&"});
                            try stmt.genExpr(unop.e);
                        } else {
                            const reftemp = try stmt.tempExpr(unop.e);
                            try stmt.j(.{ "&", reftemp });
                        }
                    },
                    .Not => {
                        try stmt.j(.{"!"});
                        try stmt.genExpr(unop.e);
                    },
                    .Update => unreachable,
                    .Negate => |ifun| {
                        try stmt.instFun(ifun);
                        try stmt.p(.{"("});
                        try stmt.genExpr(unop.e);
                        try stmt.p(.{")"});
                    },
                    .ElementAccess => |ea| {
                        try stmt.instFun(ea.access);
                        try stmt.p(.{"("});
                        try stmt.genExpr(unop.e);
                        try stmt.p(.{","});
                        try stmt.genExpr(ea.index);
                        try stmt.p(.{")"});
                    },
                }
            },
            .BinOp => |binop| {
                switch (binop.op) {
                    .Plus, .Minus, .Times, .Divide, .Equals, .NotEquals => |inst| {
                        if (binop.op == .NotEquals) {
                            try stmt.p(.{"!"});
                        }
                        try stmt.instFun(inst);
                        try stmt.p(.{"("});
                        try stmt.genExpr(binop.l);
                        try stmt.p(.{","});
                        try stmt.genExpr(binop.r);
                        try stmt.p(.{")"});
                    },
                    .GreaterThan, .LessThan, .GreaterEqualThan, .LessEqualThan => |inst| {
                        try stmt.instFun(inst);
                        try stmt.p(.{"("});
                        try stmt.genExpr(binop.l);
                        try stmt.p(.{","});
                        try stmt.genExpr(binop.r);
                        try stmt.p(.{")"});

                        switch (binop.op) {
                            .GreaterThan => {
                                try stmt.p("==");
                                try stmt.p("2");
                            },
                            .LessThan => {
                                try stmt.p("==");
                                try stmt.p("0");
                            },
                            .GreaterEqualThan => {
                                try stmt.p("!=");
                                try stmt.p("0");
                            },
                            .LessEqualThan => {
                                try stmt.p("!=");
                                try stmt.p("2");
                            },
                            else => unreachable,
                        }
                    },

                    // TODO: incorrect, because temp values break the evaluation order.
                    .Or => {
                        try stmt.genExpr(binop.l);
                        try stmt.p("||");
                        try stmt.genExpr(binop.r);
                    },
                    .And => {
                        try stmt.genExpr(binop.l);
                        try stmt.p("&&");
                        try stmt.genExpr(binop.r);
                    },
                    else => unreachable,
                }
            },
            .Lam => |lam| {
                const nuId = (try genFunctionForReal(stmt.ctx, "lam", lam.params, lam.returnType(), lam.env, null, switch (lam.body) {
                    .Expr => |lamexpr| .{ .Expr = lamexpr },
                    .Body => |bod| .{ .Body = bod.stmts },
                })).id;

                // TEMP?
                if (try isEnvEmpty(stmt.ctx, lam.env)) {
                    try stmt.p(.{ast.Var{ .name = "lam", .uid = nuId }});
                } else {
                    // CRINGE
                    const args = try stmt.ctx.backend.al.alloc(ast.Type, lam.params.len);
                    for (lam.params, args) |param, *arg| {
                        arg.* = try stmt.ctx.typeContext.mapType(stmt.ctx.tymap.match, param.d.t);
                    }
                    try genEnvInst(stmt.ctx, args, try stmt.ctx.typeContext.mapType(stmt.ctx.tymap.match, lam.returnType()), nuId);

                    try stmt.p(.{"(struct"});
                    try stmt.j(.{
                        "funenv",
                        nuId,
                        "){ .fun = ",
                        ast.Var{ .name = "lam", .uid = nuId },
                        ", .env = ",
                        "envinst",
                        nuId,
                        " }",
                    });
                }
            },
            .AnonymousRecord => |rec| {
                switch (try getTypeMapped(stmt.ctx, expr.t)) {
                    .Con => |tyApp| {
                        const tyName = try datatype(stmt.ctx, tyApp);
                        try stmt.j(.{ "(", tyName, ")" });
                        try stmt.p("{");
                        for (rec, 0..) |field, i| {
                            if (i != 0) {
                                try stmt.j(",");
                            }
                            try stmt.j(.{ ".", try genField(stmt.ctx, field.field, expr.t) });
                            try stmt.p("=");
                            try stmt.genExpr(field.value);
                        }
                        try stmt.p("}");
                    },

                    .Anon => |fields| {
                        const nuId = try anonRecord(stmt.ctx, fields);
                        try stmt.j(.{ "(", "struct anon_", nuId, ")" });
                        try stmt.p("{");
                        for (rec, 0..) |field, i| {
                            if (i != 0) {
                                try stmt.j(",");
                            }
                            try stmt.j(.{ ".", "f_", sanitize(field.field) });
                            try stmt.p("=");
                            try stmt.genExpr(field.value);
                        }
                        try stmt.p("}");
                    },

                    else => unreachable,
                }
            },
            .StaticArray => |arr| {
                const tyApp = (try getTypeMapped(stmt.ctx, expr.t)).Con;
                const tyname = (try datatype(stmt.ctx, tyApp));
                try stmt.j(.{ "(", tyname, ")" });
                try stmt.p(.{ "{", "._arr", "=", "{" });
                for (arr, 0..) |ae, i| {
                    if (i != 0) {
                        try stmt.p(",");
                    }

                    try stmt.genExpr(ae);
                }
                try stmt.p(.{ "}", "}" });
            },
            .IfElse => |ifelse| {
                try stmt.genExpr(ifelse.cond);
                try stmt.p("?");
                try stmt.genExpr(ifelse.ifTrue);
                try stmt.p(":");
                for (ifelse.ifOthers) |elif| {
                    try stmt.genExpr(elif.cond);
                    try stmt.p("?");
                    try stmt.genExpr(elif.then);
                    try stmt.p(":");
                }
                try stmt.genExpr(ifelse.ifFalse);
            },
            .CaseExpr => |caseexpr| {
                // GCC statement expression: ( { T refvar = switchOn; ternary_chain; } )
                // The outer ( ) are added by genExpr, so we emit { ... } inline.
                try stmt.p("{");
                try stmt.definition(caseexpr.switchOn.t, caseexpr.refvar);
                try stmt.p("=");
                try stmt.genExpr(caseexpr.switchOn);
                try stmt.p(";");

                for (caseexpr.cases) |case| {
                    switch (case) {
                        .Expr => |ec| {
                            switch (ec.decon.d) {
                                .None, .Var => {
                                    // Wildcard — final expression, no condition needed
                                    try stmt.genExpr(ec.expr);
                                    break; // stop generating, rest are unreachable.
                                },
                                else => {
                                    _ = try stmt.deconCondition(ec.decon, caseexpr.refvar);
                                    try stmt.p("?");
                                    try stmt.genExpr(ec.expr);
                                    try stmt.p(":");
                                },
                            }
                        },
                        .Case => unreachable,
                    }
                }

                try stmt.p(";");
                try stmt.p("}");
            },
            else => unreachable,
        }
        try stmt.p(")");
    }

    fn transientVar(stmt: *@This(), v: ast.Var) !void {
        _ = stmt;
        _ = v;
        unreachable;
    }

    fn instFun(stmt: *@This(), inst: ast.InstFunInst) !void {
        const aref = inst.*.?;
        switch (aref) {
            .InstFun => |ifun| {
                try stmt.function(ifun.fun, ifun.m, ifun.locality);
            },
            .Id => |id| {
                const ip = stmt.ctx.tymap.tryGetFunctionByID(id).?;
                try stmt.function(ip.fun, ip.m, .External); // i guess if  it's an Id (which means it was generalized), it should always be external
            },
        }
    }

    fn function(stmt: *@This(), fun: *ast.Function, m: *const ast.Match, locality: ast.Locality) !void {
        const tymap = TypeMap{
            .scheme = &m.scheme,
            .match = try stmt.ctx.typeContext.mapMatch(stmt.ctx.tymap, m),
            .prev = stmt.ctx.tymap,
        };
        const oldTymap = stmt.ctx.tymap;
        stmt.ctx.tymap = &tymap;
        defer stmt.ctx.tymap = oldTymap;

        const funNuId = try genFunctionForRealForReal(stmt.ctx, fun);
        const nuId = funNuId.id;

        if (try isEnvEmpty(stmt.ctx, fun.env)) {
            try stmt.p(.{ast.Var{ .name = fun.name.name, .uid = nuId }});
        } else {
            // CRINGE

            const args = try stmt.ctx.backend.al.alloc(ast.Type, fun.params.len);
            for (fun.params, args) |param, *arg| {
                arg.* = try stmt.ctx.typeContext.mapType(stmt.ctx.tymap.match, param.d.t);
            }
            try genEnvInst(stmt.ctx, args, try stmt.ctx.typeContext.mapType(stmt.ctx.tymap.match, fun.ret), nuId);

            try stmt.p(.{"(struct"});
            try stmt.j(.{
                "funenv",
                nuId,
                "){ .fun = ",
                ast.Var{ .name = fun.name.name, .uid = nuId },
                ", .env = ",
            });
            if (funNuId.type == .Recursive) {
                try stmt.j(.{"env"});
            } else {
                try stmt.j(.{
                    if (locality == .Local) "" else "env.",
                    "envinst",
                    nuId,
                });
            }
            try stmt.j(.{
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

    fn varExpr(stmt: *@This(), v: ast.Var, expr: *ast.Expr) anyerror!void {
        var s = startLine(stmt.ctx);
        try s.definition(expr.t, v);
        try s.p("=");
        try s.genExpr(expr);
        try s.finishStmt();
    }

    fn definition(stmt: *@This(), t: ast.Type, v: anytype) GenError!void {
        try stmt.definitionBegin(t);
        try stmt.p(.{v});
        try stmt.definitionEnd(t);
    }

    fn definitionBegin(stmt: *@This(), t: ast.Type) GenError!void {
        switch (try getTypeMapped(stmt.ctx, t)) {
            .Con => |con| {
                try stmt.p(.{try datatype(stmt.ctx, con)});
            },
            .Fun => |fun| {
                const envm = getEnv(stmt.ctx, fun.env);
                const env = envm.env;
                const mNuId = try genEnvStruct(stmt.ctx, .{ .fun = envm.fun, .env = env }, envm.match);
                if (mNuId) |nuId| {
                    try genEnvInst(stmt.ctx, fun.args, fun.ret, nuId);
                    try stmt.p(.{"struct"});
                    try stmt.j(.{ "funenv", nuId });
                } else {
                    try stmt.definitionBegin(fun.ret);
                    try stmt.p(.{"(*"});
                }
            },
            .Anon => |anon| {
                const nuId = try anonRecord(stmt.ctx, anon);
                try stmt.p(.{"struct"});
                try stmt.j(.{ "anon_", nuId });
            },
            .TVar => unreachable,
            .TyVar => |tyv| {
                if (stmt.ctx.typeContext.getFieldsForTVar(tyv) != null) unreachable;

                try stmt.p(.{try datatype(stmt.ctx, .{
                    .type = stmt.ctx.typeContext.prelude.?.defined(.Unit),
                    .application = &ast.Match.Empty,
                    .outerApplication = &.{},
                })});
            },
        }
    }

    fn definitionEnd(stmt: *@This(), t: ast.Type) GenError!void {
        switch (try getType(stmt.ctx, t)) {
            .Fun => |fun| {
                const envm = getEnv(stmt.ctx, fun.env);
                if (try isEnvEmptyT(stmt.ctx, &envm)) {
                    try stmt.p(.{ ")(", SepBy(", ", fun.args), ")" });
                }
                try stmt.definitionEnd(fun.ret);
            },
            else => {},
        }
    }

    fn deconCondition(self: *@This(), decon: *const ast.Decon, refvar: ast.Var) !bool {
        var hadCondition = false;
        const dp = ast.Decon.PathM{ .Tip = .{ .t = decon.t, .v = refvar } };
        try self.deconCondition_(&hadCondition, decon, &dp);
        if (!hadCondition) {
            try self.ctx.backend.imports.insert("stdbool.h");
            try self.p("true");
        }
        return hadCondition;
    }

    fn deconCondition_(self: *@This(), hadCondition: *bool, decon: *const ast.Decon, dp: *const ast.Decon.PathM) !void {
        switch (decon.d) {
            .None => return,
            .Var => return,
            .Num => |num| {
                try self.deconConnect(hadCondition);
                try self.p(.{ dp, "==", num });
            },
            .Con => |con| {
                if (con.con.data.isPointer()) {
                    const ptrdp = ast.Decon.PathM{ .Concat = .{ .path = .Ptr, .next = dp } };
                    try deconCondition_(self, hadCondition, con.decons[0], &ptrdp);
                    return;
                }

                const tyApp = (try getTypeMapped(self.ctx, decon.t)).Con;
                const nuId = (try datatype(self.ctx, tyApp)).Application.id;
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
                            const nextDP = ast.Decon.PathM{
                                .Concat = .{
                                    .path = .{
                                        .Con = .{
                                            .con = con.con,
                                            .field = i,
                                            .t = nuId,
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
                            const nextDP = ast.Decon.PathM{
                                .Concat = .{
                                    .path = .{
                                        .Con = .{
                                            .con = con.con,
                                            .field = i,
                                            .t = nuId,
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
            .Record => |fields| {
                for (fields) |field| {
                    const nextDP = ast.Decon.PathM{
                        .Concat = .{
                            .path = .{
                                .Field = .{ .rec = field.field, .t = decon.t },
                            },
                            .next = dp,
                        },
                    };
                    try self.deconCondition_(hadCondition, field.decon, &nextDP);
                }
            },
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

    // TODO: I need to rethink how deconstructions are represented - this is sheeet.
    fn localVarThatCouldPossiblyBeDeconed(self: *@This(), v: ast.DeconVar) !void {
        if (v.dc) |dpv| {
            try writeDeconPath(.Poly, dpv.dp, self);
        } else {
            try self.p(.{v.v});
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
        else if (argTy == ast.NumRef) {
            const numref = @as(ast.NumRef, arg);
            const num = self.ctx.typeContext.getNum(numref).Literal;
            try self.j(.{num});
        } //
        else if (argTy == TypeName) {
            const tyname = (@as(TypeName, arg));
            switch (tyname) {
                .Defined => |def| try self.j(def),
                .Ptr => |pointedTo| {
                    try self.j(.{ pointedTo, "*" });
                },
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
                .prev = self.ctx.tymap,
                .scheme = &inst.m.scheme,
                .match = inst.m,
            };

            const oldTyMap = self.ctx.tymap;
            self.ctx.tymap = &tymap;
            defer self.ctx.tymap = oldTyMap;

            switch (inst.v) {
                .Var => |v| try self.definition(inst.t, v.v),
                .Fun => |fun| {
                    const mnuId = try genEnvStruct(self.ctx, .{ .env = fun.env, .fun = fun }, inst.m);
                    if (mnuId) |nuId| {
                        const tfun = self.ctx.typeContext.getType(inst.t);
                        try genEnvInst(self.ctx, tfun.Fun.args, tfun.Fun.ret, nuId);
                        try self.p(.{"struct"});
                        try self.j(.{ "env", nuId });
                        try self.j(.{ "envinst", nuId });
                    }
                },
                .ClassFun => |cfun| {
                    switch (cfun.ref.*.?) {
                        .Id => |id| {
                            const funm = self.ctx.tymap.tryGetFunctionByID(id).?;
                            const fun = funm.fun;
                            const mnuId = try genEnvStruct(self.ctx, .{ .env = fun.env, .fun = fun }, funm.m);
                            if (mnuId) |nuId| {
                                const tfun = self.ctx.typeContext.getType(inst.t);
                                try genEnvInst(self.ctx, tfun.Fun.args, tfun.Fun.ret, nuId);
                                try self.p(.{"struct"});
                                try self.j(.{ "env", nuId });
                                try self.j(.{ "envinst", nuId });
                            }
                        },
                        .InstFun => unreachable,
                    }
                },
                else => unreachable,
            }

            // try self.definition(
            //     inst.t,
            // );
        } //
        else if (argTy == *const ast.Decon.PathM) {
            const path = (@as(*const ast.Decon.PathM, arg));
            try writeDeconPath(.Mono, path, self);
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

const FieldName = union(enum) {
    Custom: Str,
    Normal: Str,

    pub fn write(s: @This(), stmt: *Stmt) anyerror!void {
        try switch (s) {
            .Normal => |f| stmt.j(.{ "f_", sanitize(f) }),
            .Custom => |f| stmt.j(f),
        };
    }
};
fn genField(self: *Self, mem: Str, t: ast.Type) !FieldName {
    const ty = try getTypeMapped(self, t);
    switch (ty) {
        .Con => |con| {
            switch (con.type.stuff) {
                .recs => |rs| {
                    for (rs) |r| {
                        if (common.streq(mem, r.rec.field)) {
                            if (ast.Annotation.find(r.anns, "cfield")) |ann| {
                                return .{ .Custom = ann.params[0] };
                            }
                        }
                    } else {
                        return .{ .Normal = mem };
                    }
                },
                .cons => return .{ .Normal = mem },
            }
        },
        else => return .{ .Normal = mem },
    }
}

fn isLValue(expr: *const ast.Expr) bool {
    var curexpr = expr;
    var depth: i32 = 0;
    while (true) {
        switch (curexpr.e) {
            .Var => break,
            .UnOp => |unop| {
                switch (unop.op) {
                    .Ref => depth += 1,
                    .Deref => depth -= 1,
                    .Access => {},
                    else => return false,
                }
                curexpr = unop.e;
            },
            else => return false,
        }
    }
    return depth <= 0;
}

fn getType(self: *const Self, ogt: ast.Type) !ast.TypeF(ast.Type) {
    return switch (self.typeContext.getType(ogt)) {
        .TVar => |tv| getType(self, self.tymap.mapTVar(tv) orelse unreachable),
        else => |t| t,
    };
}

// TODO: I should just have a comparison function that takes a TyMap OR have a mapMatch/mapType for a TyMap.
fn getTypeMapped(self: *const Self, ogt: ast.Type) !ast.TypeF(ast.Type) {
    // NOTE: changed self.match -> self.backend.tymap.match
    // that's because stuff is weird now.
    const ty = try self.typeContext.mapType(self.tymap, ogt);
    return self.typeContext.getType(ty);
}

fn getEnv(self: *const Self, ogenv: ast.EnvRef) TypeContext.Env {
    const base = self.typeContext.getEnv(ogenv).base;
    return self.typeContext.getEnv(self.tymap.mapEnv(base) orelse base).env.*.?;
}

fn isEnvEmptyT(self: *Self, env: *const TypeContext.Env) !bool {
    const oldTymap = self.tymap;
    defer self.tymap = oldTymap;
    self.tymap = &TypeMap.init(env.match, self.tymap);
    return try isEnvEmpty(self, env.env);
}

fn isEnvEmpty(self: *Self, env: *ast.Env) !bool {
    if (env.monoFinished) {
        // self.ctx.print(.{ env, "\n" });
        var it = env.monoInsts.iterator();
        while (it.next()) |inst| {
            switch (inst.v) {
                .Var => return false,
                .Fun => |fun| {
                    const oldTymap = self.tymap;
                    defer self.tymap = oldTymap;
                    self.tymap = &TypeMap.init(inst.m, self.tymap); // TODO: map match?

                    if (!try isEnvEmpty(self, fun.env)) return false;
                },
                .ClassFun => |cfun| {
                    switch (cfun.ref.*.?) {
                        .Id => |id| {
                            if (self.tymap.tryGetFunctionByID(id)) |ip| {
                                const oldTymap = self.tymap;
                                defer self.tymap = oldTymap;
                                self.tymap = &TypeMap.init(ip.m, self.tymap); // TODO: map match?

                                if (!try isEnvEmpty(self, ip.fun.env)) return false;
                            } else {
                                unreachable;
                            }
                        },
                        .InstFun => unreachable,
                    }
                },
                .TNum => unreachable,
            }
        }

        return true;
    } else {
        // finish the env thing.
        for (env.insts.items) |inst| {
            try env.monoInsts.insert(inst);
        }

        env.monoFinished = true;

        return try isEnvEmpty(self, env);
    }
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
        } else return;

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

// fn anonRecord(self: *Self, anons: []ast.Expr.Field) !Unique {
//     {
//         const nuId = self.backend.temp();

//         const oldCW = self.backend.cur;
//         self.backend.cur = CW.init(self.backend.al);
//         defer self.backend.cur = oldCW;

//         var e = startLine(self);
//         try e.p(.{"struct"});
//         try e.j(.{ "anon", "_", nuId });
//         try e.beginBody();

//         for (anons) |field| {
//             var l = startLine(self);
//             try l.j(.{ sanitize(field.name), "_", nuId, "_", sanitize(field.name), "," });
//             try l.finish();
//         }

//         try endBodyAndFinishStmt(self);
//         try self.backend.parts.append(self.backend.cur);
//     }
//     unreachable;
// }
const Field = ast.TypeF(ast.Type).Field;
fn anonRecord(self: *Self, fields: []Field) !Unique {
    std.mem.sort(Field, fields, .{}, struct {
        fn ltf(ctx: @TypeOf(.{}), l: Field, r: Field) bool {
            _ = ctx;
            return std.mem.order(u8, l.field, r.field) == .lt;
        }
    }.ltf);

    const nuId: Unique = b: {
        const gp = try self.backend.anonsGenerated.getOrPut(fields);
        if (gp.found_existing) return gp.value_ptr.*;
        const nuId = self.backend.temp();
        gp.value_ptr.* = nuId.id;
        break :b nuId.id;
    };

    {
        const oldCW = self.backend.cur;
        self.backend.cur = CW.init(self.backend.al);
        defer self.backend.cur = oldCW;

        var e = startLine(self);
        try e.p(.{"struct"});
        try e.j(.{ "anon", "_", nuId });
        try e.beginBody();

        for (fields) |field| {
            var i = startLine(self);
            try i.definition(field.t, Join(.{ "f_", sanitize(field.field) }));
            try i.finishStmt();
        }

        try endBodyAndFinishStmt(self);
        try self.backend.parts.append(self.backend.cur);
    }

    return nuId;
}

const TypeName = union(enum) {
    Defined: Str,
    Ptr: ast.Type,
    Application: struct { data: *const ast.Data, id: Unique },
};
fn datatype(self: *Self, tyApp: ast.TypeApplication) !TypeName {
    const data = tyApp.type;

    if (ast.Annotation.find(data.annotations, "cstdinclude")) |ann| {
        for (ann.params) |param| {
            try self.backend.imports.insert(param);
        }
    }

    if (ast.Annotation.find(data.annotations, "ctype")) |ann| {
        return .{ .Defined = ann.params[0] };
    }

    if (self.prelude.fromData(data)) |pt| {
        switch (pt) {
            .Unit => {},
            .Bool => {},
            .ConstStr => {},
            .I8 => {},
            .I32 => {},
            .I64 => {},
            .U8 => {},
            .U32 => {},
            .F64 => {},
            .Ordering => {},
            .Maybe => {},
            .StrConcat => {},
            .Tuple2 => {},
            .Tuple3 => {},
            .Tuple4 => {},

            .Ptr => return .{ .Ptr = tyApp.application.tvars[0].Type },
            .Array => {
                const nuId = b: {
                    const gpr = try self.backend.typesGenerated.getOrPut(tyApp);
                    if (gpr.found_existing) {
                        return .{ .Application = .{ .data = data, .id = gpr.value_ptr.* } };
                    }
                    const nuId = self.backend.temp().id;
                    gpr.value_ptr.* = nuId;
                    break :b nuId;
                };

                {
                    const oldCW = self.backend.cur;
                    self.backend.cur = CW.init(self.backend.al);
                    defer self.backend.cur = oldCW;

                    var s = startLine(self);
                    try s.p(.{"struct"});
                    try s.j(.{ data.name, "_", nuId });
                    try s.beginBody();
                    {
                        var l = startLine(self);
                        try l.definition(tyApp.application.tvars[1].Type, "_arr");
                        try l.j(.{ "[", tyApp.application.tvars[0].Num, "]" });
                        try l.finishStmt();
                    }

                    try endBodyAndFinishStmt(self);

                    try self.backend.parts.append(self.backend.cur);
                }

                return .{ .Application = .{ .data = data, .id = nuId } };
            },
            else => unreachable,
        }
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
                    const nuId = self.backend.temp().id;
                    gpr.value_ptr.* = nuId;

                    const oldTyMap = self.tymap;
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
                    self.tymap = &tymap;
                    defer self.tymap = oldTyMap;

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

                    return .{ .Application = .{ .data = data, .id = nuId } };
                },
                .RecordLike => {
                    const con = &cons[0];

                    // TEMP: COPYPASTA
                    const gpr = try self.backend.typesGenerated.getOrPut(tyApp);
                    if (gpr.found_existing) {
                        return .{ .Application = .{ .data = data, .id = gpr.value_ptr.* } };
                    }

                    const nuId = self.backend.temp().id;
                    gpr.value_ptr.* = nuId;

                    const oldTyMap = self.tymap;
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
                    self.tymap = &tymap;
                    defer self.tymap = oldTyMap;

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

                    return .{ .Application = .{ .data = data, .id = nuId } };
                },
                .ADT => {
                    // TEMP: COPYPASTA
                    const gpr = try self.backend.typesGenerated.getOrPut(tyApp);
                    if (gpr.found_existing) {
                        return .{ .Application = .{ .data = data, .id = gpr.value_ptr.* } };
                    }
                    const nuId = self.backend.temp().id;
                    gpr.value_ptr.* = nuId;

                    const oldTyMap = self.tymap;
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
                    self.tymap = &tymap;
                    defer self.tymap = oldTyMap;

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

                    return .{ .Application = .{ .data = data, .id = nuId } };
                },
            }
        },
        .recs => |fields| {
            const gpr = try self.backend.typesGenerated.getOrPut(.{ .type = data, .application = tyApp.application, .outerApplication = tyApp.outerApplication });
            if (gpr.found_existing) {
                return .{ .Application = .{ .data = data, .id = gpr.value_ptr.* } };
            }

            const nuId = self.backend.temp().id;
            gpr.value_ptr.* = nuId;

            const oldTyMap = self.tymap;
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
            self.tymap = &tymap;
            defer self.tymap = oldTyMap;

            // generate dat data definition.
            const oldCW = self.backend.cur;
            self.backend.cur = CW.init(self.backend.al);
            defer self.backend.cur = oldCW;

            var e = startLine(self);
            try e.p(.{"struct"});
            try e.j(.{ sanitize(data.name), "_", nuId });
            try e.beginBody();

            for (fields) |afield| {
                const field = afield.rec;
                var i = startLine(self);
                try i.definition(field.t, Join(.{ "f_", sanitize(field.field) }));
                try i.finishStmt();
            }

            try endBodyAndFinishStmt(self);
            try self.backend.parts.append(self.backend.cur);

            return .{ .Application = .{ .data = data, .id = nuId } };
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
        try e.j(.{ sanitize(data.name), "_", nuId });
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

fn genPanic(self: *Self, s: Str) !void {
    try self.backend.imports.insert("stdlib.h");
    try self.backend.imports.insert("stdio.h");

    var l = startLine(self);
    try l.j(.{ "printf", "(", "\"%s\\n\", \"", s, "\")" });
    try l.finishStmt();

    var e = startLine(self);
    try e.p(.{ "exit", "(", 1, ")" });
    try e.finishStmt();
}

const FunInst = ast.TypeF(ast.Type).Fun;

// NOTE: This generates a lot of pointless functions, because lambdas for example may not use environment crap, yet it's here. (see: allocate-str - each different string will regenerate lambda that's inside.)
// after I sorta finish the C backend thing, I should take care of this
// (IMO i should gather all tvars/envs/assocs which the environment is using and monomorphise based on that. It might be slow BUT we'll decrease the amount of C code we're writing!)
pub const EnvApp = struct {
    env: *ast.Env,
    ml: ?*const MatchLink,

    pub fn fromEnv(self: *Self, envfun: ast.EnvFun, m: *const ast.Match) !@This() {
        const env = envfun.env;
        var ml: ?*const MatchLink = null;
        const al = self.backend.al;
        const tymap = TypeMap.init(m, self.tymap);

        var mfe: ?ast.EnvFun = envfun;
        while (mfe) |fe| {
            defer mfe = fe.env.outer;

            if (fe.fun) |fun| {
                const tvars = try al.alloc(ast.TypeOrNum, fun.scheme.tvars.len);
                for (fun.scheme.tvars, tvars) |st, *t| {
                    t.* = switch (st) {
                        .TVar => |tv| ast.TypeOrNum{ .Type = tymap.mapTVar(tv).? },
                        .TNum => |tn| ast.TypeOrNum{ .Num = tymap.mapTNum(tn).? },
                    };
                }

                const envs = try al.alloc(ast.EnvRef, fun.scheme.envVars.len);
                for (fun.scheme.envVars, envs) |se, *e| {
                    const base = self.typeContext.getEnv(se).base;
                    e.* = tymap.mapEnv(base).?;
                }

                const assocs = try al.alloc(?ast.Match.AssocRef, fun.scheme.associations.len);
                for (fun.scheme.associations, assocs) |sa, *a| {
                    if (sa.concrete) |_| {
                        a.* = .{ .InstFun = tymap.tryGetFunctionByID(sa.uid).? };
                    } else {
                        a.* = null;
                    }
                }

                const match = try common.allocOne(al, ast.Match{
                    .tvars = tvars,
                    .envVars = envs,
                    .assocs = assocs,
                    .scheme = fun.scheme,
                });
                ml = try MatchLink.link(al, match, ml);
            }
        }

        return .{
            .ml = ml,
            .env = env,
        };
    }

    pub const Comparator = struct {
        typeContext: *const TypeContext,

        pub fn eql(ctx: @This(), a: EnvApp, b: EnvApp) bool {
            if (a.env.id != b.env.id) return false;

            var mlml = a.ml;
            var mrml = b.ml;
            while (mlml) |lml| {
                const rml = mrml.?;
                defer {
                    mlml = lml.next;
                    mrml = rml.next;
                }

                if (!ast.Match.Comparator.eql(.{ .typeContext = ctx.typeContext }, lml.match, rml.match)) return false;
            }

            std.debug.assert(mrml == null);
            return true;
        }

        pub fn hash(ctx: @This(), k: EnvApp) u64 {
            // return k.env.id * 497192 + ast.Match.Comparator.hash(.{ .typeContext = ctx.typeContext }, k.m);
            _ = ctx;
            _ = k;
            return 0;
        }
    };
};

// When deconstructing, we must actually operate on a reference when it comes to pointers and stuff.
// eg: Ptr(self)
// or: Ptr({ field: x })
// NOTE: technically, all DeconTypes should start with Ptr.
const TransientDecons = std.HashMap(ast.Var, TransientDecon, ast.Var.comparator(), std.hash_map.default_max_load_percentage);
const TransientDecon = struct {
    temp: Temp,
    dp: []DeconPath.Type,
    t: ast.Type,
};
// const DeconType = union(enum) {
//     Field: Str,
//     Ptr,
// };

/////
