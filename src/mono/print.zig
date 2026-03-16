const ast = @import("../ast.zig");
const mono = @import("../mono.zig");
const GenError = mono.GenError;

const Self = mono.Mono(@This());
pub const Mono = Self;
pub fn genFunction(self: *Self, fun: *ast.Function, m: *const ast.Match) GenError!void { // TODO: params
    // const funId = self.backend.gen.newUnique();
    self.ctx.print(.{ fun.name, " ", m });

    const oldM = self.ctx.mapTypes;
    self.ctx.mapTypes = m;
    defer self.ctx.mapTypes = oldM;

    const c = self.ctx;
    c.print(.{" ("});
    c.sepBy(fun.params, ", ");
    c.s(")");
    fun.env.print(c);
    // c.encloseSepBy(fun.temp__calls.items, ", ", "[", "]");
    c.s(" -> ");
    fun.ret.print(c);
    c.s(" ## ");
    fun.scheme.print(c);
    c.s("\n");

    { // this is the same copypasta for every function, need to rethink the api...
        self.ctx.indent += 1;
        defer self.ctx.indent -= 1;

        const oldUseScope = self.useScope;
        defer self.useScope = oldUseScope;
        var curUseScope = self.newUseScope();
        self.useScope = &curUseScope;

        try self.monoScope(fun.body);
    }
    self.ctx.print(.{ "ENV: [", ast.Ctx.iter(fun.env.monoInsts.iterator(), ", "), "] ", m, "\n" });
}

// this is the "public api" part
pub fn genStmt(self: *Self, stmt: *ast.Stmt) !void {
    const b = &self.backend;
    _ = b;
    switch (stmt.*) {
        // .VarDec => |vd| {
        //     // vd.varDef
        //     _ = vd;
        // },
        // .Expr => |expr| {
        //     try self.genExpr(expr);
        //     self.ctx.print("\n");
        // },

        // gets eliminated beforehand.
        .Function => unreachable,
        .Instance => unreachable,

        else => {
            self.ctx.print(stmt.*);
        }, //TEMP
    }
}

fn genExpr(self: *Self, expr: *ast.Expr) !void {
    _ = self;
    switch (expr.e) {
        else => unreachable,
    }
}

// currently generating env in genFunction
// fn genEnv(self: *Self, env: *const ast.Env.Mono, m: *const ast.Match, exclusions: ?struct {}) !void { // TODO: exclusions
//     _ = exclusions;
//     self.ctx.print(.{ "ENV: [", ast.Ctx.iter(env.iterator(), ", "), "] ", m, "\n" });
// }

pub fn genEnvCompletion(self: *Self, incompleteEnv: ast.Function.FunApp, completedEnv: ast.Function.FunApp) !void {
    self.ctx.print(.{ "||", incompleteEnv.fun.name, " ", incompleteEnv.m, "\n^^", completedEnv.fun.name, " ", completedEnv.m, "\n" });
}
