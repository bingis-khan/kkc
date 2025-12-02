const std = @import("std");
const Module = @import("Module.zig");
const common = @import("common.zig");
const ast = @import("ast.zig");
const Str = common.Str;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig");
const Errors = @import("error.zig").Errors;
const Prelude = @import("Prelude.zig");
const TypeContext = @import("TypeContext.zig");

const Self = @This();
pub const ModuleLookup = std.HashMap(Module.Path, ?Module, struct {
    pub fn eql(ctx: @This(), a: Module.Path, b: Module.Path) bool {
        _ = ctx;

        if (a.len != b.len) return false;
        for (a, b) |sa, sb| {
            if (!common.streq(sa, sb)) return false;
        }

        return true;
    }

    pub fn hash(ctx: @This(), k: Module.Path) u64 {
        _ = ctx;

        var cumhash: u64 = 1;
        for (k) |kk| {
            cumhash *%= std.hash_map.StringContext.hash(undefined, kk);
        }

        return cumhash;
    }
}, std.hash_map.default_max_load_percentage);

// NOTE: null means that the Module is still being parsed.
modules: ModuleLookup,
errors: *Errors,
typeContext: *TypeContext,
al: std.mem.Allocator,
defaultExports: std.ArrayList(Module.Exports), // TODO: instead of re-adding stuff for every module, save the state and COPY.
prelude: ?Prelude,
full: std.ArrayList(ast),

pub fn init(al: std.mem.Allocator, errors: *Errors, typeContext: *TypeContext) Self {
    return .{
        .modules = ModuleLookup.init(al),
        .errors = errors,
        .al = al,
        .typeContext = typeContext,
        .defaultExports = std.ArrayList(Module.Exports).init(al),
        .prelude = null,
        .full = std.ArrayList(ast).init(al),
    };
}

pub fn loadPrelude(self: *Self, path: Str) !Prelude {
    const preludeModule = try self.loadDefault(path);
    const prelude = try preludeModule.mkPrelude();
    self.prelude = prelude;
    return prelude;
}

pub fn loadDefault(self: *Self, path: Str) !Module {
    var modpath: Module.Path = undefined;
    modpath.len = 1;
    modpath.ptr = @constCast(@ptrCast(&path));
    const module = try self.loadModule(&.{}, modpath);
    try self.defaultExports.append(module.?.exports);
    return module.?;
}

// NOTE: this function reports errors with modules that were not found.
pub fn loadModule(self: *Self, base: Module.Path, path: Module.Path) !?Module {
    const fullPath = b: {
        if (base.len == 0) break :b path;
        if (path.len == 0) break :b base;

        const pp = try self.al.alloc(Str, base.len + path.len);
        std.mem.copyForwards(Str, pp[0..base.len], base);
        std.mem.copyForwards(Str, pp[base.len .. base.len + path.len], path);
        break :b pp;
    };

    if (self.modules.get(fullPath)) |module| {
        if (module == null) {
            try self.errors.append(.{ .CircularModuleReference = .{} });
            return null;
        }

        return module.?;
    }

    try self.modules.put(fullPath, null);

    const filepath = try self.modulePathToFilepath(base, path);
    const source = std.fs.cwd().readFileAlloc(self.al, filepath, 1337420) catch return error.TempError; // TODO: proper errors.

    const lexer = Lexer.init(source);
    var l = lexer;
    while (!l.finished()) {
        const tok = l.nextToken();
        std.debug.print("{}\n", .{tok});
    }

    var parser = try Parser.init(lexer, self.prelude, base, self, self.errors, self.typeContext, self.al);
    for (self.defaultExports.items) |xports| {
        try parser.addExports(&xports);
    }
    const module = try parser.parse();
    try self.full.append(module.ast);

    var hadNewline: bool = undefined;
    const ctx = ast.Ctx.init(&hadNewline, self.typeContext);
    module.ast.print(ctx);

    return module;
}

pub fn getAST(self: *const Self) []ast {
    return self.full.items;
}

fn modulePathToFilepath(self: *const Self, base: Module.Path, path: Module.Path) !Str {
    var sb = std.ArrayList(u8).init(self.al);
    // TODO: i don't feel like making an iterator in stack :)
    for (base) |p| {
        try sb.appendSlice(p);
        try sb.append('/');
    }

    try sb.appendSlice(path[0]);
    for (path[1..path.len]) |p| {
        try sb.append('/');
        try sb.appendSlice(p);
    }

    try sb.appendSlice(".kkc");

    return sb.items;
}
