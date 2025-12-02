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
pub const ModuleLookup = std.HashMap(Module.BasePath, ?Module, Module.BasePath.Ctx, std.hash_map.default_max_load_percentage);

// NOTE: null means that the Module is still being parsed.
modules: ModuleLookup,
errors: *Errors,
typeContext: *TypeContext,
al: std.mem.Allocator,
defaultExports: std.ArrayList(Module.Exports), // TODO: instead of re-adding stuff for every module, save the state and COPY.
prelude: ?Prelude,
full: std.ArrayList(ast),
rootPath: Str,

pub fn init(al: std.mem.Allocator, errors: *Errors, typeContext: *TypeContext, root: Str) Self {
    return .{
        .modules = ModuleLookup.init(al),
        .errors = errors,
        .al = al,
        .typeContext = typeContext,
        .defaultExports = std.ArrayList(Module.Exports).init(al),
        .prelude = null,
        .full = std.ArrayList(ast).init(al),
        .rootPath = root,
    };
}

pub fn loadPrelude(self: *Self, path: Str) !Prelude {
    const preludeModule = try self.loadDefault(path);
    const prelude = try preludeModule.mkPrelude();
    self.prelude = prelude;
    return prelude;
}

pub fn loadDefault(self: *Self, path: Str) !Module {
    const module = try self.loadModule(.{ .ByFilename = .{
        .isSTD = true,
        .path = path,
    } });
    try self.defaultExports.append(module.?.exports);
    return module.?;
}

pub fn initialModule(self: *Self, filename: Str) !Module {
    return (try self.loadModule(.{ .ByFilename = .{ .isSTD = false, .path = filename } })).?;
}

// NOTE: this function reports errors with modules that were not found.
// OMG I HATE THIS BRUH. IT BECAME SO COMPLICATED. FOR SOME REASON I CANT THINK ABOUT THIS STUFF??????? WTF??????
pub fn loadModule(self: *Self, pathtype: union(enum) {
    ByModulePath: struct { base: Module.BasePath, path: Module.Path },
    ByFilename: struct { isSTD: bool, path: Str },
}) !?Module {
    var fullPath: Module.BasePath = switch (pathtype) {
        .ByModulePath => |modpath| bb: {
            const fullPath: Module.BasePath = b: {
                const base = modpath.base;
                const path = modpath.path;
                if (base.path.len == 0) break :b .{ .isSTD = base.isSTD, .path = path };
                if (path.len == 0) unreachable;

                const pp = try self.al.alloc(Str, base.path.len + path.len);
                std.mem.copyForwards(Str, pp[0..base.path.len], base.path);
                std.mem.copyForwards(Str, pp[base.path.len .. base.path.len + path.len], path);
                break :b .{ .isSTD = base.isSTD, .path = pp };
            };

            if (self.modules.get(fullPath)) |module| {
                if (module == null) {
                    std.debug.print("{s}", .{fullPath.path[fullPath.path.len - 1]});
                    try self.errors.append(.{ .CircularModuleReference = .{} });
                    return null;
                }

                return module.?;
            } else {
                break :bb fullPath;
            }
        },

        .ByFilename => |filename| .{
            .isSTD = filename.isSTD,
            .path = &.{filename.path},
        },
    };

    const source = switch (pathtype) {
        .ByModulePath => self.readSource(try self.modulePathToFilepath(fullPath)) catch |err| switch (err) {
            error.FileNotFound => b: {
                if (fullPath.isSTD) return error.TempError;
                fullPath.isSTD = true;

                if (self.modules.get(fullPath)) |module| {
                    if (module == null) {
                        try self.errors.append(.{ .CircularModuleReference = .{} });
                        return null;
                    }

                    return module.?;
                }

                const stdSource = self.readSource(try self.modulePathToFilepath(fullPath)) catch return error.TempError;
                break :b stdSource;
            },
            else => return error.TempError,
        },

        .ByFilename => |fullpath| b: {
            var filepath = std.ArrayList(u8).init(self.al);
            if (fullpath.isSTD) {
                try filepath.appendSlice("std/");
            } else {
                if (self.rootPath.len > 0) {
                    try filepath.appendSlice(self.rootPath);
                    try filepath.append('/');
                }
            }
            try filepath.appendSlice(fullpath.path);
            std.debug.print("{s}\n", .{filepath.items});
            break :b self.readSource(filepath.items) catch return error.TempError;
        },
    };

    try self.modules.put(fullPath, null);
    const lexer = Lexer.init(source);
    var l = lexer;
    while (!l.finished()) {
        const tok = l.nextToken();
        std.debug.print("{}\n", .{tok});
    }

    var parser = try Parser.init(lexer, self.prelude, switch (pathtype) {
        .ByModulePath => |modpath| modpath.base,
        .ByFilename => |filename| .{ .isSTD = filename.isSTD, .path = &.{} },
    }, self, self.errors, self.typeContext, self.al);
    for (self.defaultExports.items) |xports| {
        try parser.addExports(&xports);
    }
    const module = try parser.parse();
    try self.full.append(module.ast);
    try self.modules.put(fullPath, module);

    var hadNewline: bool = undefined;
    const ctx = ast.Ctx.init(&hadNewline, self.typeContext);
    module.ast.print(ctx);

    return module;
}

pub fn getAST(self: *const Self) []ast {
    return self.full.items;
}

fn readSource(self: *Self, filepath: Str) !Str {
    const source = try std.fs.cwd().readFileAlloc(self.al, filepath, 1337420);
    return source;
}

fn modulePathToFilepath(self: *const Self, base: Module.BasePath) !Str {
    var sb = std.ArrayList(u8).init(self.al);

    if (base.isSTD) {
        try sb.appendSlice("std/");
    } else {
        try sb.appendSlice(self.rootPath);
        try sb.append('/');
    }

    // TODO: i don't feel like making an iterator in stack :)
    for (base.path[0 .. base.path.len - 1]) |p| {
        try sb.appendSlice(p);
        try sb.append('/');
    }
    try sb.appendSlice(base.path[base.path.len - 1]);
    try sb.appendSlice(".kkc");

    return sb.items;
}
