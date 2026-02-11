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
const UniqueGen = @import("UniqueGen.zig");
const Args = @import("Args.zig");

const Self = @This();
pub const ModuleLookup = std.HashMap(Module.BasePath, ?Module, Module.BasePath.Ctx, std.hash_map.default_max_load_percentage);

pub const Gen = struct {
    vars: UniqueGen,
    types: UniqueGen,
    cons: UniqueGen,
    tvars: UniqueGen,
    // mems: UniqueGen,
    classes: UniqueGen,
    classFuns: UniqueGen,
    instances: UniqueGen,
    assocs: UniqueGen,

    fn init() @This() {
        return std.mem.zeroInit(@This(), .{});
    }

    fn clone(self: *const @This()) @This() {
        // everything is by value, so no need to clone anything.
        return self.*;
    }
};

// NOTE: null means that the Module is still being parsed.
modules: ModuleLookup,
errors: *Errors,
typeContext: *TypeContext,
al: std.mem.Allocator,
preludeExports: ?Module.Exports, // TODO: instead of re-adding stuff for every module, save the state and COPY.
stdExports: ?Module.Exports,
prelude: ?Prelude,
full: std.ArrayList(ast),
rootPath: Str,
stdPath: Str,
gen: Gen,
opts: *const Args,

pub fn init(al: std.mem.Allocator, errors: *Errors, typeContext: *TypeContext, root: Str, stdPath: Str, args: *const Args) Self {
    return .{
        .modules = ModuleLookup.init(al),
        .errors = errors,
        .al = al,
        .typeContext = typeContext,
        .preludeExports = null,
        .stdExports = null,
        .prelude = null,
        .full = std.ArrayList(ast).init(al),
        .stdPath = stdPath,
        .rootPath = root,
        .gen = Gen.init(),
        .opts = args,
    };
}

const preludePath: Str = "prelude.kkc";
// sets defaultExports
pub fn loadPrelude(self: *Self) !Prelude {
    const preludeModule = (try self.loadModule(
        .{ .ByFilename = .{
            .isSTD = true,
            .path = &preludePath,
        } },
        .{ .printAST = self.opts.printAST, .printTokens = self.opts.printTokens },
    )) orelse unreachable;

    self.preludeExports = preludeModule.exports;

    const prelude = try preludeModule.mkPrelude(self.typeContext);
    self.prelude = prelude;

    return prelude;
}

// sets stdExports
const convergedPath: Str = "converged.kkc";
pub fn loadConverged(self: *Self) !Module {
    const module = try self.loadModule(
        .{ .ByFilename = .{
            .isSTD = true,
            .path = &convergedPath,
        } },
        .{ .printAST = self.opts.printAST, .printTokens = self.opts.printTokens },
    );
    self.stdExports = module.?.exports;
    return module.?;
}

pub fn initialModule(self: *Self, filename: *const Str) !Module {
    return (try self.loadModule(
        .{ .ByFilename = .{ .isSTD = false, .path = filename } },
        .{ .printAST = self.opts.printRootAST or self.opts.printAST, .printTokens = self.opts.printRootTokens or self.opts.printTokens },
    )).?;
}

// NOTE: this function reports errors with modules that were not found.
// OMG I HATE THIS BRUH. IT BECAME SO COMPLICATED. FOR SOME REASON I CANT THINK ABOUT THIS STUFF??????? WTF??????
pub fn loadModule(self: *Self, pathtype: union(enum) {
    ByModulePath: struct { base: Module.BasePath, path: Module.Path },
    ByFilename: struct { isSTD: bool, path: *const Str },
}, opts: struct {
    printTokens: ?bool = null,
    printAST: ?bool = null,
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
                    try self.errors.append(.{
                        .err = .{ .CircularModuleReference = .{} },
                        .module = .{
                            .name = "<TODO>",
                            .source = "<TODO>",
                        },
                    });
                    return null;
                }

                return module.?;
            } else {
                break :bb fullPath;
            }
        },

        .ByFilename => |filename| .{
            .isSTD = filename.isSTD,
            .path = common.singleElemSlice(Str, filename.path),
        },
    };

    const source = switch (pathtype) {
        .ByModulePath => self.readSource(try self.modulePathToFilepath(fullPath)) catch |err| switch (err) {
            error.FileNotFound => b: {
                if (fullPath.isSTD) return error.TempError;
                fullPath.isSTD = true;

                if (self.modules.get(fullPath)) |module| {
                    if (module == null) {
                        try self.errors.append(.{
                            .err = .{ .CircularModuleReference = .{} },
                            .module = .{
                                .name = "<TODO>",
                                .source = "<TODO>",
                            },
                        });
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
                try filepath.appendSlice(self.stdPath);
            } else {
                if (self.rootPath.len > 0) {
                    try filepath.appendSlice(self.rootPath);
                    try filepath.append('/');
                }
            }
            try filepath.appendSlice(fullpath.path.*);
            // std.debug.print("{s}\n", .{filepath.items});
            break :b self.readSource(filepath.items) catch return error.TempError;
        },
    };

    try self.modules.put(fullPath, null);

    const moduleName = fullPath.path[fullPath.path.len - 1];
    const lexer = Lexer.init(source, moduleName, self.errors);

    if (opts.printTokens orelse self.opts.printTokens) {
        std.debug.print("TOKENS OF {s}\n", .{fullPath.path[fullPath.path.len - 1]});
        var l = lexer;
        l.errors = null; // disable errors
        while (!l.finished()) {
            const tok = l.nextToken();
            std.debug.print("{}\n", .{tok});
        }
    }

    const modBasePath: Module.BasePath = switch (pathtype) {
        .ByModulePath => |modpath| modpath.base,
        .ByFilename => |filename| .{ .isSTD = filename.isSTD, .path = &.{} },
    };
    var parser = try Parser.init(lexer, self.prelude, modBasePath, moduleName, self, self.errors, self.typeContext, self.al);

    if (self.preludeExports) |*xports| {
        try parser.addExports(xports);
    }

    if (!fullPath.isSTD) {
        try parser.addExports(&self.stdExports.?);
    }

    const module = try parser.parse();
    try self.full.append(module.ast);
    try self.modules.put(fullPath, module);

    if (opts.printAST orelse self.opts.printAST) {
        var hadNewline: bool = undefined;
        const ctx = ast.Ctx.init(&hadNewline, self.typeContext);
        module.ast.print(ctx);
    }

    // module exports
    if (self.opts.printExports) {
        var hadNewline: bool = undefined;
        const ctx = ast.Ctx.init(&hadNewline, self.typeContext);
        ctx.print(.{ "Exports for module ", moduleName, "\n" });
        module.exports.print(ctx);
    }

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
        try sb.appendSlice(self.stdPath);
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

pub fn cloneWithAllocator(self: *const Self, al: std.mem.Allocator) !Self {
    const nuErrors = try common.allocOne(al, try common.cloneArrayListWithAllocator(self.errors.*, al));
    return .{
        .modules = try self.modules.cloneWithAllocator(al),
        .errors = nuErrors,
        .typeContext = try common.allocOne(al, try self.typeContext.cloneWithAllocator(nuErrors, al)),
        .al = al,
        .preludeExports = self.preludeExports.?.clone(), // TODO: instead of re-adding stuff for every module, save the state and COPY.
        .stdExports = self.stdExports.?.clone(),
        .prelude = self.prelude,
        .full = try common.cloneArrayListWithAllocator(self.full, al),
        .rootPath = self.rootPath,
        .stdPath = self.stdPath,
        .gen = self.gen.clone(),
        .opts = self.opts,
    };
}
