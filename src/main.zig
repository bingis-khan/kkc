const std = @import("std");
const Parser = @import("parser.zig");
const Lexer = @import("lexer.zig").Lexer;
const ast = @import("ast.zig");
const Errors = @import("error.zig").Errors;
const Interpreter = @import("Interpreter.zig");
const Prelude = @import("Prelude.zig");
const Modules = @import("Modules.zig");
const TypeContext = @import("TypeContext.zig");
const common = @import("common.zig");
const Str = common.Str;
const Args = @import("Args.zig");

pub fn main() !void {
    // SETUP
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const al = gpa.allocator();
    defer {
        const deinit_status = gpa.deinit(); // this prints all the leaks
        std.debug.print("gpa deinit status = {}\n", .{deinit_status});
    }

    // global allocator for STUFF
    var arena = std.heap.ArenaAllocator.init(al);
    defer arena.deinit();
    const aa = arena.allocator();

    // PARSE ARGS
    const opts = try Args.parse(std.process.args(), aa);

    const compilationStartTime = try std.time.Instant.now();
    var modules = try preloadModules(&opts, aa);
    try compileFile(&modules, opts.filename);

    const compilationTime = std.time.Instant.since(try std.time.Instant.now(), compilationStartTime) / std.time.ns_per_ms;

    std.debug.print("=== compilation time: {}ms ===\n", .{compilationTime});

    var fakeNewline: bool = undefined;
    const fakeHackCtx = ast.Ctx.init(&fakeNewline, modules.typeContext);
    fakeNewline = false; // SIKE (but obv. temporary)
    for (modules.errors.items) |err| {
        err.err.print(fakeHackCtx, err.module);
    }

    // go and interpret
    if (modules.errors.items.len == 0 and !opts.dontRun) {
        const interpretStartTime = try std.time.Instant.now();
        const moduleAST = modules.getAST();
        const ret = try Interpreter.run(moduleAST, modules.prelude.?, modules.typeContext, opts.programArgs, aa);
        const interpretTime = std.time.Instant.since(try std.time.Instant.now(), interpretStartTime) / std.time.ns_per_ms;

        std.debug.print("=== return value: {} ===\n", .{ret});
        std.debug.print("=== interpret time: {}ms ===\n", .{interpretTime});
    }
}

pub const CompilationStuff = struct {
    prelude: Prelude,
    ast: []ast,
    modules: Modules,
    // compilationTimeMS: u64,
};

pub fn preloadModules(opts: *const Args, aa: std.mem.Allocator) !Modules {
    const stdRoot = std.process.getEnvVarOwned(aa, "KKC_STD") catch |e| switch (e) {
        error.EnvironmentVariableNotFound => b: {
            std.debug.print("KKC_STD env var not set. Defaulting to 'std/'.\n", .{});
            break :b "std/";
        },
        else => return e,
    };

    // -|| MODULES ||-

    const errors = try common.allocOne(aa, Errors.init(aa));
    const typeContext = try common.allocOne(aa, try TypeContext.init(aa, errors));
    var modules = Modules.init(aa, errors, typeContext, "", stdRoot, opts);

    _ = try modules.loadPrelude();
    _ = try modules.loadConverged();

    return modules;
}

pub fn compileFile(modules: *Modules, filename: Str) !void {
    _ = try modules.initialModule(&filename);
}
