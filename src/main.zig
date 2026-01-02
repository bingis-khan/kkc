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

    const stdRoot = std.process.getEnvVarOwned(aa, "KKC_STD") catch |e| switch (e) {
        error.EnvironmentVariableNotFound => b: {
            std.debug.print("KKC_STD env var not set. Defaulting to 'std/'.\n", .{});
            break :b "std/";
        },
        else => return e,
    };

    // PARSE ARGS
    const opts = try Args.parse(std.process.args(), aa);

    // -|| MODULES ||-
    const compilationStartTime = try std.time.Instant.now();

    var errors = Errors.init(aa);
    var typeContext = try TypeContext.init(aa, &errors);
    var modules = Modules.init(aa, &errors, &typeContext, "", stdRoot, &opts);

    const prelude = try modules.loadPrelude();
    _ = try modules.loadConverged();

    _ = try modules.initialModule(&opts.filename);
    const compilationTime = std.time.Instant.since(try std.time.Instant.now(), compilationStartTime) / std.time.ns_per_ms;
    std.debug.print("=== compilation time: {}ms ===\n", .{compilationTime});

    const fullAST = modules.getAST();

    var fakeNewline: bool = undefined;
    const fakeHackCtx = ast.Ctx.init(&fakeNewline, &typeContext);
    fakeNewline = false; // SIKE (but obv. temporary)
    for (errors.items) |err| {
        err.err.print(fakeHackCtx, err.module);
    }

    // go and interpret
    if (errors.items.len == 0 and !opts.dontRun) {
        const interpretStartTime = try std.time.Instant.now();
        const ret = try Interpreter.run(fullAST, prelude, &typeContext, opts.programArgs, aa);
        const interpretTime = std.time.Instant.since(try std.time.Instant.now(), interpretStartTime) / std.time.ns_per_ms;

        std.debug.print("=== return value: {} ===\n", .{ret});
        std.debug.print("=== interpret time: {}ms ===\n", .{interpretTime});
    }
}
