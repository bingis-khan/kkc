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
const Module = @import("Module.zig");
const mono = @import("mono.zig");
const VM = @import("mono/bytecode.zig");
const Bytecode = VM.Mono;
const C = @import("mono/c.zig");

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
    _ = try compileFile(&modules, opts.filename);

    const compilationTime = std.time.Instant.since(try std.time.Instant.now(), compilationStartTime) / std.time.ns_per_ms;

    std.debug.print("=== compilation time: {}ms ===\n", .{compilationTime});

    // context setup
    var fakeNewline: bool = undefined;
    const fakeHackCtx = ast.Ctx.init(&fakeNewline, modules.typeContext);
    fakeNewline = false; // SIKE (but obv. temporary)

    if (!opts.hideErrors) {
        for (modules.errors.items) |err| {
            err.err.print(fakeHackCtx, err.module);
        }
    } else {
        if (modules.errors.items.len > 0) {
            std.debug.print("Hidden {} errors.\n", .{modules.errors.items.len});
        }
    }

    const moduleAST = modules.getAST();

    if (modules.errors.items.len > 0) return;

    // go and interpret
    if (opts.backend) |backend| {
        switch (backend) {
            .c => {
                // mono it
                // var backend = Bytecode.Backend.init(aa, modules.typeContext);
                var cbackend = C.init(aa, modules.typeContext);
                try C.Mono.mono(moduleAST, modules.getRoots(), &modules.prelude.?, modules.typeContext, &cbackend, aa);

                const cWritingCompilingStartTime = try std.time.Instant.now();
                const rawStdout = std.io.getStdOut().writer();
                var stdoutbuf = std.io.bufferedWriter(rawStdout);
                const stdout = stdoutbuf.writer();

                if (opts.printAST or opts.printRootAST) {
                    try cbackend.writeTo(stdout);
                    try stdoutbuf.flush();
                }

                const filename = std.fs.path.stem(opts.filename);
                const outname = opts.exeName orelse filename;
                const c_filename = try std.mem.concat(aa, u8, &.{ outname, ".c" });

                const file = try std.fs.cwd().createFile(c_filename, .{});
                defer file.close();

                const writer = file.writer();
                try cbackend.writeTo(writer);

                {
                    const res = try std.process.Child.run(.{ .allocator = aa, .argv = &.{ "cc", c_filename, "-o", outname } });
                    try stdout.writeAll(res.stdout);
                    try stdout.writeAll(res.stderr);
                    try stdoutbuf.flush();

                    switch (res.term) {
                        .Exited => |code| {
                            if (code != 0) {
                                return;
                            }
                        },
                        else => return,
                    }
                }

                const cWritingCompilingTime = std.time.Instant.since(try std.time.Instant.now(), cWritingCompilingStartTime) / std.time.ns_per_ms;
                std.debug.print("=== writing and compiling (C) time: {}ms ===\n", .{cWritingCompilingTime});

                if (!opts.dontRun) {
                    try stdoutbuf.flush();

                    const exe_name = try std.mem.concat(aa, u8, &.{ "./", outname });
                    var child = std.process.Child.init(&.{exe_name}, aa);
                    child.stdin_behavior = .Inherit;
                    child.stdout_behavior = .Inherit;
                    child.stderr_behavior = .Inherit;

                    try child.spawn();
                    const term = try child.wait();

                    switch (term) {
                        .Exited => |code| {
                            std.debug.print("program exited with code {}\n", .{code});
                        },
                        else => |code| {
                            std.debug.print("unexpected STOP ({})\n", .{code});
                        },
                    }
                }
            },
        }
    } else {
        if (!opts.dontRun) {
            const interpretStartTime = try std.time.Instant.now();
            const ret = try Interpreter.run(moduleAST, modules.prelude.?, modules.typeContext, opts.programArgs, aa);
            const interpretTime = std.time.Instant.since(try std.time.Instant.now(), interpretStartTime) / std.time.ns_per_ms;

            std.debug.print("=== return value: {} ===\n", .{ret});
            std.debug.print("=== interpret time: {}ms ===\n", .{interpretTime});
            return;
        }
    }

    // backend.print(fakeHackCtx);
    // const retVal = try VM.exec(&backend.cur, al);
    // std.debug.print("VM: {}\n", .{retVal});
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
    var modules = try Modules.init(aa, errors, typeContext, "", stdRoot, opts);

    const prelude = try modules.loadPrelude();
    typeContext.prelude = prelude;
    _ = try modules.loadConverged();

    return modules;
}

pub fn compileFile(modules: *Modules, filename: Str) !Module {
    return try modules.initialModule(&filename);
}
