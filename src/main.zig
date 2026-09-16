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

pub fn main(init: std.process.Init) !void {
    const io = init.io;

    // SETUP
    const al = init.gpa;

    // global allocator for STUFF
    const arena = init.arena;
    const aa = arena.allocator();

    // PARSE ARGS
    const opts = try Args.parse(init.minimal.args.iterate(), aa);

    const compilationStartTime = std.Io.Timestamp.now(io, .real);
    var modules = try preloadModules(&opts, io, init.environ_map, aa);
    const fileonly = std.fs.path.basename(opts.filename);
    _ = try compileFile(&modules, fileonly, io);

    const compilationTime = std.Io.Timestamp.durationTo(compilationStartTime, std.Io.Timestamp.now(io, .real)).toMilliseconds();

    std.debug.print("=== compilation time: {}ms ===\n", .{compilationTime});

    // context setup
    var fakeNewline: bool = undefined;
    const fakeHackCtx = ast.Ctx.init(&fakeNewline, modules.typeContext);
    fakeNewline = false; // SIKE (but obv. temporary)

    if (!opts.hideErrors) {
        for (modules.errors.list.items) |err| {
            err.err.print(fakeHackCtx, err.module);
        }
    } else {
        if (modules.errors.list.items.len > 0) {
            std.debug.print("Hidden {} errors.\n", .{modules.errors.list.items.len});
        }
    }

    const moduleAST = modules.getAST();

    if (modules.errors.list.items.len > 0) return;

    // go and interpret
    if (opts.backend) |backend| {
        switch (backend) {
            .c => {
                // mono it
                // var backend = Bytecode.Backend.init(aa, modules.typeContext);
                var cbackend = C.init(aa, modules.typeContext);
                try C.Mono.mono(moduleAST, modules.getRoots(), &modules.prelude.?, modules.typeContext, &cbackend, io, aa, true);

                const outname = opts.exeName orelse std.fs.path.stem(opts.filename);

                var stdoutbuf: [4096]u8 = undefined;
                var stdout = std.Io.File.stdout().writer(io, &stdoutbuf);
                const outwriter = &stdout.interface;
                const ccomp = try compileC(aa, &cbackend, outname, null, outwriter, io);
                try stdout.flush();

                if (opts.printAST or opts.printRootAST) {
                    try cbackend.writeTo(outwriter);
                }

                try stdout.flush();

                std.debug.print("=== writing and compiling (C) time: {}ms ===\n", .{ccomp.time});

                if (ccomp.succeeded and !opts.dontCompile and !opts.dontRun) {
                    const term = try runExe(io, aa, ccomp.exeFilename, opts.programArgs);

                    switch (term) {
                        .exited => |code| {
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
            const interpretStartTime = std.Io.Timestamp.now(io, .real);

            // how would I handle a partially declared Prelude? or should I even do it? it may be useful?
            const ret = try Interpreter.run(moduleAST, modules.prelude.?, modules.typeContext, opts.programArgs, aa, al);
            const interpretTime = std.Io.Timestamp.durationTo(interpretStartTime, std.Io.Timestamp.now(io, .real)).toMilliseconds();

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

pub fn preloadModules(opts: *const Args, io: std.Io, environ: *const std.process.Environ.Map, aa: std.mem.Allocator) !Modules {
    const stdRoot = environ.get("KKC_STD") orelse b: {
        std.debug.print("KKC_STD env var not set. Defaulting to 'std/'.\n", .{});
        break :b "std/";
    };

    // -|| MODULES ||-

    const errors = try common.allocOne(aa, Errors.init(aa));
    const typeContext = try common.allocOne(aa, try TypeContext.init(aa, errors));
    const root = std.fs.path.dirname(opts.filename) orelse "";
    var modules = try Modules.init(aa, errors, typeContext, root, stdRoot, opts);

    if (!opts.noImplicitPrelude) {
        const prelude = try modules.loadPrelude(io);
        typeContext.prelude = prelude;
        if (!opts.noDefaultImports) {
            _ = try modules.loadConverged(io);
        }
    }

    return modules;
}

pub fn compileC(aa: std.mem.Allocator, cbackend: *const C, outName: Str, mdir: ?Str, out: *std.Io.Writer, io: std.Io) !struct {
    time: i64,
    exeFilename: [:0]const u8,
    succeeded: bool,
} {
    const cWritingCompilingStartTime = std.Io.Timestamp.now(io, .real);

    var c_filename = try std.mem.concat(aa, u8, &.{ outName, ".c" });
    if (mdir) |dir| {
        c_filename = try std.mem.concat(aa, u8, &.{ dir, "/", c_filename });
    }

    var outname = outName;
    if (mdir) |dir| {
        outname = try std.mem.concat(aa, u8, &.{ dir, "/", outname });
    }

    const file = try std.Io.Dir.cwd().createFile(io, c_filename, .{});
    defer file.close(io);

    var filebuf: [4096]u8 = undefined;
    var writer = file.writer(io, &filebuf);
    try cbackend.writeTo(&writer.interface);

    var copts = std.ArrayList([]const u8).empty; // aa
    try copts.appendSlice(aa, &.{ "cc", c_filename, "-o", outname });

    var prog_c_opts = cbackend.coptions.iterator();
    while (prog_c_opts.next()) |copt| {
        try copts.append(aa, copt.*);
    }

    const res = try std.process.run(aa, io, .{ .argv = copts.items });
    try out.writeAll(res.stdout);
    try out.writeAll(res.stderr);

    const cWritingCompilingTime = std.Io.Timestamp.durationTo(cWritingCompilingStartTime, std.Io.Timestamp.now(io, .real)).toMilliseconds();
    const exeFilename = try aa.dupeSentinel(u8, outname, 0);
    switch (res.term) {
        .exited => |code| {
            if (code == 0) {
                return .{
                    .time = cWritingCompilingTime,
                    .exeFilename = exeFilename,
                    .succeeded = true,
                };
            }
        },
        else => {},
    }

    return .{
        .time = cWritingCompilingTime,
        .exeFilename = exeFilename,
        .succeeded = false,
    };
}

pub fn runExe(io: std.Io, aa: std.mem.Allocator, outName: Str, args: []const [*:0]const u8) !std.process.Child.Term {
    const exe_name = if (outName[0] != '/') try std.mem.concat(aa, u8, &.{ "./", outName }) else outName;

    // prepare prog with args.
    var proc_params = std.ArrayList([]const u8).empty; // aa
    try proc_params.append(aa, exe_name);
    for (args[1..]) |ztArg| {
        var arg: []const u8 = undefined;
        arg.ptr = ztArg;
        arg.len = std.mem.len(ztArg);
        try proc_params.append(aa, arg);
    }
    const options = std.process.RunOptions{ .argv = proc_params.items };
    var child = try std.process.spawn(io, .{
        .argv = options.argv,
        .cwd = options.cwd,
        .environ_map = options.environ_map,
        .expand_arg0 = options.expand_arg0,
        .progress_node = options.progress_node,
        .create_no_window = options.create_no_window,
        .disable_aslr = options.disable_aslr,

        .stdin = .inherit,
        .stdout = .inherit,
        .stderr = .inherit,
    });

    const term = try child.wait(io);

    return term;
}

pub fn compileFile(modules: *Modules, filename: Str, io: std.Io) !Module {
    return try modules.initialModule(&filename, io);
}
