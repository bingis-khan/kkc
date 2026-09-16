const std = @import("std");
const builtin = @import("builtin");
const kkc_main = @import("main.zig");
const Args = @import("Args.zig");
const common = @import("common.zig");
const Str = common.Str;
const Interpreter = @import("Interpreter.zig");
const Errors = @import("error.zig").Errors;
const AST = @import("ast.zig");
const TypeContext = @import("TypeContext.zig");
const Modules = @import("Modules.zig");
const Module = @import("Module.zig");
const ast = @import("ast.zig");
const MonoC = @import("mono/c.zig");

const BaseDir = "test/tests/";

pub fn main(init: std.process.Init) !void {
    const gpa = init.gpa;
    const io = init.io;
    const env = init.environ_map;
    try runTests(InterpreterRunner, io, init.minimal.args.iterate(), env, gpa);

    const dir = tmpDir(io, .{}).dir;
    const cthing = CRunner{ .dir = dir };
    try runTests(cthing, io, init.minimal.args.iterate(), env, gpa);
}

fn runTests(Runner: anytype, io: std.Io, args: std.process.Args.Iterator, env: *const std.process.Environ.Map, al: std.mem.Allocator) !void {
    // TODO: for now allocate everything in arena.
    // later we should free old stuff.
    // global allocator for STUFF
    var arena = std.heap.ArenaAllocator.init(al);
    defer arena.deinit();

    const aa = arena.allocator();

    var state = try Runner.init(io, env, aa);

    var tests = std.ArrayList(Str).empty; // al
    defer {
        for (tests.items) |t| {
            al.free(t);
        }
        tests.deinit(al);
    }

    var argIt = args;
    _ = argIt.skip();
    var testOptions = TestOptions{};
    while (argIt.next()) |arg| {
        if (startsWith(arg, "-")) {
            const opt = arg[1..];
            if (common.streq(opt, "f")) {
                testOptions.failingOnly = true;
            } else {
                errprint("unrecognized option {s}\n", .{arg});
            }
        } else {
            testOptions.filter = arg;
        }
    }

    var dir = try std.Io.Dir.cwd().openDir(io, BaseDir, .{ .iterate = true });
    defer dir.close(io);
    var dirIterator = dir.iterate();
    while (try dirIterator.next(io)) |dirContent| {
        if (testOptions.filter == null or startsWith(dirContent.name, testOptions.filter.?)) {
            try tests.append(al, try al.dupe(u8, dirContent.name));
        }
    }

    std.mem.sort(Str, tests.items, @as(void, undefined), (struct {
        fn order(ctx: void, lhs: Str, rhs: Str) bool {
            _ = ctx;
            return std.mem.order(u8, lhs, rhs) == .lt;
        }
    }).order);

    const total = tests.items.len;
    var passed: u32 = 0;
    var todo: u32 = 0;
    var skipped: u32 = 0;
    for (tests.items) |filename| {
        const result = runTest(filename, io, aa, &state);

        if (!testOptions.failingOnly or !result.status.passed()) {
            std.debug.print("[{s}] ({s}) {s}\n", .{ switch (result.status) {
                .Passed => "V",
                .Disabled => ".",
                .Todo => "todo",
                else => "X",
            }, result.filename, result.testname });
        }

        if (result.status == .Passed) {
            passed += 1;
        } else if (result.status == .Disabled) {
            skipped += 1;
        } else if (result.status == .Todo) {
            todo += 1;
        }

        if (result.errors) |errors| {
            var fakeNewline: bool = undefined;
            const fakeHackCtx = AST.Ctx.init(&fakeNewline, result.typeContext.?);
            fakeNewline = false; // SIKE (but obv. temporary)
            for (errors.list.items) |err| {
                err.err.print(fakeHackCtx, err.module);
            }
        }

        // print errors.
        switch (result.status) {
            .CompilerError => |cerr| std.debug.print("{s}\n", .{@errorName(cerr)}),
            else => {},
        }

        if (result.output) |output| {
            std.debug.print("Output not matched.\nExpected:\n{s}\nGot:\n{s}\n", .{ output.expected, output.got });
        }
        if (result.returnValue) |returnValue| {
            std.debug.print("Return value not matched.\nExpected: {}\nGot: {}\n", .{ returnValue.expected, returnValue.got });
        }

        for (result.subtestErrors) |subtestErr| {
            subtestErr.subtest.printName();
            errprint(" : {s}\n", .{subtestErr.err});
        }
    }

    std.debug.print("Passed {}/{} (todo {}) (skipped {})\n", .{ passed, total - skipped - todo, todo, skipped });
}

const InterpreterRunner = struct {
    const State = struct {
        modules: Modules,
        al: std.mem.Allocator,
        io: std.Io,

        test_modules: ?Modules = null,

        fn beforeTest(self: *@This()) !void {
            self.test_modules = try self.modules.cloneWithAllocator(self.al);
        }

        fn compile(self: *@This(), filename: Str) !CompilationState {
            const module = try kkc_main.compileFile(&self.test_modules.?, filename, self.io);
            return .{
                .errors = self.test_modules.?.errors,
                .typeContext = self.test_modules.?.typeContext,
                .mainModule = module,
            };
        }

        fn run(self: *@This()) !i64 {
            return try Interpreter.run(self.test_modules.?.getAST(), self.test_modules.?.prelude.?, self.test_modules.?.typeContext, &.{}, self.al, self.al);
        }
    };

    fn init(io: std.Io, env: *const std.process.Environ.Map, al: std.mem.Allocator) !State {
        const opts = Args{ .filename = "miauuuuuuuuuu" };
        const ogModules = try kkc_main.preloadModules(&opts, io, env, al);
        return .{ .modules = ogModules, .al = al, .io = io };
    }
};

const CRunner = struct {
    const State = struct {
        io: std.Io,
        env: *const std.process.Environ.Map,
        al: std.mem.Allocator,
        dir: Str,

        test_modules: ?Modules = null,
        cCompilerOutput: ?std.ArrayList(u8) = null,
        exeName: ?[:0]const u8 = null,

        fn beforeTest(self: *@This()) !void {
            // NOTE: well, i got what i deserved. the global state in the ast is fucking me up. i gotta remove it and do it properly.
            const opts = Args{ .filename = "miauuuuuuuuuu" };
            const ogModules = try kkc_main.preloadModules(&opts, self.io, self.env, self.al);
            self.test_modules = ogModules;
            self.cCompilerOutput = null; // self.al
        }

        fn compile(self: *@This(), filename: Str) !CompilationState {
            const modules = &self.test_modules.?;
            const module = try kkc_main.compileFile(modules, filename, self.io);

            var cbackend = MonoC.init(self.al, modules.typeContext);
            // TODO: this can crash. I should also run it in a different process. But how would we transfer data?
            try MonoC.Mono.mono(modules.getAST(), modules.getRoots(), &modules.prelude.?, modules.typeContext, &cbackend, self.io, self.al, false);

            const outname = std.fs.path.stem(filename);
            var outwriter = std.Io.Writer.Allocating.init(self.al);
            const ccomp = try kkc_main.compileC(self.al, &cbackend, outname, self.dir, &outwriter.writer, self.io);
            self.exeName = ccomp.exeFilename;
            self.cCompilerOutput = outwriter.toArrayList();

            // todo: compile the C file too. somehow put extra errors here.
            return .{
                .errors = self.test_modules.?.errors,
                .typeContext = self.test_modules.?.typeContext,
                .mainModule = module,
            };
        }

        fn run(self: *@This()) !i64 {
            const ret = try kkc_main.runExe(self.io, self.al, self.exeName.?, &.{self.exeName.?});
            return switch (ret) {
                .exited => |exitcode| exitcode,
                else => -1,
            };
        }
    };

    dir: std.Io.Dir,

    fn init(self: *const @This(), io: std.Io, env: *const std.process.Environ.Map, al: std.mem.Allocator) !State {
        const dirpath = try self.dir.realPathFileAlloc(io, ".", al);
        return .{
            .io = io,
            .env = env,
            .al = al,
            .dir = dirpath,
        };
    }
};

const CompilationState = struct {
    errors: *const Errors,
    typeContext: *const TypeContext,
    mainModule: Module,
};

const TestResult = struct {
    filename: Str,
    testname: Str,
    status: union(enum) {
        CompilerError: CompilerError,
        FailedToCompile,
        OutputNotMatched,
        SubtestFailed,
        // ASTNotMatched,
        // HadLeaks,  // TODO: not yet checked, because I use arena all the time
        Disabled,
        Todo,
        Passed,

        fn passed(self: @This()) bool {
            return switch (self) {
                .Disabled, .Todo, .Passed => true,
                else => false,
            };
        }
    },

    errors: ?*const Errors,
    typeContext: ?*const TypeContext,

    output: ?struct {
        expected: Str,
        got: Str,
    } = null,

    returnValue: ?struct {
        expected: u8,
        got: u8,
    } = null,

    subtestErrors: []SubtestError,

    compileMS: ?u64,
    runMS: ?u64,

    const SubtestError = struct { subtest: Subtest, err: Str };
};
fn runTest(filename: Str, io: std.Io, al: std.mem.Allocator, runner: anytype) TestResult {
    return runTest_(filename, io, al, runner) catch |err| .{
        .filename = filename,
        .testname = "???",
        .status = .{ .CompilerError = err },
        .errors = null,
        .typeContext = null,
        .compileMS = null,
        .runMS = null,
        .subtestErrors = &.{},
    };
}

const CompilerError = error{InterpreterPanic} || ErrSet(kkc_main.preloadModules) || ErrSet(kkc_main.compileFile) || ErrSet(runAndReadStdout) || ErrSet(readHeader);

fn runTest_(filename: Str, io: std.Io, aa: std.mem.Allocator, runner: anytype) !TestResult {
    try runner.beforeTest();

    // stuff
    const relFilename = try std.mem.concat(aa, u8, &.{ BaseDir, filename });
    const header = try readHeader(relFilename, io, aa);
    if (header.disabled) |disability| {
        return TestResult{
            .filename = filename,
            .testname = header.testTitle,
            .status = switch (disability) {
                .Disabled => .Disabled,
                .Todo => .Todo,
            },
            .errors = null,
            .typeContext = null,
            .subtestErrors = &.{},
            .compileMS = null,
            .runMS = null,
        };
    }

    const compilationStartTime = std.Io.Timestamp.now(io, .real);
    const compilationState = try runner.compile(relFilename);
    const compilationTime = std.Io.Timestamp.durationTo(compilationStartTime, std.Io.Timestamp.now(io, .real)).toMilliseconds();

    var result = TestResult{
        .filename = filename,
        .testname = header.testTitle,
        .status = .Passed,
        .errors = compilationState.errors,
        .typeContext = compilationState.typeContext,
        .compileMS = @intCast(compilationTime),
        .runMS = null,
        .subtestErrors = &.{},
    };

    if (compilationState.errors.empty()) {
        const run = try runAndReadStdout(io, aa, runner);

        if (!run.failed) {
            if (!common.streq(run.stdout, header.expectedOutput)) {
                if (result.status == .Passed)
                    result.status = .OutputNotMatched;
                result.output = .{
                    .expected = header.expectedOutput,
                    .got = run.stdout,
                };
            }

            if (run.returnValue != header.expectedReturnCode) {
                if (result.status == .Passed)
                    result.status = .OutputNotMatched;
                result.returnValue = .{
                    .expected = header.expectedReturnCode,
                    .got = run.returnValue,
                };
            }

            var subtestErrors = std.ArrayList(TestResult.SubtestError).empty; // aa
            for (header.subtests) |*subtest| {
                if (try subtest.verify(&compilationState.mainModule, aa)) |err| {
                    try subtestErrors.append(aa, .{ .subtest = subtest.*, .err = err });
                    if (result.status == .Passed)
                        result.status = .SubtestFailed;
                }
            }

            result.subtestErrors = subtestErrors.items;
        } else {
            result.status = .{ .CompilerError = error.InterpreterPanic };
        }

        result.testname = header.testTitle;
        result.runMS = run.interpretTimeMS;
    } else {
        result.status = .FailedToCompile;
    }

    return result;
}

const Run = struct {
    failed: bool,
    stdout: Str,
    returnValue: u8,
    interpretTimeMS: u64,
};
fn runAndReadStdout(io: std.Io, aa: std.mem.Allocator, runner: anytype) anyerror!Run {
    const interpretStartTime = std.Io.Timestamp.now(io, .real);
    var fd: [2]std.c.fd_t = undefined;
    const res = std.c.pipe(&fd); // .{ read, write }
    if (res > 0) return error.ForkFailedOAlgo;
    const pid = std.c.fork();
    if (pid == 0) { // child process.
        errdefer std.process.exit(1); // in case of any errors, make sure to EXIT!
        _ = std.c.close(fd[0]); // close read - we are only writing

        const dup2_res = std.c.dup2(fd[1], std.Io.File.stdout().handle);
        if (dup2_res < 0) {
            return error.Dup2Failed;
        }
        _ = std.c.close(fd[1]);

        const ret = try runner.run();
        std.process.exit(@intCast(ret));
    }

    // PARENT
    _ = std.c.close(fd[1]); // close write

    // pump stdout of child to array reader.
    var writer = std.Io.Writer.Allocating.init(aa);
    const fakeyFile = std.Io.File{ .handle = fd[0], .flags = .{ .nonblocking = false } }; // make a zig file handle out of the thing. also, i randomly chose nonblocking=false
    var buf: [1024]u8 = undefined;
    var reader = fakeyFile.reader(io, &buf);
    _ = try std.Io.Reader.streamRemaining(&reader.interface, &writer.writer);

    // parent - wait and read stdout?
    var failed = false;
    var waitpidStatus: c_int = 0;
    _ = std.c.waitpid(pid, &waitpidStatus, 0);
    if ((waitpidStatus & 0x7f) > 0) {
        std.debug.print("waitpid() failed {}\n", .{waitpidStatus});
        failed = true;
    }
    const returnValue: u8 = @intCast((waitpidStatus >> 8) & 0xff); // that's how return value seems to be encoded!
    const interpretTime = std.Io.Timestamp.durationTo(interpretStartTime, std.Io.Timestamp.now(io, .real)).toMilliseconds();
    // std.debug.print("=== interpret time: {}ms ===\n", .{interpretTime});

    return .{
        .failed = failed,
        .returnValue = returnValue,
        .stdout = writer.toArrayList().items,
        .interpretTimeMS = @intCast(interpretTime),
    };
}

const Header = struct {
    expectedOutput: Str = "",
    expectedReturnCode: u8 = 0,
    testTitle: Str = "<title not provided>",
    disabled: ?enum {
        Disabled,
        Todo,
    } = null,
    subtests: []const Subtest = &.{},
};

fn readHeader(filepath: Str, io: std.Io, aa: std.mem.Allocator) !Header {
    var header = Header{};

    var file = try std.Io.Dir.cwd().openFile(io, filepath, .{});
    defer file.close(io);
    var read_buf: [4096]u8 = undefined;
    var reader = file.reader(io, &read_buf);
    const in_stream = &reader.interface;

    var expectedOutput = std.ArrayList(u8).empty; // aa
    var subtests = std.ArrayList(Subtest).empty; // aa
    while (in_stream.takeDelimiterInclusive('\n')) |line| {
        if (startsWith(line, "#!")) {
            // ignore shebang
        } else if (startsWith(line, "#$")) {
            header.testTitle = try aa.dupeSentinel(u8, trim(line[2..]), 0);
        } else if (startsWith(line, "#?")) {
            header.expectedReturnCode = std.fmt.parseInt(u8, trim(line[2..]), 10) catch unreachable;
        } else if (startsWith(line, "#=")) {
            var fieldIter = std.mem.splitAny(u8, trim(line[2..]), &std.ascii.whitespace);
            const maybeFunName = fieldIter.next();
            if (maybeFunName) |funName| {
                if (common.streq(funName, "disabled")) {
                    header.disabled = .Disabled;
                } //
                else if (common.streq(funName, "todo")) {
                    header.disabled = .Todo;
                } //
                else if (common.streq(funName, "envsize")) {
                    // TODO: i think we can do some fun zig stuff to automatically parse the arguments given an enum.
                    // thats for later doe.
                    const kcFunName = fieldIter.next() orelse {
                        errprint("expect function name", .{});
                        continue;
                    };
                    const envSizeStr = fieldIter.next() orelse {
                        errprint("expect env size", .{});
                        continue;
                    };
                    const envSize = parseInt(envSizeStr) catch {
                        errprint("could not parse envSize", .{});
                        continue;
                    };

                    try subtests.append(aa, .{ .envsize = .{ .kcFunName = try aa.dupe(u8, kcFunName), .envsize = envSize } });
                } //
                else {
                    std.debug.print("unknown option '{s}'\n", .{funName});
                }
            } else {
                std.debug.print("Empty option found.\n", .{});
            }
        } else if (startsWith(line, "##")) {
            // ignore! just a comment
        } else if (startsWith(line, "#")) {
            try expectedOutput.appendSlice(aa, trim(line[1..]));
            try expectedOutput.append(aa, '\n');
        } else {
            // header end.
            break;
        }
    } else |err| switch (err) {
        error.EndOfStream => {}, // technically valid on basically empty source file.
        error.StreamTooLong => unreachable, // should be effectively unreachable!.
        else => return err,
    }
    header.expectedOutput = expectedOutput.items;
    header.subtests = subtests.items;
    return header;
}

fn parseInt(s: Str) !i32 {
    return std.fmt.parseInt(i32, s, 10);
}

fn startsWith(s: Str, prefix: Str) bool {
    return std.mem.startsWith(u8, s, prefix);
}

fn trim(s: Str) Str {
    return std.mem.trim(u8, s, &std.ascii.whitespace);
}

fn ErrSet(fun: anytype) type {
    return @typeInfo(@typeInfo(@TypeOf(fun)).@"fn".return_type.?).error_union.error_set;
}

const errprint = std.debug.print;

const Subtest = union(enum) {
    envsize: struct { kcFunName: Str, envsize: i32 },

    fn printName(self: *const @This()) void {
        switch (self.*) {
            .envsize => |envsize| {
                errprint("envsize {s} {}", .{ envsize.kcFunName, envsize.envsize });
            },
        }
    }

    fn verify(self: *const @This(), module: *const Module, al: std.mem.Allocator) !?Str {
        switch (self.*) {
            .envsize => |envsize| {
                const fun = findFirstFunctionWithName(module, envsize.kcFunName) orelse return "Could not find function.";
                const foundFunEnvSize = fun.env.insts.items.len;
                if (foundFunEnvSize != envsize.envsize) {
                    return try std.fmt.allocPrint(al, "Expected env size is {}, but got {}", .{ envsize.envsize, foundFunEnvSize });
                } else {
                    return null;
                }
            },
        }
    }

    // NOTE: not very complete, since the full implementation would be long (miss Haskell ㅠㅠ)
    fn findFirstFunctionWithName(module: *const Module, kcFunName: Str) ?*ast.Function {
        return findFirstFunctionWithNameInStmts(module.AST.toplevel, kcFunName);
    }

    fn findFirstFunctionWithNameInStmts(stmts: []*ast.Stmt, kcFunName: Str) ?*ast.Function {
        for (stmts) |stmt| {
            switch (stmt.*) {
                .Function => |fun| {
                    if (common.streq(fun.name.name, kcFunName)) {
                        return fun;
                    }

                    if (findFirstFunctionWithNameInStmts(fun.body, kcFunName)) |found| {
                        return found;
                    }
                },

                else => {},
            }
        }

        return null;
    }
};

const TestOptions = struct {
    filter: ?Str = null,
    failingOnly: bool = false,
};

// tmpDir without the is_test requirement cuz lets be real
pub fn tmpDir(io: std.Io, opts: std.Io.Dir.OpenOptions) std.testing.TmpDir {
    const random_bytes_count = 12;
    const sub_path_len = comptime std.base64.url_safe.Encoder.calcSize(random_bytes_count);
    var random_bytes: [random_bytes_count]u8 = undefined;
    io.random(&random_bytes);
    var sub_path: [sub_path_len]u8 = undefined;
    _ = std.base64.url_safe.Encoder.encode(&sub_path, &random_bytes);

    const cwd = std.Io.Dir.cwd();
    var cache_dir = cwd.createDirPathOpen(io, ".zig-cache", .{}) catch
        @panic("unable to make tmp dir for testing: unable to make and open .zig-cache dir");
    defer cache_dir.close(io);
    const parent_dir = cache_dir.createDirPathOpen(io, "tmp", .{}) catch
        @panic("unable to make tmp dir for testing: unable to make and open .zig-cache/tmp dir");
    const dir = parent_dir.createDirPathOpen(io, &sub_path, .{ .open_options = opts }) catch
        @panic("unable to make tmp dir for testing: unable to make and open the tmp dir");

    return .{
        .dir = dir,
        .parent_dir = parent_dir,
        .sub_path = sub_path,
    };
}
