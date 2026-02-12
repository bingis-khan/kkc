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

const BaseDir = "test/tests/";

pub fn main() !void {
    // SETUP
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const deinit_status = gpa.deinit(); // this prints all the leaks
        if (deinit_status == .leak) {
            // result.status = .HadLeaks;
            // bruh
            @panic("bruh, leaks");
        }
    }
    const al = gpa.allocator();

    // TODO: for now allocate everything in arena.
    // later we should free old stuff.
    // global allocator for STUFF
    var arena = std.heap.ArenaAllocator.init(al);
    defer arena.deinit();

    const aa = arena.allocator();

    const opts = Args{ .filename = "miauuuuuuuuuu" };
    const ogModules = try kkc_main.preloadModules(&opts, aa);

    var tests = std.ArrayList(Str).init(al);
    defer {
        for (tests.items) |t| {
            al.free(t);
        }
        tests.deinit();
    }

    var argIt = std.process.args();
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

    var dir = try std.fs.cwd().openDir(BaseDir, .{ .iterate = true });
    defer dir.close();
    var dirIterator = dir.iterate();
    while (try dirIterator.next()) |dirContent| {
        if (testOptions.filter == null or startsWith(dirContent.name, testOptions.filter.?)) {
            try tests.append(try al.dupe(u8, dirContent.name));
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
        var modules = try ogModules.cloneWithAllocator(aa);
        const result = runTest(filename, &modules);

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
            const fakeHackCtx = AST.Ctx.init(&fakeNewline, &result.typeContext.?);
            fakeNewline = false; // SIKE (but obv. temporary)
            for (errors.items) |err| {
                err.err.print(fakeHackCtx, err.module);
            }
        }

        // print errors.
        switch (result.status) {
            .CompilerError => |cerr| std.debug.print("{?}\n", .{cerr}),
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
            errprint(" failed with: {s}\n", .{subtestErr.err});
        }
    }

    std.debug.print("Passed {}/{} (todo {}) (skipped {})\n", .{ passed, total - skipped - todo, todo, skipped });
}

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

    errors: ?Errors,
    typeContext: ?TypeContext,

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
fn runTest(filename: Str, modules: *Modules) TestResult {
    return runTest_(filename, modules) catch |err| .{
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

fn runTest_(filename: Str, modules: *Modules) !TestResult {

    // stuff
    const aa: std.mem.Allocator = modules.al;
    const relFilename = try std.mem.concat(aa, u8, &.{ BaseDir, filename });
    const header = try readHeader(relFilename, aa);
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

    const compilationStartTime = try std.time.Instant.now();
    const module = try kkc_main.compileFile(modules, relFilename);
    const compilationTime = std.time.Instant.since(try std.time.Instant.now(), compilationStartTime) / std.time.ns_per_ms;

    var result = TestResult{
        .filename = filename,
        .testname = header.testTitle,
        .status = .Passed,
        .errors = modules.errors.*,
        .typeContext = modules.typeContext.*,
        .compileMS = compilationTime,
        .runMS = null,
        .subtestErrors = &.{},
    };

    if (modules.errors.items.len == 0) {
        const run = try runAndReadStdout(aa, modules);

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

            var subtestErrors = std.ArrayList(TestResult.SubtestError).init(aa);
            for (header.subtests) |*subtest| {
                if (try subtest.verify(&module, aa)) |err| {
                    try subtestErrors.append(.{ .subtest = subtest.*, .err = err });
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
fn runAndReadStdout(aa: std.mem.Allocator, modules: *const Modules) !Run {
    const interpretStartTime = try std.time.Instant.now();
    const fd = try std.posix.pipe(); // .{ read, write }
    const pid = try std.posix.fork();
    if (pid == 0) { // child process.
        errdefer std.process.exit(1); // in case of any errors, make sure to EXIT!
        std.posix.close(fd[0]); // close read - we are only writing

        try std.posix.dup2(fd[1], std.io.getStdOut().handle);
        std.posix.close(fd[1]);

        const ret = try Interpreter.run(modules.getAST(), modules.prelude.?, modules.typeContext, &.{}, aa);
        std.process.exit(@intCast(ret));
    }

    // PARENT
    std.posix.close(fd[1]); // close write

    // pump stdout of child to array reader.
    var progOut = std.ArrayList(u8).init(aa);
    const fakeyFile = std.fs.File{ .handle = fd[0] }; // make a zig file handle out of the thing.
    const reader = fakeyFile.reader();
    var pumper = std.fifo.LinearFifo(u8, .Dynamic).init(aa);
    try pumper.ensureTotalCapacity(4096);
    try pumper.pump(reader, progOut.writer());

    // parent - wait and read stdout?
    var failed = false;
    const waitpidStatus = std.posix.waitpid(pid, 0).status;
    if ((waitpidStatus & 0x7f) > 0) {
        std.debug.print("waitpid() failed {}\n", .{waitpidStatus});
        failed = true;
    }
    const returnValue: u8 = @intCast((waitpidStatus >> 8) & 0xff); // that's how return value seems to be encoded!
    const interpretTime = std.time.Instant.since(try std.time.Instant.now(), interpretStartTime) / std.time.ns_per_ms;
    // std.debug.print("=== interpret time: {}ms ===\n", .{interpretTime});

    return .{
        .failed = failed,
        .returnValue = returnValue,
        .stdout = progOut.items,
        .interpretTimeMS = interpretTime,
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

fn readHeader(filepath: Str, aa: std.mem.Allocator) !Header {
    var header = Header{};

    var file = try std.fs.cwd().openFile(filepath, .{});
    defer file.close();
    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();
    var buf: [1024]u8 = undefined;

    var expectedOutput = std.ArrayList(u8).init(aa);
    var subtests = std.ArrayList(Subtest).init(aa);
    while (in_stream.readUntilDelimiterOrEof(&buf, '\n') catch |err| switch (err) {
        error.StreamTooLong => unreachable, // should be effectively unreachable!.
        else => return err,
    }) |line| {
        if (startsWith(line, "#!")) {
            // ignore shebang
        } else if (startsWith(line, "#$")) {
            header.testTitle = try aa.dupeZ(u8, trim(line[2..]));
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

                    try subtests.append(.{ .envsize = .{ .kcFunName = try aa.dupe(u8, kcFunName), .envsize = envSize } });
                } //
                else {
                    std.debug.print("unknown option '{s}'\n", .{funName});
                }
            } else {
                std.debug.print("Empty option found.\n", .{});
            }
        } else if (startsWith(line, "#")) {
            try expectedOutput.appendSlice(trim(line[1..]));
            try expectedOutput.append('\n');
        } else {
            // header end.
            break;
        }
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
    return @typeInfo(@typeInfo(@TypeOf(fun)).Fn.return_type.?).ErrorUnion.error_set;
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
        return findFirstFunctionWithNameInStmts(module.ast.toplevel, kcFunName);
    }

    fn findFirstFunctionWithNameInStmts(stmts: []*const ast.Stmt, kcFunName: Str) ?*ast.Function {
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
