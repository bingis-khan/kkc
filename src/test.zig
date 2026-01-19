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

    var tests = std.ArrayList(Str).init(al);
    defer {
        for (tests.items) |t| {
            al.free(t);
        }
        tests.deinit();
    }

    var dir = try std.fs.cwd().openDir(BaseDir, .{ .iterate = true });
    defer dir.close();
    var dirIterator = dir.iterate();
    while (try dirIterator.next()) |dirContent| {
        try tests.append(try al.dupe(u8, dirContent.name));
    }

    std.mem.sort(Str, tests.items, @as(void, undefined), (struct {
        fn order(ctx: void, lhs: Str, rhs: Str) bool {
            _ = ctx;
            return std.mem.order(u8, lhs, rhs) == .lt;
        }
    }).order);

    const total = tests.items.len;
    var passed: u32 = 0;
    var skipped: u32 = 0;
    for (tests.items) |filename| {
        // global allocator for STUFF
        var arena = std.heap.ArenaAllocator.init(al);
        defer arena.deinit();

        const aa = arena.allocator();

        const result = runTest(filename, aa);
        std.debug.print("[{s}] ({s}) {s}\n", .{ switch (result.status) {
            .Passed => "V",
            .Disabled => ".",
            else => "X",
        }, result.filename, result.testname });

        if (result.status == .Passed) {
            passed += 1;
        } else if (result.status == .Disabled) {
            skipped += 1;
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
    }

    std.debug.print("Passed {}/{} (skipped {})\n", .{ passed, total - skipped, skipped });
}

const TestResult = struct {
    filename: Str,
    testname: Str,
    status: union(enum) {
        CompilerError: CompilerError,
        FailedToCompile,
        OutputNotMatched,
        // ASTNotMatched,
        // HadLeaks,  // TODO: not yet checked, because I use arena all the time
        Disabled,
        Passed,
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

    compileMS: ?u64,
    runMS: ?u64,
};
fn runTest(filename: Str, aa: std.mem.Allocator) TestResult {
    return runTest_(filename, aa) catch |err| .{
        .filename = filename,
        .testname = "???",
        .status = .{ .CompilerError = err },
        .errors = null,
        .typeContext = null,
        .compileMS = null,
        .runMS = null,
    };
}

const CompilerError = error{InterpreterPanic} || ErrSet(kkc_main.compileFile) || ErrSet(runAndReadStdout) || ErrSet(readHeader);

fn runTest_(filename: Str, aa: std.mem.Allocator) !TestResult {

    // stuff
    const relFilename = try std.mem.concat(aa, u8, &.{ BaseDir, filename });
    const header = try readHeader(relFilename, aa);
    if (header.disabled) {
        return TestResult{
            .filename = filename,
            .testname = header.testTitle,
            .status = .Disabled,
            .errors = null,
            .typeContext = null,
            .compileMS = null,
            .runMS = null,
        };
    }

    const opts = Args{ .filename = relFilename };
    const s = try kkc_main.compileFile(opts, aa);

    var result = TestResult{
        .filename = filename,
        .testname = header.testTitle,
        .status = .Passed,
        .errors = s.errors,
        .typeContext = s.typeContext,
        .compileMS = s.compilationTimeMS,
        .runMS = null,
    };

    if (s.errors.items.len == 0) {
        const run = try runAndReadStdout(aa, &s);

        if (run.failed) {
            result.status = .{ .CompilerError = error.InterpreterPanic };
        }

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
fn runAndReadStdout(aa: std.mem.Allocator, s: *const kkc_main.CompilationStuff) !Run {
    const interpretStartTime = try std.time.Instant.now();
    const fd = try std.posix.pipe(); // .{ read, write }
    const pid = try std.posix.fork();
    if (pid == 0) { // child process.
        std.posix.close(fd[0]); // close read - we are only writing

        try std.posix.dup2(fd[1], std.io.getStdOut().handle);
        std.posix.close(fd[1]);

        const ret = try Interpreter.run(s.ast, s.prelude, &s.typeContext, &.{}, aa);
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
    if (std.posix.waitpid(pid, 0).status != 0) {
        std.debug.print("waitpid() failed\n", .{});
        failed = true;
    }
    const interpretTime = std.time.Instant.since(try std.time.Instant.now(), interpretStartTime) / std.time.ns_per_ms;
    // std.debug.print("=== interpret time: {}ms ===\n", .{interpretTime});

    return .{
        .failed = failed,
        .stdout = progOut.items,
        .returnValue = 0, // TODO
        .interpretTimeMS = interpretTime,
    };
}

const Header = struct {
    expectedOutput: Str = "",
    expectedReturnCode: u8 = 0,
    testTitle: Str = "<title not provided>",
    disabled: bool = false,
};
fn readHeader(filepath: Str, aa: std.mem.Allocator) !Header {
    var header = Header{};

    var file = try std.fs.cwd().openFile(filepath, .{});
    defer file.close();
    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();
    var buf: [1024]u8 = undefined;

    var expectedOutput = std.ArrayList(u8).init(aa);
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
            const val = trim(line[2..]);
            if (common.streq(val, "disabled")) {
                header.disabled = true;
            } else {
                std.debug.print("unknown option '{s}'\n", .{val});
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
    return header;
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
