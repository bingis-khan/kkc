const std = @import("std");
const Parser = @import("parser.zig");
const Lexer = @import("lexer.zig").Lexer;
const ast = @import("ast.zig");
const Errors = @import("error.zig").Errors;
const Interpreter = @import("Interpreter.zig");
const Prelude = @import("Prelude.zig");

pub fn main() !void {
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

    const source = try std.fs.cwd().readFileAlloc(al, "test.kkc", 1337420);
    defer al.free(source);
    const lexer = Lexer.init(source);
    // var l = lexer;
    // while (!l.finished()) {
    //     const tok = l.nextToken();
    //     std.debug.print("{}\n", .{tok});
    // }

    var errors = Errors.init(aa);
    var parser = try Parser.init(lexer, &errors, aa);
    const module = try parser.parse();
    // var hadNewline: bool = undefined;
    // const ctx = ast.Ctx.init(&hadNewline, &parser.typeContext);
    // module.ast.print(ctx);

    var fakeNewline: bool = undefined;
    const fakeHackCtx = ast.Ctx.init(&fakeNewline, &parser.typeContext);
    fakeNewline = false; // SIKE (but obv. temporary)
    for (errors.items) |err| {
        err.print(fakeHackCtx);
    }

    // temp: make prelude here.
    const prelude = try parser.mkPrelude();

    // go and interpret
    if (errors.items.len == 0) {
        const ret = try Interpreter.run(module.ast, prelude, &parser.typeContext, aa);
        std.debug.print("=== return value: {} ===\n", .{ret});
    }
}
