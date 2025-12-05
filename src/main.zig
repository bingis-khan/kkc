const std = @import("std");
const Parser = @import("parser.zig");
const Lexer = @import("lexer.zig").Lexer;
const ast = @import("ast.zig");
const Errors = @import("error.zig").Errors;
const Interpreter = @import("Interpreter.zig");
const Prelude = @import("Prelude.zig");
const Modules = @import("Modules.zig");
const TypeContext = @import("TypeContext.zig");
const Str = @import("common.zig").Str;

pub fn main() !void {
    var args = std.process.args();
    _ = args.skip(); // skip process name
    const filename = args.next() orelse {
        std.debug.print("Expect filename\n", .{});
        return;
    };
    std.debug.print("{s}\n", .{filename});
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

    // -|| MODULES ||-
    var errors = Errors.init(aa);
    var typeContext = try TypeContext.init(aa, &errors);
    var modules = Modules.init(aa, &errors, &typeContext, "");
    const prelude = try modules.loadPrelude(&"prelude.kkc");
    // TODO: load "converged" with default exports.
    // try modules.loadDefault("converged.kc");
    _ = try modules.initialModule(&filename);
    const fullAST = modules.getAST();

    var fakeNewline: bool = undefined;
    const fakeHackCtx = ast.Ctx.init(&fakeNewline, &typeContext);
    fakeNewline = false; // SIKE (but obv. temporary)
    for (errors.items) |err| {
        err.print(fakeHackCtx);
    }

    // go and interpret
    if (errors.items.len == 0) {
        const ret = try Interpreter.run(fullAST, prelude, &typeContext, aa);
        std.debug.print("=== return value: {} ===\n", .{ret});
    }
}
