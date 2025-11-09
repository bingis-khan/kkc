const std = @import("std");
const Parser = @import("parser.zig");
const Lexer = @import("lexer.zig").Lexer;
const ast = @import("ast.zig");

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
    var l = lexer;
    while (!l.finished()) {
        const tok = l.nextToken();
        std.debug.print("{}\n", .{tok});
    }

    var parser = Parser.init(lexer, aa);
    const module = try parser.parse();
    var hadNewline: bool = undefined;
    const ctx = ast.Ctx.init(&hadNewline);
    module.ast.print(ctx);
    for (module.errors.items) |err| {
        err.print();
    }

    // var what = std.ArrayList(u8).init(al);
    // defer what.deinit();
    // try what.append('m');
    // defer what.deinit();
}
