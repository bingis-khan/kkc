const std = @import("std");
const Parser = @import("parser.zig").Parser;
const Lexer = @import("lexer.zig").Lexer;

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
    const ast = try parser.parse();
    ast.print();

    // var what = std.ArrayList(u8).init(al);
    // defer what.deinit();
    // try what.append('m');
    // defer what.deinit();
}
