const std = @import("std");
const parser = @import("parser.zig");
const Lexer = @import("lexer.zig").Lexer;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const al = gpa.allocator();
    defer {
        const deinit_status = gpa.deinit(); // this prints all the leaks
        std.debug.print("gpa deinit status = {}\n", .{deinit_status});
    }

    const source = try std.fs.cwd().readFileAlloc(al, "test.clop", 1337420);
    defer al.free(source);
    var lexer = Lexer.init(source);
    while (!lexer.finished()) {
        const tok = lexer.nextToken();
        std.debug.print("{}\n", .{tok.type});
    }

    // var what = std.ArrayList(u8).init(al);
    // defer what.deinit();
    // try what.append('m');
    // defer what.deinit();
}
