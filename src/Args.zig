const std = @import("std");
const common = @import("common.zig");
const Str = common.Str;

// Ideally should be in main, but I'm using parsed options from this to modify output.

filename: Str,
printTokens: bool = false,
printAST: bool = false,

pub fn parse(args: std.process.ArgIterator) !@This() {
    var opts = @This(){ .filename = undefined };
    var filename: ?Str = null;
    var argIt = args;
    _ = argIt.skip(); // skip filename
    while (argIt.next()) |arg| {
        if (std.mem.eql(u8, arg[0..2], "--")) {
            const option = std.meta.stringToEnum(ProgramOption, arg[2..]) orelse return error.UnrecognizedOption;

            switch (option) {
                .tokens => opts.printTokens = true,
                .ast => opts.printAST = true,
            }
        } else {
            filename = arg;
        }
    }

    if (filename) |realAssFilename| {
        opts.filename = realAssFilename;
        return opts;
    } else {
        return error.DidNotDefineFilename;
    }
}

// define enum for quick qt parsing
const ProgramOption = enum {
    tokens,
    ast,
};
