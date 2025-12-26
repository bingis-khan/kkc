const std = @import("std");
const common = @import("common.zig");
const Str = common.Str;

// Ideally should be in main, but I'm using parsed options from this to modify output.

filename: Str,
printTokens: bool = false,
printRootTokens: bool = false,
printAST: bool = false,
printRootAST: bool = false,
printExports: bool = false,
programArgs: []Arg = &.{},

pub const Arg = [*:0]const u8;

pub fn parse(args: std.process.ArgIterator, al: std.mem.Allocator) !@This() {
    var opts = @This(){ .filename = undefined };
    var filename: ?[:0]const u8 = null;
    var progArgs = std.ArrayList(Arg).init(al);
    var argIt = args;
    _ = argIt.skip(); // skip filename
    while (argIt.next()) |arg| {
        if (std.mem.eql(u8, arg[0..2], "--")) {
            // NOTE: the commented out part would be the correct thing to do
            // But I don't feel like writing '--'
            //
            // // check if this is an arg break.
            // if (arg.len == 2) {
            //     // if filename was not defined, I assume the first arg is the filename (for shebang stuff)
            //     if (filename == null) {
            //         const fname = argIt.next() orelse return error.DidNotDefineFilename;
            //         filename = fname;
            //         try progArgs.append(fname); // also make sure that the first arg is the filename.
            //     }

            //     while (argIt.next()) |a| {
            //         try progArgs.append(a);
            //     }
            // }

            const option = std.meta.stringToEnum(ProgramOption, arg[2..]) orelse {
                // if option does not exist, pass it to the proogram
                try progArgs.append(arg);
                continue;
            };

            switch (option) {
                .tokens => opts.printTokens = true,
                .ast => opts.printAST = true,
                .exports => opts.printExports = true,
                .@"!tokens" => opts.printRootTokens = true,
                .@"!ast" => opts.printRootAST = true,
            }
        } else {
            if (filename == null) {
                filename = arg;
            } else {
                // each subsequent option is treated as an argument to the program
                try progArgs.append(arg);
            }
        }
    }

    if (filename) |realAssFilename| {
        opts.filename = realAssFilename;
        try progArgs.insert(0, realAssFilename); // remember to insert the filename at the beginning of program args to match the C stuff.
        opts.programArgs = progArgs.items;
        return opts;
    } else {
        return error.DidNotDefineFilename;
    }
}

// define enum for quick qt parsing
const ProgramOption = enum {
    tokens,
    ast,
    exports,
    @"!tokens",
    @"!ast",
};
