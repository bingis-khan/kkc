const std = @import("std");
const Str = @import("common.zig").Str;

ty: Type,
args: u8,

pub const Type = enum {
    undefined,
    cast,
    @"offset-ptr",
    @"size-of",
    argv,
    argc,
    memeq,
    // inteq,
};

const List = [_]Self{
    .{ .ty = .undefined, .args = 0 },
    .{ .ty = .cast, .args = 1 },
    .{ .ty = .@"offset-ptr", .args = 2 },
    .{ .ty = .@"size-of", .args = 1 },
    .{ .ty = .argv, .args = 0 },
    .{ .ty = .argc, .args = 0 },
    .{ .ty = .memeq, .args = 2 },
    // .{ .ty = .inteq, .args = 2 },
};

const Self = @This();

// assumes NO special symbol at the beginning.
pub fn findByName(name: Str) ?Self {
    const values = comptime b: {
        const Tup = struct { Str, Self };
        var tuples: []const Tup = &.{};
        for (List) |entry| {
            tuples = tuples ++ .{.{
                @tagName(entry.ty),
                entry,
            }};
        }

        break :b tuples;
    };
    const finder = comptime std.StaticStringMap(Self).initComptime(values);
    return finder.get(name);
}
