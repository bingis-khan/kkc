const std = @import("std");
const Str = @import("common.zig").Str;

ty: Type,
args: u8,

pub const Type = enum {
    undefined,
    cast,
};

const List = [_]Self{
    .{ .ty = .undefined, .args = 0 },
    .{ .ty = .cast, .args = 1 },
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
