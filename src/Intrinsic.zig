const std = @import("std");
const Str = @import("common.zig").Str;

ty: Type,
args: u8,

pub const Type: type = addNumericEnums(enum {
    undefined,
    cast,
    @"offset-ptr",
    @"size-of",
    argv,
    argc,
    memeq,
    errno,

    @"i64-f64",
    @"f64-i64-floor",
});

const List = [_]Self{
    .{ .ty = .undefined, .args = 0 },
    .{ .ty = .cast, .args = 1 },
    .{ .ty = .@"offset-ptr", .args = 2 },
    .{ .ty = .@"size-of", .args = 1 },
    .{ .ty = .argv, .args = 0 },
    .{ .ty = .argc, .args = 0 },
    .{ .ty = .memeq, .args = 2 },
    .{ .ty = .errno, .args = 0 },
    .{ .ty = .@"i64-f64", .args = 1 },
    .{ .ty = .@"f64-i64-floor", .args = 1 },
} ++ addNumericDecls();

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

const NumberTypes = [_][]const u8{ "i64", "f64" };
const NumberOps = [_][]const u8{ "add", "sub", "mul", "div" };

// https://kihlander.net/post/extending-an-enum-in-zig/
fn addNumericEnums(t: type) type {
    const enumType = @typeInfo(t).Enum;

    const EnumField = std.builtin.Type.EnumField;
    var numberOps: []const EnumField = &.{};
    inline for (NumberTypes) |numty| {
        inline for (NumberOps) |op| {
            numberOps = numberOps ++ .{EnumField{
                .name = numty ++ "-" ++ op,
                .value = enumType.fields.len + numberOps.len,
            }};
        }
    }

    const enumInfo = std.builtin.Type.Enum{
        .tag_type = u8,
        .fields = enumType.fields ++ numberOps,
        .decls = &[0]std.builtin.Type.Declaration{},
        .is_exhaustive = true,
    };

    return @Type(std.builtin.Type{ .Enum = enumInfo });
}

fn addNumericDecls() []const Self {
    var decls: []const Self = &.{};
    inline for (NumberTypes) |numty| {
        inline for (NumberOps) |op| {
            const opname = numty ++ "-" ++ op;
            const field = @field(Type, opname);
            decls = decls ++ .{Self{
                .ty = field,
                .args = 2,
            }};
        }
    }
    return decls;
}
