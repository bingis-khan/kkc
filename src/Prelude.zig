const std = @import("std");
const ast = @import("ast.zig");
const Scope = ast.Scope;
const common = @import("common.zig");
const Str = common.Str;

predefinedTypes: [NumPredefinedTypes]*ast.Data,

const Self = @This();

pub const PremadeType = enum {
    Unit,
    Bool,
    Int,
    ConstStr,
};
pub const NumPredefinedTypes = @typeInfo(PremadeType).Enum.fields.len;

// later should be defined in prelude?
pub fn defined(self: *const Self, premade: PremadeType) ast.Type {
    _ = self;
    _ = premade;
    // return .{ .id = @intCast(@intFromEnum(premade)) };
    unreachable;
}

// I don't know how to easily extract field names from an enum value.
pub const PremadeTypeName: std.EnumArray(PremadeType, Str) = b: {
    var typenames = std.EnumArray(PremadeType, Str).initUndefined();
    for (@typeInfo(PremadeType).Enum.fields) |enumField| {
        typenames.set(@enumFromInt(enumField.value), enumField.name);
    }
    break :b typenames;
};
