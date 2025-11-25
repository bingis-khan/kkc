const std = @import("std");
const ast = @import("ast.zig");
const common = @import("common.zig");
const Str = common.Str;

predefinedTypes: [NumPredefinedTypes]ast.Type,

const Self = @This();

pub fn init() Self {
    return .{
        .predefinedTypes = .{null} ** NumPredefinedTypes,
    };
}

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
    return .{ .id = @intCast(@intFromEnum(premade)) };
}

// I don't know how to easily extract field names from an enum value.
pub const PremadeTypeName: std.EnumArray(PremadeType, Str) = b: {
    var typenames = std.EnumArray(PremadeType, Str).initUndefined();
    for (@typeInfo(PremadeType).Enum.fields) |enumField| {
        typenames.set(@enumFromInt(enumField.value), enumField.name);
    }
    break :b typenames;
};
