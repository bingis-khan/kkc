const std = @import("std");
const ast = @import("ast.zig");
const Scope = ast.Scope;
const common = @import("common.zig");
const Str = common.Str;

predefinedTypes: [NumPredefinedTypes]*ast.Data,
predefinedClasses: [NumPredefinedClasses]*ast.Class,

const Self = @This();

pub const PremadeType = enum {
    Unit,
    Bool,
    Int,
    ConstStr,
    Char,
    Ptr,
    StrConcat,
    ListSpread,
};
pub const NumPredefinedTypes = NumEnums(PremadeType);

// later should be defined in prelude?
pub fn defined(self: *const Self, premade: PremadeType) *ast.Data {
    return self.predefinedTypes[@intFromEnum(premade)];
}

// I don't know how to easily extract field names from an enum value.
pub const PremadeTypeName = TypeNameArray(PremadeType);

pub const PremadeClass = enum {
    Eq,

    FromChar,
    ListLike,
    // MapList,
};

pub const NumPredefinedClasses = NumEnums(PremadeClass);

pub const PremadeClassName = TypeNameArray(PremadeClass);

pub fn definedClass(self: *const Self, premade: PremadeClass) *ast.Class {
    return self.predefinedClasses[@intFromEnum(premade)];
}

// generic stuff
fn NumEnums(t: type) comptime_int {
    return @typeInfo(t).Enum.fields.len;
}

fn TypeNameArray(t: type) std.EnumArray(t, Str) {
    var typenames = std.EnumArray(t, Str).initUndefined();
    for (@typeInfo(t).Enum.fields) |enumField| {
        typenames.set(@enumFromInt(enumField.value), enumField.name);
    }
    return typenames;
}
