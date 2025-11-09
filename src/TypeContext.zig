const std = @import("std");
const ast = @import("ast.zig");
const UniqueGen = @import("UniqueGen.zig");
const Unique = UniqueGen.Unique;

// Personal Note: bruh, I mean, an allocator for this would basically be the same.
// I guess this is more local.
const TyRef = ast.TyRef;
const TyVar = Unique;
const TyStoreElem = union(enum) {
    Ref: TyVar,
    Type: ast.TypeF(TyRef), // not necessarily good, because it takes a lot of space. Ideally, it would be a second index to an array of actual immutable AST types.
};
const TyStore = std.ArrayList(TyStoreElem);

context: TyStore,
gen: UniqueGen,

const Self = @This();
pub fn init(al: std.mem.Allocator) Self {
    return .{
        .context = TyStore.init(al),
        .gen = UniqueGen.init(),
    };
}

pub fn fresh(self: *Self) !TyRef {
    const tid = self.gen.newUnique();
    return self.addType(.{ .Ref = tid });
}

pub fn newType(self: *Self, t: ast.TypeF(TyRef)) !TyRef {
    return self.addType(.{ .Type = t });
}

fn addType(self: *Self, t: TyStoreElem) !TyRef {
    try self.context.append(t);
    const tid = self.context.items.len - 1;
    return .{ .id = tid };
}
