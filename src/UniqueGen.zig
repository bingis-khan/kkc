pub const Unique = u64;
state: u64,

const Self = @This();
pub fn newUnique(self: *Self) u64 {
    const u = self.state;
    self.state += 1;
    return u;
}

pub fn init() Self {
    return .{ .state = 0 };
}
