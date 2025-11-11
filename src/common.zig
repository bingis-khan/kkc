const std = @import("std");
pub const Str = []const u8;
pub fn streq(s1: Str, s2: Str) bool {
    return std.mem.eql(u8, s1, s2);
}
pub const Location = struct {
    from: usize,
    to: usize,
    source: Str, // store this, because errors can come from diffent files.
};

pub const MaxIndent = 512;

pub fn allocOne(al: std.mem.Allocator, comptime T: type, e: T) !*T {
    const ptr = try al.create(T);
    ptr.* = e;
    return ptr;
}
