const std = @import("std");
pub const Str = []const u8;
pub fn streq(s1: Str, s2: Str) bool {
    return std.mem.eql(u8, s1, s2);
}
pub const Location = struct {
    from: usize,
    to: usize,
    source: Str, // store this, because errors can come from diffent files.

    const Self = @This();
    pub fn between(l: *const Self, r: *const Self) Self {
        if (l.source.ptr != r.source.ptr) {
            unreachable;
        }
        return .{
            .from = @min(l.from, r.from),
            .to = @max(l.to, r.to),
            .source = l.source,
        };
    }
};

pub const MaxIndent = 512;

pub fn allocOne(al: std.mem.Allocator, e: anytype) !*@TypeOf(e) {
    const ptr = try al.create(@TypeOf(e));
    ptr.* = e;
    return ptr;
}

pub fn singleElemSlice(T: type, x: *const T) []const T {
    var s: []T = undefined;
    s.len = 1;
    s.ptr = @constCast(@ptrCast(x));
    return s;
}

pub fn byteSlice(p: *anyopaque, size: usize) []const u8 {
    var s: []const u8 = undefined;
    s.len = size;
    s.ptr = @ptrCast(p);
    return s;
}
