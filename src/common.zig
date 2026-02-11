const std = @import("std");
pub const Str = []const u8;
pub fn streq(s1: Str, s2: Str) bool {
    return std.mem.eql(u8, s1, s2);
}

pub const ModuleInfo = struct {
    source: Str, // store this, because errors can come from diffent files.
    name: Str,
};

pub const Location = struct {
    from: usize,
    to: usize,
    line: usize,
    module: ModuleInfo,

    // TODO: but them together in some sort of "ModuleInfo"

    const Self = @This();
    pub fn between(l: *const Self, mr: anytype) Self {
        const r = b: {
            if (@TypeOf(mr) == *const Self) {
                break :b mr.*;
            } else if (@TypeOf(mr) == Self) {
                break :b mr;
            } else if (@TypeOf(mr) == ?Self) {
                if (mr == null) return l.*;
                break :b mr.?;
            } else {
                @compileError("expect Location");
            }
        };
        if (l.module.source.ptr != r.module.source.ptr) {
            unreachable;
        }
        return .{
            .line = @min(l.line, r.line),
            .from = @min(l.from, r.from),
            .to = @max(l.to, r.to),
            .module = l.module,
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

pub fn bytecopy(dest: *anyopaque, src: *const anyopaque, count: usize) void {
    var sref: []u8 = undefined;
    sref.len = count;
    sref.ptr = @ptrCast(dest);

    @memcpy(sref, @as([*]const u8, @ptrCast(src)));
}

pub fn cloneArrayListWithAllocator(arraylist: anytype, al: std.mem.Allocator) !@TypeOf(arraylist) {
    const ArrayListType = @TypeOf(arraylist);
    var nuArrayList = try ArrayListType.initCapacity(al, arraylist.capacity);
    try nuArrayList.appendSlice(arraylist.items);
    return nuArrayList;
}
