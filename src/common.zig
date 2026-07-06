const std = @import("std");
pub const Str = []const u8;
pub fn streq(s1: Str, s2: Str) bool {
    return std.mem.eql(u8, s1, s2);
}

// TODO: a character should be a grapheme cluster.
pub fn isSingleCharacter(s: Str) bool {
    const len = std.unicode.utf8ByteSequenceLength(s[0]) catch unreachable; // TODO: what kind of error should I throw? malformed unicode or something?
    if (len > s.len) {
        unreachable; // TODO: also incorrect string thing.
    }

    return len == s.len; // this means it's a single scalar value (we don't yet care about surrogate pairs)
}

pub const ModuleInfo = struct {
    source: Str, // store this, because errors can come from diffent files.
    name: Str,

    pub fn fromFilename(filename: Str) @This() {
        return .{ .source = filename, .name = filename };
    }
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

pub fn byteSlice(p: anytype, size: usize) []u8 {
    var s: []u8 = undefined;
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

//

pub fn id(comptime T: type) fn (T) T {
    return struct {
        fn idfn(x: T) T {
            return x;
        }
    }.idfn;
}

pub fn mapSlice(al: std.mem.Allocator, comptime From: type, comptime To: type, comptime fun: fn (From) To, slice: []From) ![]To {
    const nuslice = try al.alloc(To, slice.len);
    for (0..nuslice.len) |i| {
        nuslice[i] = fun(slice[i]);
    }
    return nuslice;
}

pub fn addToHash(dest: anytype, src: anytype) !void {
    var it = src.iterator();
    while (it.next()) |e| {
        try dest.put(e.key_ptr.*, e.value_ptr.*);
    }
}

pub fn SliceIter(slice: anytype) struct {
    i: usize,
    slice: @TypeOf(slice),

    const Elem = @typeInfo(@TypeOf(slice.ptr)).Pointer.child;

    pub fn next(self: *@This()) ?Elem {
        if (self.i >= self.slice.len) return null;
        const elem = self.slice[self.i];
        self.i += 1;
        return elem;
    }
} {
    return .{ .slice = slice, .i = 0 };
}
