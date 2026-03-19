const std = @import("std");

// TODO: maybe move buffer and set to its own file/namespace, so importing is nicer?

// Context: eql + hash.
// I support contexts, because I want to compare shit by uid.
pub fn Set(comptime K: type, comptime Context: type) type {
    return struct {
        hash: Hash,

        const Hash = std.hash_map.HashMap(K, struct {}, Context, std.hash_map.default_max_load_percentage);
        const Self = @This();
        pub fn init(al: std.mem.Allocator) Self {
            // if we ever have a "stateful" context, make an `initContext` version.
            return .{ .hash = Hash.init(al) };
        }

        pub fn initContext(al: std.mem.Allocator, ctx: Context) Self {
            return .{ .hash = Hash.initContext(al, ctx) };
        }

        // NOTE: I will be using normal/Haskell terminology for simplicity (insert/delete instead of put/remove)
        pub fn insert(self: *Self, x: K) !void {
            try self.hash.put(x, .{});
        }

        pub fn tryInsert(self: *Self, x: K) !bool {
            const gpr = try self.hash.getOrPut(x);
            if (gpr.found_existing) {
                return false;
            }

            gpr.value_ptr.* = .{};
            return true;
        }

        pub fn contains(self: *const Self, e: K) bool {
            return self.hash.contains(e);
        }

        // TODO: change it to remove later (too Haskell brained)
        pub fn delete(self: *Self, x: K) void {
            _ = self.hash.remove(x);
        }

        pub const Iterator = Hash.KeyIterator;
        pub fn iterator(self: *const Self) Iterator {
            return self.hash.keyIterator();
        }

        pub fn empty(self: *const Self) bool {
            return self.hash.count() == 0;
        }

        pub fn difference(self: *Self, other: *const Self) void {
            var it = other.iterator();
            while (it.next()) |k| {
                self.delete(k.*);
            }
        }

        pub fn deinit(self: *Self) void {
            self.hash.deinit();
        }

        pub fn clone(self: *const Self) !Self {
            return .{
                .hash = try self.hash.clone(),
            };
        }
    };
}
