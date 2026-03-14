// NOTE: CURRENTLY UNUSED; LEFT FOR REFERENCE
const Set = @import("Set.zig").Set;
const std = @import("std");

// LIFO; unique values only. saves topmost value.
pub fn StackSet(comptime K: type, comptime Context: type) type {
    return struct {
        unique: Set(K, Context),
        order: std.ArrayList(K),

        const Self = @This();
        pub fn initContext(al: std.mem.Allocator, ctx: Context) Self {
            return .{
                .unique = Set(K, Context).initContext(al, ctx),
                .order = std.ArrayList(K).init(al),
            };
        }

        pub fn push(self: *Self, e: K) !void {
            try self.unique.insert(e);
            try self.order.append(e);
        }

        pub const Iterator = struct {
            state: Self,
            i: usize,

            pub fn next(it: *@This()) ?K {
                while (it.i < it.state.order.items.len) {
                    defer it.i += 1;
                    const e = it.state.order.items[it.i];
                    if (it.state.unique.contains(e)) { // if contains, then it's the first occurence
                        it.state.unique.delete(e);
                        return e;
                    }

                    // set does not contain it, which means it occured before.
                }

                return null;
            }
        };

        pub fn reverseIterator(self: *const Self) !Iterator {
            return .{
                .state = try self.clone(),
                .i = 0,
            };
        }

        fn clone(self: *const Self) !Self {
            return .{
                .unique = try self.unique.clone(),
                .order = try self.order.clone(),
            };
        }
    };
}
