pub fn Fixed(comptime t: type, comptime sz: usize) type {
    return struct {
        mem: [sz]t,
        current: usize,

        const Self = @This();

        pub fn init() Self {
            return .{
                .mem = undefined,
                .current = 0,
            };
        }

        // TODO: add overflow checking  and errors (but it should not happen when the code is correct, so I'm not sure)
        pub fn push(s: *Self, item: t) void {
            s.mem[s.current] = item;
            s.current += 1;
        }

        pub fn pop(s: *Self) t {
            s.current -= 1;
            return s.mem[s.current];
        }

        pub fn top(s: *const Self) t {
            return s.mem[s.current - 1];
        }

        pub fn peek(s: *const Self) t {
            return s.mem[s.current - 2];
        }

        pub fn topp(s: *Self) *t {
            return &s.mem[s.current - 1];
        }

        // reversed iterators
        const ReverseIterator = struct {
            og: *Self,
            cur: usize,

            pub fn next(self: *@This()) ?t {
                if (self.nextPtr()) |p| {
                    return p.*;
                } else {
                    return null;
                }
            }

            pub fn nextPtr(self: *@This()) ?*t {
                if (self.cur == 0) return null;

                self.cur -= 1;
                return &self.og.mem[self.cur];
            }
        };

        pub fn iterateFromTop(s: *Self) ReverseIterator {
            return .{
                .og = s,
                .cur = s.current,
            };
        }
    };
}
