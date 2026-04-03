const ast = @import("ast.zig");
const TypeContext = @import("TypeContext.zig");

pub const TypeMap = struct {
    prev: ?*const @This(),
    scheme: *const ast.Scheme,
    match: *const ast.Match,

    pub const Empty = @This(){
        .prev = null,
        .scheme = &ast.Scheme.Empty,
        .match = &ast.Match.Empty,
    };

    pub fn init(match: *const ast.Match, prev: ?*const @This()) @This() {
        return .{
            .prev = prev,
            .scheme = &match.scheme,
            .match = match,
        };
    }

    pub fn initMap(match: *const ast.Match, tyc: *TypeContext, prev: ?*const @This()) !@This() {
        const mm = if (prev) |tm| try tyc.mapMatch(tm, match) else match;
        return TypeMap.init(mm, prev);
    }

    pub fn mapTVar(self: *const @This(), tv: ast.TVar) ?ast.Type {
        // SLOW
        for (self.scheme.tvars, self.match.tvars) |s, m| {
            switch (s) {
                .TVar => |stv| {
                    if (stv.eq(tv)) {
                        return m.Type;
                    }
                },

                else => {},
            }
        } else {
            return (self.prev orelse return null).mapTVar(tv);
        }
    }

    pub fn mapTNum(self: *const @This(), tnum: ast.TNum) ?ast.NumRef {
        return self.match.mapTNum(tnum) orelse (self.prev orelse return null).mapTNum(tnum);
    }

    pub fn mapEnv(self: *const @This(), base: ast.EnvRef) ?ast.EnvRef {
        return self.match.mapEnv(base) orelse (self.prev orelse return null).mapEnv(base);
        // const be = tc.getEnv(base);
        // if (be.env.*) |env| {
        //     return .{ .base = be.base, .env = env };
        // } else {
        //     var tymap: ?*const @This() = self;
        //     while (tymap) |tm| {
        //         for (tm.scheme.envVars, tm.match.envVars) |sb, mb| {
        //             if (sb.id == be.base.id) {
        //                 return tm.mapEnv(mb, tc);
        //             }
        //         }

        //         tymap = tm.prev;
        //     }

        //     unreachable;
        // }
    }

    pub fn tryGetFunctionByID(self: *const @This(), uid: ast.Association.ID) ?ast.Match.AssocRef.InstPair {
        for (self.scheme.associations, self.match.assocs) |a, r| {
            if (a.uid == uid) {
                return switch (r.?) {
                    .Id => |refuid| return self.tryGetFunctionByID(refuid),
                    .InstFun => |instfun| instfun,
                };
            }
        } else {
            return (self.prev orelse return null).tryGetFunctionByID(uid);
        }
    }
};
