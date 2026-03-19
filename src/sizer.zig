// module which sizes structs like C to be compatible.
const std = @import("std");
const ast = @import("ast.zig");
const TypeContext = @import("TypeContext.zig");
const common = @import("common.zig");
const Str = common.Str;
const Prelude = @import("Prelude.zig");

// represents size of a struct.
pub const Size = struct {
    size: usize = 0,
    alignment: usize = 1,

    // calculate offset of each member.
    pub fn add(og: *@This(), new: @This()) usize {
        const padding = calculatePadding(og.size, new.alignment);
        const off = og.size + padding;
        og.size += padding + new.size;
        og.alignment = @max(og.alignment, new.alignment);
        return off;
    }

    // finish making the struct to get its final size.
    pub fn finish(self: *const @This()) @This() {
        var s: @This() = self.*;
        s.size += calculatePadding(s.size, s.alignment);
        return s;
    }

    pub fn union_(m: *@This(), sz: @This()) void {
        if (sz.size > m.size) {
            m.size = sz.size;
        }

        // with unions, both are split. imagine union of 13 chars and one long.
        // it'll be aligned to 8, so size 16
        // (i tested it, it works like that)
        if (sz.alignment > m.alignment) {
            m.alignment = sz.alignment;
        }
    }

    // common
    pub const tag = @This(){
        .size = @sizeOf(Tag),
        .alignment = @alignOf(Tag),
    };

    pub const ptr = @This(){
        .size = @sizeOf(*anyopaque),
        .alignment = @alignOf(*anyopaque),
    };
};

fn calculatePadding(cur: usize, alignment: usize) usize {
    const padding = alignment - (cur % alignment);
    if (padding == alignment) return 0;
    return padding;
}

pub const Tag = u32;

pub const TypeSize = struct {
    tyc: *const TypeContext,
    match: *const ast.Match,
    prelude: *const Prelude,

    const Self = @This();
    // calculates total size of the record (including tag)
    //  size includes alignment!
    //  VERY SLOW, BECAUSE IT RECALCULATES ALIGNMENT EACH TIME.
    //  BUG: works for Ints only accidentally, since i64 and ptr have the same size. FIXIT!
    pub fn sizeOf(self: *Self, t: ast.Type) Size {
        switch (self.tyc.getType(t)) {
            .Anon => |fields| {
                return self.sizeOfRecord(fields);
            },
            .Con => |c| {
                // before all that check for 'bytes' annotation.
                if (ast.Annotation.find(c.type.annotations, "bytes")) |ann| {
                    const sz = std.fmt.parseInt(usize, ann.params[0], 10) catch unreachable; // TODO: USER ERROR
                    return .{
                        .size = sz,
                        .alignment = sz,
                    };
                }

                // const oldTyMap = self.tymap;
                // // FIXES INFINITE LOOP for functions that operate on datatypes which have outer tvars.
                // // VERY HACKY.
                // // basically, when we're in a function which defines the tvars, the c.outerApplication has the tvar itself as its value
                // // so if we don't get the value of the tvar first, we get an infinite loop.
                // // maybe there is a better way which does not require allocation?
                // const outerApplication = self.arena.alloc(ast.TypeOrNum, c.outerApplication.len) catch unreachable; // TEMP
                // for (0..outerApplication.len) |i| {
                //     const app = c.outerApplication[i];
                //     switch (app) {
                //         .Type => |appt| {
                //             outerApplication[i] = .{
                //                 .Type = switch (self.typeContext.getType(appt)) {
                //                     .TVar => |tv| oldTyMap.getTVar(tv) orelse appt, // very bad!! xddd
                //                     else => appt,
                //                 },
                //             };
                //         },
                //         .Num => |appnum| {
                //             outerApplication[i] = .{
                //                 .Num = switch (self.typeContext.getNum(appnum)) {
                //                     .TNum => |tnum| oldTyMap.getTNum(tnum) orelse appnum,
                //                     else => appnum,
                //                 },
                //             };
                //         },
                //     }
                // }
                // const outerTVScheme = ast.Scheme{
                //     .tvars = c.type.outerTVars,
                //     .envVars = &.{},
                //     .associations = &.{},
                //     .env = null,
                // };
                // const outerTVMatch = ast.Match{
                //     .tvars = outerApplication,
                //     .envVars = &.{},
                //     .assocs = &.{},
                //     .scheme = outerTVScheme,
                // };
                // const outerTVMap = TypeMap{
                //     .prev = oldTyMap,
                //     .scheme = &outerTVScheme,
                //     .match = &outerTVMatch,
                // };
                // const tymap = TypeMap{
                //     .prev = &outerTVMap,
                //     .scheme = &c.type.scheme,
                //     .match = c.application,
                // };
                // self.tymap = &tymap;
                // defer self.tymap = oldTyMap;

                // check if ptr
                if (c.type.eq(self.prelude.defined(.Ptr))) {
                    return Size.ptr;
                }

                // do the match thing
                // TODO: incomplete, also do the outer vars.
                const oldMatch = self.match;
                defer self.match = oldMatch;
                self.match = c.application;

                // check if array
                if (c.type.eq(self.prelude.defined(.Array))) {
                    // TODO: refactor
                    const count: usize = b: {
                        const numref = c.application.tvars[0].Num;
                        while (true) {
                            switch (self.tyc.getNum(numref)) {
                                // .TNum => |tnum| numref = self.tymap.getTNum(tnum) orelse unreachable,
                                .TNum => unreachable,
                                .Literal => |lit| break :b @intCast(lit),
                                .Unknown => unreachable,
                            }
                        }
                    };
                    const ty = c.application.tvars[1].Type;
                    const sz = self.sizeOf(ty);

                    // NOTE: padding
                    // Then I compiler a ThreeChar struct in C, a 5 element 3 char array has 15 bytes, which means no padding between elements.
                    // Based on this, the algorithm seems correct.

                    return .{ .size = sz.size * count, .alignment = sz.alignment };
                }

                switch (c.type.structureType()) {
                    // NOTE: not sure if it's correct, but assume pointer size, because that's what opaque types mostly are. I guess I should also use some annotations to check size.
                    //  I wonder if I should make sizes in annotations OR will the compiler just *know* about inbuilt types?
                    .Opaque => return Size.ptr,
                    // ERROR: this is not correct for ints, so watch out.
                    //  I should be able to specify expected datatype size.
                    .EnumLike => {
                        return Size.tag;
                    },
                    .RecordLike => {
                        return switch (c.type.stuff) {
                            .cons => |cons| self.sizeOfCon(&cons[0], Size{}),
                            .recs => |recs| self.sizeOfRecord(recs),
                        };
                    },
                    .ADT => {
                        var tagged = Size{};
                        _ = tagged.add(Size.tag);

                        var max: ?Size = null;
                        for (c.type.stuff.cons) |*con| {
                            const sz = self.sizeOfCon(con, tagged);
                            if (max) |*m| {
                                m.union_(sz);
                            } else {
                                max = sz;
                            }
                        }

                        // make sure to realign it again.
                        return max.?.finish();
                    },
                }
            },
            .TVar => unreachable, // |tv| return self.sizeOf(self.tymap.getTVar(tv)),

            .Fun => {
                // return .{
                //     .size = @sizeOf(*RawValue.Fun),
                //     .alignment = @alignOf(*RawValue.Fun),
                // };
                return Size.ptr;
            },
            .TyVar => unreachable, // actual error. should not happen!
        }
    }

    fn sizeOfCon(self: *Self, con: *const ast.Con, begin: Size) Size {
        var size = begin;
        for (con.tys) |ty| {
            _ = size.add(self.sizeOf(ty));
        }

        return size.finish();
    }

    // stupid copy
    fn sizeOfRecord(self: *Self, fields: []ast.TypeF(ast.Type).Field) Size {
        var size = Size{};
        for (fields) |field| {
            const ty = field.t;
            const sz = self.sizeOf(ty);
            _ = size.add(sz);
        }

        return size.finish();
    }

    pub fn getFieldOffsetFromType(self: *Self, t: ast.Type, mem: Str) usize {
        switch (self.tyc.getType(t)) {
            .Anon => |fields| {
                return self.getFieldOffsetFromFields(fields, mem);
            },
            .Con => |con| {
                switch (con.type.stuff) {
                    .recs => |recs| return self.getFieldOffsetFromFields(recs, mem),
                    .cons => unreachable,
                }
            },

            else => unreachable,
        }
    }

    pub fn getFieldOffsetFromFields(self: *Self, fields: []ast.Record, mem: Str) usize {
        var size = Size{};
        for (fields) |field| {
            const sz = self.sizeOf(field.t);
            const off = size.add(sz);
            if (common.streq(field.field, mem)) {
                return off;
            }
        } else {
            unreachable;
        }
    }
};
