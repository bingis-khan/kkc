const std = @import("std");
const Common = @import("common.zig");
const Str = Common.Str;
const Loc = Common.Location;
const ast = @import("ast.zig");
const Module = @import("Module.zig");
const ModuleInfo = Common.ModuleInfo;
const token = @import("token.zig");
const Token = token.Token;
const TokenType = token.TokenType;
const parser = @import("parser.zig");

pub const Error = union(enum) {
    IncorrectIndent: struct {},

    UnexpectedToken: struct {
        got: Token,
        expected: TokenType,
    },

    UnexpectedThing: struct {
        expected: Str,
        at: Token,
    },

    UndefinedVariable: struct {
        varname: ast.Var,
        loc: Loc,
    },

    UndefinedType: struct {
        typename: Str,
        loc: Loc,
    },

    UndefinedCon: struct {
        conname: Str,
        loc: Loc,
    },

    UndefinedTVar: struct {
        tvname: Str,
        loc: Loc,
    },

    UndefinedClass: struct {
        className: Str,
    },

    UndefinedIntrinsic: struct {
        name: Str,
        loc: Loc,
    },

    MismatchingTypes: struct {
        lfull: ast.Type,
        lt: ast.Type,
        lpos: Loc,
        rfull: ?ast.Type,
        rt: ast.Type,
        rpos: ?Loc,
    },

    MismatchingEnv: struct {
        le: ast.Env,
        lpos: Loc,
        re: ast.Env,
        rpos: ?Loc,
    },

    MismatchingParamLen: struct {
        lpl: usize,
        lloc: Loc,
        rpl: usize,
        rloc: ?Loc,
    },

    MismatchingKind: struct {
        data: *ast.Data,
        expect: usize,
        actual: usize,
    },

    RecursiveType: struct {
        tyv: ast.TyVar,
        in: ast.Type,
    },

    TuplesNotYetSupported: struct {},

    CannotDirectlyMutateVarFromEnv: struct {},

    TryingToMutateNonVar: struct {},

    CircularModuleReference: struct {},

    UnimportedModule: struct {},

    ModuleDoesNotExportThing: struct {},

    DataDoesNotExportThing: struct {},
    ClassDoesNotExportThing: struct {},

    CouldNotFindInstanceForType: struct {
        data: *ast.Data,
        class: *ast.Class,
        possibilities: Module.DataInstance,
        loc: Loc,
    },

    ConstraintsLeft: []parser.Association,

    TVarDoesNotImplementClass: struct {
        tv: ast.TVar,
        class: *ast.Class,
    },

    ConstrainedNonExistentTVar: struct {
        tvname: Str,
    },

    // TODO: make it a warning (unlike ziggers)
    UnreachableCode: struct {}, // do something cool: take only lines and show:
    // unreachable code
    //     return 69  <- from here
    // --- Term.println('owo')
    // |   if miau
    // |     do-sth()
    // |   else
    // ---   dupa()

    MissingReturn: struct {},

    RecordsAndConstructorsPresent: struct {},

    TypeDoesNotHaveField: struct {
        t: ast.Type,
        field: Str,
        loc: Loc,
    },

    TypeIsNotARecord: struct {
        t: ast.Type,
        field: Str,
    },

    DataIsNotARecord: struct {
        data: *ast.Data,
    },

    DidNotDefineField: struct {
        field: Str,
        loc: Loc,
    },

    DuplicateField: struct {
        field: Str,
    },

    fn p(comptime fmt: anytype, args: anytype) void {
        std.debug.print(fmt ++ "\n", args);
    }

    pub const ErrCtx = struct {
        module: ModuleInfo,
        c: ast.Ctx,

        pub fn atLocation(self: *const @This(), loc: Loc, labels: anytype) void {
            errorAtLocation(self.module, self.c, loc, labels);
        }
    };
    pub fn print(self: @This(), c: ast.Ctx, module: ModuleInfo) void {
        const err = ErrCtx{
            .module = module,
            .c = c,
        };
        switch (self) {
            .IncorrectIndent => p("incorrect indent", .{}),
            .UnexpectedToken => |e| {
                err.atLocation(e.got.toLocation(module.source, module.name), .{
                    .label = .{ "expected ", ast.Ctx.wrap(e.expected), ", but got ", ast.Ctx.wrap(e.got.type) },
                });
                // p("expected {}, but got {}", .{ e.expected, e.got });
            },
            .UnexpectedThing => |e| p("expect {s} at {}", .{ e.expected, e.at }),
            .UndefinedVariable => |uv| {
                err.atLocation(uv.loc, .{
                    .label = .{ "undefined variable ", uv.varname.name },
                });
                // p("undefined variable {s}{} at ({}, {})", .{ uv.varname.name, uv.varname.uid, uv.loc.from, uv.loc.to });
            },
            .UndefinedCon => |e| p("undefined con {s} at ({}, {})", .{ e.conname, e.loc.from, e.loc.to }),
            .UndefinedType => |e| p("undefined type {s} at ({}, {})", .{ e.typename, e.loc.from, e.loc.to }),
            .UndefinedTVar => |e| p("undefined tvar {s} at ({}, {})", .{ e.tvname, e.loc.from, e.loc.to }),
            .UndefinedIntrinsic => |e| {
                err.atLocation(e.loc, .{ .label = .{"undefined intrinsic"} });
            },
            .RecursiveType => |e| {
                c.print(.{ "tried to unify tyvar ", e.tyv, ", which is in type ", e.in, "\n" });
            },
            .MismatchingTypes => |e| {
                if (e.rpos) |rpos| {
                    err.atLocation(e.lpos, .{ .label = .{ "expected type ", e.lt, ast.Ctx.onlyIf(!e.lt.eq(e.lfull), .{ " (", e.lfull, ")" }) } });
                    err.atLocation(rpos, .{ .label = .{ "but got ", e.rt, ast.Ctx.onlyIf(e.rfull != null and !e.rt.eq(e.rfull.?), .{ " (", e.rfull.?, ")" }) } });
                } else {
                    err.atLocation(e.lpos, .{ .label = .{
                        "expected type ",
                        e.lt,
                        ast.Ctx.onlyIf(!e.lt.eq(e.lfull), .{ " (", e.lfull, ")" }),
                        ", but got ",
                        e.rt,
                        ast.Ctx.onlyIf(e.rfull != null and !e.rt.eq(e.rfull.?), .{ " (", e.rfull.?, ")" }),
                    } });
                }
            },
            .MismatchingEnv => |e| {
                c.s("Mismatching envs: ");
                c.encloseSepBy(e.le, ", ", "[", "]"); // UGLY
                c.s(" =/= ");
                c.encloseSepBy(e.re, ", ", "[", "]");
                p("", .{}); // newline
            },
            .MismatchingParamLen => |e| {
                if (e.rloc) |rloc| {
                    err.atLocation(e.lloc, .{ .label = .{ "mismatching param lengths. expected length of ", e.lpl } });
                    err.atLocation(rloc, .{ .label = .{ "but got length of ", e.rpl } });
                } else {
                    err.atLocation(e.lloc, .{ .label = .{ "mismatching param lengths. expected length of ", e.lpl, ", but got ", e.rpl } });
                }
                // p("Mismatching lengths: {} =/= {}", .{ e.lpl, e.rpl });
            },
            .MismatchingKind => |e| p("Mismatching kind for {s}: expect {}, but got {}", .{ e.data.name, e.expect, e.actual }),
            .TuplesNotYetSupported => p("Tuples not yet supported!", .{}),

            .CannotDirectlyMutateVarFromEnv => p("cannot directly mutate a var from outer scope", .{}),
            .TryingToMutateNonVar => p("trying to mutate non var", .{}),
            .CircularModuleReference => p("circular module reference", .{}),

            .UnimportedModule => p("unimported module", .{}),
            .ModuleDoesNotExportThing => p("ModuleDoesNotExportThing", .{}),

            .DataDoesNotExportThing => p("DataDoesNotExportThing", .{}),
            .ClassDoesNotExportThing => p("ClassDoesNotExportThing", .{}),
            .CouldNotFindInstanceForType => |e| {
                err.atLocation(e.loc, .{
                    .label = .{ "Could not find instance of ", e.class, " for type ", e.data, ". Possible instances: ", ast.Ctx.iter(e.possibilities.iterator(), ", ") },
                });
            },
            .ConstraintsLeft => |e| {
                p("{s}: constraints left {}:", .{ module.name, e.len });
                for (e) |constr| {
                    err.atLocation(constr.loc.?, .{ .label = .{
                        constr.from,
                        " => ",
                        constr.to,
                        " for class fun ",
                        constr.classFun,
                        "(",
                        constr.classFun.class,
                        ")",
                    } });
                }
            },
            .TVarDoesNotImplementClass => |e| p("tvar {s} does not implement class {s}", .{ e.tv.name, e.class.name }),
            .ConstrainedNonExistentTVar => |e| p("constrained non existent tvar {s}", .{e.tvname}),
            .UnreachableCode => p("unreachable code", .{}),
            .MissingReturn => p("missing return", .{}),
            .RecordsAndConstructorsPresent => p("records and constructors present", .{}),
            .TypeDoesNotHaveField => |e| {
                err.atLocation(e.loc, .{ .label = .{ "type ", e.t, " does not have field '", e.field, "'" } });
            },
            .TypeIsNotARecord => |e| {
                c.print(.{ "type ", e.t, " is not a record, so it cannot have a field ", e.field, "\n" });
            },
            .DataIsNotARecord => |e| c.print(.{ "data ", e.data, " is not a record\n" }),
            .DidNotDefineField => |e| {
                err.atLocation(e.loc, .{ .label = .{ "you forgot to define field '", e.field, "', bruh" } });
            },
            .DuplicateField => |e| c.print(.{ "duplicate field '", e.field, "'\n" }),
            .UndefinedClass => |e| c.print(.{ "undefined class ", e.className, "\n" }),
        }
    }
};

// const Formatted = struct { fmt: com, args: anytype };

pub const Errors = std.ArrayList(struct { module: ModuleInfo, err: Error });
fn errorAtLocation(module: ModuleInfo, c: ast.Ctx, loc: Loc, labels: anytype) void {
    const label = @field(labels, "label");
    c.print(.{ module.name, ": " });
    c.print(label);
    c.print("\n");

    // I actually don't know when it can happen, so TODO
    if (loc.module.source[loc.from] == '\n') {
        unreachable;
    }

    var lineBeginIndex = loc.from;
    while (true) {
        if (lineBeginIndex == 0) break;
        if (loc.module.source[lineBeginIndex] == '\n') {
            lineBeginIndex += 1;
            break;
        }
        lineBeginIndex -= 1;
    }

    var tabsBeforeToken: u32 = 0;
    for (lineBeginIndex..loc.from) |i| {
        if (loc.module.source[i] == '\t') {
            tabsBeforeToken += 1;
        }
    }

    var lineEndIndex = loc.to;
    while (true) {
        if (lineEndIndex == loc.module.source.len or loc.module.source[lineEndIndex] == '\n') {
            break;
        }

        lineEndIndex += 1;
    }

    var linesInBetween: u32 = 0;
    for (loc.from..loc.to) |i| {
        if (loc.module.source[i] == '\n') linesInBetween += 1;
    }

    if (linesInBetween == 0) {
        c.sp(" {} | {s}\n", .{ loc.line, loc.module.source[lineBeginIndex..lineEndIndex] });

        // PRINT THE UNDERLINE
        // TEMP: with normal stdio, just count the number once.
        var lineLengthMeasure = std.io.countingWriter(std.io.null_writer);
        var measureWriter = lineLengthMeasure.writer();
        measureWriter.print("{}", .{loc.line}) catch {};
        for (0..lineLengthMeasure.bytes_written + 1) |_| {
            c.print(" "); // pad
        }

        // pad to match the length of the number.
        c.print(" | ");

        for (0..tabsBeforeToken) |_| {
            c.print("\t");
        }

        const lineChars = loc.from - lineBeginIndex;
        for (0..lineChars - tabsBeforeToken) |_| {
            c.print(" ");
        }

        for (loc.from..loc.to) |_| {
            c.print("^");
        }
        c.print("\n");
    } else {
        unreachable; // TODO
    }

    if (@hasField(@TypeOf(labels), "footnote")) {
        const footFmt = @field(label, "fmt");
        _ = footFmt; // autofix
        const footArgs = @field(label, "args");
        _ = footArgs; // autofix
    }
}
