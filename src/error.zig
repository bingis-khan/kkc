const std = @import("std");
const Common = @import("common.zig");
const Str = Common.Str;
const Loc = Common.Location;
const ast = @import("ast.zig");
const Module = @import("Module.zig");
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

    MismatchingTypes: struct {
        lt: ast.Type,
        lpos: Loc,
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
    },

    ConstraintsLeft: struct {
        module: Str,
        constraints: []parser.Association,
    },

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
    },

    TypeIsNotARecord: struct {
        t: ast.Type,
        field: Str,
    },

    DataIsNotARecord: struct {
        data: *ast.Data,
    },

    MissingField: struct {
        field: Str,
    },

    DuplicateField: struct {
        field: Str,
    },

    fn p(comptime fmt: anytype, args: anytype) void {
        std.debug.print(fmt ++ "\n", args);
    }
    pub fn print(self: @This(), c: ast.Ctx, module: ModuleInfo) void {
        switch (self) {
            .IncorrectIndent => p("incorrect indent", .{}),
            .UnexpectedToken => |e| {
                module.errorAtLocation(e.got.toLocation(module.source), .{ .label = .{
                    .fmt = "expected {}, but got {}",
                    .args = .{ e.expected, e.got.type },
                } });
                // p("expected {}, but got {}", .{ e.expected, e.got });
            },
            .UnexpectedThing => |e| p("expect {s} at {}", .{ e.expected, e.at }),
            .UndefinedVariable => |uv| {
                module.errorAtLocation(uv.loc, .{ .label = .{
                    .fmt = "undefined variable {s}",
                    .args = .{uv.varname.name},
                } });
                // p("undefined variable {s}{} at ({}, {})", .{ uv.varname.name, uv.varname.uid, uv.loc.from, uv.loc.to });
            },
            .UndefinedCon => |e| p("undefined con {s} at ({}, {})", .{ e.conname, e.loc.from, e.loc.to }),
            .UndefinedType => |e| p("undefined type {s} at ({}, {})", .{ e.typename, e.loc.from, e.loc.to }),
            .UndefinedTVar => |e| p("undefined tvar {s} at ({}, {})", .{ e.tvname, e.loc.from, e.loc.to }),
            .RecursiveType => |e| {
                c.print(.{ "tried to unify tyvar ", e.tyv, ", which is in type ", e.in, "\n" });
            },
            .MismatchingTypes => |e| {
                c.s("Mismatching types: ");
                e.lt.print(c); // UGLY
                c.s(" =/= ");
                e.rt.print(c);
                p("", .{}); // newline
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
                    module.errorAtLocation(e.lloc, .{ .label = .{ .fmt = "mismatching param lengths. expected length of {}", .args = .{e.lpl} } });
                    module.errorAtLocation(rloc, .{ .label = .{ .fmt = "but got length of {}", .args = .{e.rpl} } });
                } else {
                    module.errorAtLocation(e.lloc, .{ .label = .{ .fmt = "mismatching param lengths. expected length of {}, but got {}", .args = .{ e.lpl, e.rpl } } });
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
                c.print(.{ "Could not find instance of ", e.class, " for type ", e.data, ". Possible instances: " });
                var it = e.possibilities.iterator();
                while (it.next()) |ee| {
                    ee.key_ptr.*.print(c);
                    c.s(", ");
                }
                // p("could not find instance of class {s} for type {s}", .{ e.class.name, e.data.name })
            },
            .ConstraintsLeft => |e| {
                p("{s}: constraints left {}:", .{ e.module, e.constraints.len });
                for (e.constraints) |constr| {
                    c.print(.{
                        "\t",
                        constr.from,
                        " => ",
                        constr.to,
                        " for class fun ",
                        constr.classFun,
                        "(",
                        constr.classFun.class,
                        ")",
                        "\n",
                    });
                }
            },
            .TVarDoesNotImplementClass => |e| p("tvar {s} does not implement class {s}", .{ e.tv.name, e.class.name }),
            .ConstrainedNonExistentTVar => |e| p("constrained non existent tvar {s}", .{e.tvname}),
            .UnreachableCode => p("unreachable code", .{}),
            .MissingReturn => p("missing return", .{}),
            .RecordsAndConstructorsPresent => p("records and constructors present", .{}),
            .TypeDoesNotHaveField => |e| {
                c.print(.{ "type ", e.t, " does not implement field ", e.field, "\n" });
            },
            .TypeIsNotARecord => |e| {
                c.print(.{ "type ", e.t, " is not a record, so it cannot have a field ", e.field, "\n" });
            },
            .DataIsNotARecord => |e| c.print(.{ "data ", e.data, " is not a record\n" }),
            .MissingField => |e| c.print(.{ "missing field '", e.field, "'\n" }),
            .DuplicateField => |e| c.print(.{ "duplicate field '", e.field, "'\n" }),
            .UndefinedClass => |e| c.print(.{ "undefined class ", e.className, "\n" }),
        }
    }
};

// const Formatted = struct { fmt: com, args: anytype };

pub const Errors = std.ArrayList(struct { module: ModuleInfo, err: Error });
pub const ModuleInfo = struct {
    source: Str,
    name: Str,

    fn errorAtLocation(module: *const @This(), loc: Loc, labels: anytype) void {
        const label = @field(labels, "label");
        const labelFmt = @field(label, "fmt");
        const labelArgs = @field(label, "args");
        std.debug.print("{s}: " ++ labelFmt ++ "\n", .{module.name} ++ labelArgs);

        // I actually don't know when it can happen, so TODO
        if (loc.source[loc.from] == '\n') {
            unreachable;
        }

        var lineBeginIndex = loc.from;
        while (true) {
            if (lineBeginIndex == 0) break;
            if (loc.source[lineBeginIndex] == '\n') {
                lineBeginIndex += 1;
                break;
            }
            lineBeginIndex -= 1;
        }

        var tabsBeforeToken: u32 = 0;
        for (lineBeginIndex..loc.from) |i| {
            if (loc.source[i] == '\t') {
                tabsBeforeToken += 1;
            }
        }

        var lineEndIndex = loc.to;
        while (true) {
            if (lineEndIndex == loc.source.len or loc.source[lineEndIndex] == '\n') {
                break;
            }

            lineEndIndex += 1;
        }

        var linesInBetween: u32 = 0;
        for (loc.from..loc.to) |i| {
            if (loc.source[i] == '\n') linesInBetween += 1;
        }

        if (linesInBetween == 0) {
            std.debug.print(" {} | {s}\n", .{ loc.line, loc.source[lineBeginIndex..lineEndIndex] });

            // PRINT THE UNDERLINE
            // TEMP: with normal stdio, just count the number once.
            var lineLengthMeasure = std.io.countingWriter(std.io.null_writer);
            var measureWriter = lineLengthMeasure.writer();
            measureWriter.print("{}", .{loc.line}) catch {};
            for (0..lineLengthMeasure.bytes_written + 1) |_| {
                std.debug.print(" ", .{}); // pad
            }

            // pad to match the length of the number.
            std.debug.print(" | ", .{});

            for (0..tabsBeforeToken) |_| {
                std.debug.print("\t", .{});
            }

            const lineChars = loc.from - lineBeginIndex;
            for (0..lineChars - tabsBeforeToken) |_| {
                std.debug.print(" ", .{});
            }

            for (loc.from..loc.to) |_| {
                std.debug.print("^", .{});
            }
            std.debug.print("\n", .{});
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
};
