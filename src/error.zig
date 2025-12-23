const std = @import("std");
const Common = @import("common.zig");
const Str = Common.Str;
const Loc = Common.Location;
const ast = @import("ast.zig");
const Module = @import("Module.zig");

pub const Error = union(enum) {
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

    // TODO: add location information
    MismatchingTypes: struct {
        lt: ast.Type,
        rt: ast.Type,
    },

    MismatchingEnv: struct {
        le: ast.Env,
        re: ast.Env,
    },

    MismatchingParamLen: struct {
        lpl: usize,
        rpl: usize,
    },

    MismatchingKind: struct {
        data: *ast.Data,
        expect: usize,
        actual: usize,
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
        numConstraints: usize,
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
    pub fn print(self: @This(), c: ast.Ctx) void {
        switch (self) {
            .UndefinedVariable => |uv| p("undefined variable {s}{} at ({}, {})", .{ uv.varname.name, uv.varname.uid, uv.loc.from, uv.loc.to }),
            .UndefinedCon => |e| p("undefined con {s} at ({}, {})", .{ e.conname, e.loc.from, e.loc.to }),
            .UndefinedType => |e| p("undefined type {s} at ({}, {})", .{ e.typename, e.loc.from, e.loc.to }),
            .UndefinedTVar => |e| p("undefined tvar {s} at ({}, {})", .{ e.tvname, e.loc.from, e.loc.to }),
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
            .MismatchingParamLen => |e| p("Mismatching lengths: {} =/= {}", .{ e.lpl, e.rpl }),
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
            .ConstraintsLeft => |e| p("num constraints left in module '{s}': {}", .{ e.module, e.numConstraints }),
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

pub const Errors = std.ArrayList(Error);
