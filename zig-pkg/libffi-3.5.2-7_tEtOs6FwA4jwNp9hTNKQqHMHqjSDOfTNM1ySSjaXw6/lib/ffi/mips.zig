// SPDX-License-Identifier: MIT

const builtin = @import("builtin");

const abi = @import("../abi.zig");
const default = @import("../default.zig");
const ffi = @import("../ffi.zig");
const function = @import("../function.zig");

pub const have_long_double = switch (builtin.target.cpu.arch) {
    .mips64, .mips64el => switch (builtin.target.os.tag) {
        .freebsd, .linux, .openbsd => true,
        else => false,
    },
    else => false,
};
pub const have_complex_type = true;

const arg_64 = switch (builtin.target.cpu.arch) {
    .mips, .mipsel => false,
    .mips64, .mips64el => true,
    else => unreachable,
};

pub const uarg = if (arg_64) u64 else u32;
pub const sarg = if (arg_64) i64 else i32;

pub const Abi = enum(i32) {
    o32 = 1,
    n32 = 2,
    n64 = 3,
    o32_soft_float = 4,
    n32_soft_float = 5,
    n64_soft_float = 6,
    _,

    pub const default = abi.default;
};

pub const Function = extern struct {
    abi: Abi,
    param_count: c_uint,
    param_types: ?[*]*ffi.Type,
    return_type: *ffi.Type,
    bytes: c_uint,
    flags: c_uint,
    _private1: c_uint,
    _private2: c_uint,

    pub const prepare = function.prepare;

    pub const prepareVarArgs = function.prepareVarArgs;

    pub const call = function.call;
};

pub const Closure = default.Closure(Function, switch (builtin.target.cpu.arch) {
    .mips, .mipsel => 20,
    .mips64, .mips64el => switch (builtin.target.abi) {
        .gnuabin32, .muslabin32 => 20,
        else => 56,
    },
});
