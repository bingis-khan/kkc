// SPDX-License-Identifier: MIT

const builtin = @import("builtin");

const abi = @import("../abi.zig");
const default = @import("../default.zig");
const ffi = @import("../ffi.zig");
const function = @import("../function.zig");

pub const have_complex_type = true;

pub const Abi = if (builtin.target.cpu.arch == .sparc64) enum(i32) {
    v9 = 1,
    _,

    pub const default = abi.default;
} else enum(i32) {
    v8 = 1,
    _,

    pub const default = abi.default;
};

pub const Function = if (builtin.target.cpu.arch == .sparc64) extern struct {
    abi: Abi,
    param_count: c_uint,
    param_types: ?[*]*ffi.Type,
    return_type: *ffi.Type,
    bytes: c_uint,
    flags: c_uint,
    _private1: c_uint,

    pub const prepare = function.prepare;

    pub const prepareVarArgs = function.prepareVarArgs;

    pub const call = function.call;
} else default.Function(Abi);

pub const Closure = default.Closure(Function, if (builtin.target.cpu.arch == .sparc64) 24 else 16);
