// SPDX-License-Identifier: MIT

const builtin = @import("builtin");

const abi = @import("../abi.zig");
const default = @import("../default.zig");
const ffi = @import("../ffi.zig");
const function = @import("../function.zig");

pub const have_long_double = switch (builtin.target.os.tag) {
    .freebsd, .netbsd, .openbsd => builtin.target.cpu.arch == .powerpc,
    .linux => true,
    else => false,
};

pub const Abi = if (builtin.target.os.tag.isDarwin()) enum(i32) {
    aix = 1,
    darwin = 2,
    _,

    pub const default = abi.default;
} else if (builtin.target.cpu.arch.isPowerPC64()) packed struct(i32) {
    linux_align_structs: bool,
    linux_long_double_128: bool,
    linux_long_double_128_ieee: bool,
    linux: bool,
    _pad: i28 = 0,

    pub const default = abi.default;
} else packed struct(i32) {
    sysv_soft_float: bool,
    sysv_return_structs: bool,
    sysv_long_double_128_ibm: bool,
    sysv: bool,
    sysv_long_double_128: bool,
    _pad: i27 = 0,

    pub const default = abi.default;
};

pub const Function = if (!builtin.target.os.tag.isDarwin()) extern struct {
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

pub const Closure = default.Closure(Function, if (builtin.target.cpu.arch == .powerpc64le)
    32
else if (builtin.target.cpu.arch.isPowerPC64())
    if (builtin.target.os.tag.isDarwin()) 48 else 24
else
    40);
