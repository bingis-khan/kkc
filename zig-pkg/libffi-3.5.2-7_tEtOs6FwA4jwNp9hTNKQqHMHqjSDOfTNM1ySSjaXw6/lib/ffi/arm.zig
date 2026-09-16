// SPDX-License-Identifier: MIT

const builtin = @import("builtin");

const abi = @import("../abi.zig");
const closure = @import("../closure.zig");
const default = @import("../default.zig");
const ffi = @import("../ffi.zig");
const function = @import("../function.zig");

pub const have_complex_type = builtin.target.os.tag != .windows;

pub const Abi = enum(i32) {
    sysv = 1,
    vfp = 2,
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
    _private1: c_int,
    _private2: c_ushort,
    _private3: c_ushort,
    _private4: [16]i8,

    pub const prepare = function.prepare;

    pub const prepareVarArgs = function.prepareVarArgs;

    pub const call = function.call;
};

pub const Closure = if (builtin.target.os.tag.isDarwin()) extern struct {
    trampoline_table: *anyopaque align(8),
    trampoline_table_entry: *anyopaque,
    function: *Function,
    wrapper: *const fn (*Function, *anyopaque, ?[*]*anyopaque, ?*anyopaque) callconv(.c) void,
    datum: ?*anyopaque,

    pub const Code = closure.Code;

    pub const allocCode = closure.allocCode;
} else default.Closure(Function, if (builtin.target.os.tag == .windows) 16 else 12);
