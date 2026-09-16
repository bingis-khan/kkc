// SPDX-License-Identifier: MIT

const builtin = @import("builtin");

const abi = @import("../abi.zig");
const default = @import("../default.zig");

pub const have_complex_type = true;

const arg_longlong = if (builtin.target.cpu.arch == .x86_64) switch (builtin.target.abi) {
    .gnux32, .muslx32 => true,
    else => builtin.target.os.tag == .windows,
} else false;

pub const uarg = if (arg_longlong) c_ulonglong else c_ulong;
pub const sarg = if (arg_longlong) c_longlong else c_long;

pub const Abi = if (builtin.target.os.tag == .windows)
    if (builtin.target.cpu.arch == .x86_64) enum(i32) {
        win64 = 1,
        gnuw64 = 2,
        _,

        pub const default = abi.default;
    } else enum(i32) {
        sysv = 1,
        stdcall = 2,
        thiscall = 3,
        fastcall = 4,
        cdecl = 5,
        pascal = 6,
        register = 7,
        _,

        pub const default = abi.default;
    }
else if (builtin.target.cpu.arch == .x86_64) enum(i32) {
    unix64 = 2,
    win64 = 3,
    gnuw64 = 4,
    _,

    pub const default = abi.default;
} else enum(i32) {
    sysv = 1,
    thiscall = 3,
    fastcall = 4,
    stdcall = 5,
    pascal = 6,
    register = 7,
    cdecl = 8,
    _,

    pub const default = abi.default;
};

pub const Function = default.Function(Abi);

pub const Closure = default.Closure(Function, if (builtin.target.cpu.arch == .x86_64) 32 else 16);
