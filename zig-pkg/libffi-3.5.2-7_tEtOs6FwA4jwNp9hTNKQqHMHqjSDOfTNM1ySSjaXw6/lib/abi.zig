// SPDX-License-Identifier: MIT

const ffi = @import("ffi.zig");

pub fn default() ffi.Abi {
    return @enumFromInt(ffi.ffi_get_default_abi());
}
