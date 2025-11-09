pub const Str = []const u8;
pub const Location = struct {
    from: usize,
    to: usize,
    source: Str, // store this, because errors can come from diffent files.
};

pub const MaxIndent = 512;
