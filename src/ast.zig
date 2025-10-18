pub const AST = struct {
    declarations: []Declaration,
};

pub const Declaration = union(enum) {
    Function: Function,
    Constant: struct { name: Str, value: Expr },
};

pub const Function = struct {
    name: Str,
    params: []struct { pn: Str, pt: ?*Type },
    ret: ?Type,
    body: []*Stmt,
};

pub const Stmt = *union(enum) {
    VarDec: struct { varName: Str, varValue: Expr },
    If: struct {
        cond: Expr,
        bTrue: []Stmt,
        bOthers: [][]Stmt,
        bElse: []Stmt,
    },
    Return: Expr,

    const Rec = *@This();
};

pub const Expr = *union(enum) {
    BinOp: struct { l: Expr, op: Op, r: Expr },
    Var: Str,
};
pub const Op = enum {
    Plus,
    Minus,
    Times,
    Divide,

    Equals,
    NotEquals,
    GreaterThan,
    LessThan,
    GreaterEqualThan,
    LessEqualThan,
};
pub const Type = *union(enum) {
    Con: struct { typename: Str, application: []Type },
    Function: struct { args: []Type, ret: Type },
    TVar: Str,

    const Rec = *@This();
};

// Maybe put this into utils?
const Str = []const u8;
