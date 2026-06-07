https://gitlab.com/zsaleeba/picoc/-/blob/master/interpreter.h?ref_type=heads

kinda found how we can handle structs.
the problems I have is how tf should I handle values in general, when to pass as a Value, when to allocate on stack, etc.

---

# Value Copying

i think i lost the og interpreter "spec" where I describe how/when values should be copied.

I guess the new spec is:

- LValue: kept as a reference. we don't care where it's stored.
- Smol: small enough that it's not stored anywhere... (<= 8 bytes) except functions....(because they need to store their environment; TODO). No need to copy when changing.
- Stack: stored on the "stack" - ideally some cheap allocator to manage alignment. No need to copy when changing.

---

by stack, i mean some sort of allocator (which can be a stack!).
also can be an "expression stack", meaning it's only per expression!

- return values: copy to outer function's stack
- variables: associate variable names with Value + Memory.
  - currently, scopes are per function (not block). 
  - this means that all variables share a lifetime with the whole function. meaning that *when we exit the function, free variables*.
- functions: ???? (TODO; they can escape. use global arena for now.)
- env initializing (TODO; same problem.)

one problem: when returing a value, it might still need to be on the stack, at least until the statement is finished.
  ```zig
    // cond and any of its data needs to be kept...
    const cond = try self.expr(ifs.cond);

    if (isTrue(cond)) {  // ...until here.
        try self.stmts(ifs.bTrue);
    } else {
      // ...
  ```
 - we might initialize the stack at the beginning of the statement and free it at the end. however, in terms of more "complex" statemments like loops or if statements, it will keep unnecessary data on the stack still.
 - (in this case its not needed, because the boolean value is considered Smol, but this illustrates the general example)

---

# C struct compatibility

notice, that we don't correctly handle functions in structs yet. For now, I'm not planning on making struct sizes with functions in 'em compatible.
In fact, down the line, I want to drop that compatibility in favor of explicitly marking them `extern`. That's because in extern datatypes we should explicitly "mark" the environment (or its absence) of functions in structs.

IDEA: Maybe when we drop that compatibility and make structs not binary compatible by default, then we will correctly deallocate functions. (since we will be able to easily inspect them(? we can already do that doe)).

---

# result of "first rewrite"

super slow. i've added a switch, so it's not that slow. at least, normal expressions should not leak memory now. (right now, only EnvSnapshots put stuff in a global arena, because I still haven't figured out how should I do the quick value passing)

a quick summary:
- each statement creates a new expression stack and frees it at the end (this is done with ArenaAllocators)
- also each scope (which, by scope, currently it means function scope) also creates its own ArenaAllocator, which gets freed at the end.
- new handling for values: LValues and Owned (Smol and Stack), Smol is contained in the value itself, Stack is a reference to the stack (currently allocated by the stack arena allocator).
- values contain size to not recalculate it constantly.;
- ArenaAlloc option, which enables the previous allocation strategy (and disables deallocations)
  - before: ~200ms on reminda
  - now (ArenaAlloc: off): ~1400ms(?!?!?!?!?!)
  - now (ArenaAlloc: on): ~230ms (overall 30ms slower...)

- result: i really gotta open a profiler now...
- one good thing? we have a Value abstraction and a notion of ownership which I can change if need be (without modifying the calling code too much).

