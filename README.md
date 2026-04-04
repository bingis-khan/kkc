# kkc - Kinda Kinda C

A statically-typed procedural language that is a mix of C, Haskell and Python. It compiles via a C backend or can be run directly as a script.

```
reminders = filename
	File.read-contents(al)
	lines()
	filter(fn s: not s is-blank())
	zip(from(1))
```


---

## Main Feature - Environment Handling

The most unique feature of the language is the handling of function environments. This allows for Python/dynamic language-esque function nesting without needing an allocator.

In most languages you either allocate closures on the heap (Haskell, Python) or pass context explicitly (C, Zig) or have a very limited ability to use functions with environments (Rust's `Fn..<>` traits).
In case of typeclasses, languages forbid local instances entirely (Haskell's global-instance coherence requirement, Rust's `impl` blocks).

Here, in kkc, I try to maximize expressiveness without depending on any external system for allocation. Because the type system tracks environments through inference, the compiler always knows the full shape of a function's captured state at compile time.

This makes code like this possible:

```
fn f() -> I32 -> I32
    sumtin = 420
    inst D I32  # instance definition
        d (x I32) -> I32
            return x + sumtin   # captures sumtin

    return d   # function escapes — sumtin travels with it

println(f()(69))   # prints 489
```

I'm maximizing expressiveness but adhering to the constraints of a low level language, theoretically making kkc very portable while not compromising on any of its features.


## Features

- **First class functions** - pass functions and lambdas around without worrying about memory allocation
- **No hidden allocations(TM)** - instead, custom allocators are passed explicitly
- **Single-pass compilation** - already faster than C#'s compiler :)
- **Typeclasses** - ad-hoc polymorphism and local typeclass instances
- **ADTs** - algebraic data types
- **Expressive Pattern Matching** - deconstruct ADTs, records, lists(!); match on integers and strings too.
- **Operator overloading** - add new number types, add indexing (`[]`) operator to custom lists and hash maps.
- **String interpolation** - staticically allocated interpolation: `'\(x) with \(y)'`
- **Anonymous records** - structural record types without named types
- **Integers in types** - type-level integer parameters for static arrays and typed array/matrix dimensions.
- **Postfix calls** - for function chaining, like `map-filter-reduce` style code.
- **A modern standard library** - written in kkc itself from the [ground up](std/Slice.kkc). I want to provide everything from an HTTP server to [graphical libraries](std/Tui.kkc)


## Select Samples

Cartesian product between two iterables:
```
fn cartesian (lit, rit): lit
	map(fn x: rit map(fn y: (x, y)))
	flatten()
```

Part of a makefile-esque script:
```
if not opt('clean') and 'prog' deps (['src'])
	'rebuilding' println()
	'cc -O3 -o prog src/main.c' Sh.run(al)
```

Setting the cells of a screen (with external environment):
```
s chars() zip(from(0)) for-each(fn((c, xx)))
	Slice.set(screen&.current, i + xx i32-size(), Cell { c, fg, bg })
```

---

## Building

Requires [Zig](https://ziglang.org/) 0.13.

```bash
zig build                        # build the compiler → zig-out/bin/kkc

# optionally move it to a PATH reachable place.
./install.sh                     # install to ~/.local/bin/
# Don't forget to set the KKC_STD
```

## Running

```bash
zig build run -- <file.kkc>               # run via interpreter (default)
zig build run -- --backend c <file.kkc>   # compile to native binary via C and run
```

Or with the installed binary:

```bash
kkc <file.kkc>
kkc --backend c <file.kkc>
```

**Environment variable:** `KKC_STD` — path to the standard library. Defaults to `std/` relative to the binary.


## Tests

```bash
zig build test                   # run all tests
zig build test -- <prefix>       # run tests matching a prefix
zig build test -- -f             # run only failing tests
```

Tests live in `test/tests/` as `.kkc` files with embedded expected output.

---


## Standard Library

The stdlib is written in kkc. `prelude.kkc` contains compiler primitives. `converged.kkc` contains default exports.

| Module | Contents |
|--------|----------|
| `Slice` | Slices and basic operations |
| `List` | Arraylist |
| `Str` | Strings |
| `Iter` | Iterators |
| `Array` | Fixed-size arrays |
| `Hash` | Hash map |
| `Math` | Arithmetic utilities |
| `File` | File I/O |
| `Sh` | Shell commands |
| `Mem` | Memory / casting utilities |
| `Alloc` | Allocators |
| `Term` | Printing and ANSI terminal primitives |
| `Tui` | Terminal UI (screen buffer, input, mouse) |

---

## Status

In active development.

- The core of the language is fairly finished.
- 2 complete backends: interpreter and C gen
- I can now write real program in it.


The core is finished, so now I'm polishing up all the rough edges:

- Planned LLVM backend and bytecode
	- Either monomorphised or polymorphic bytecode to speed up execution for faster startup time.
	- Generate LLVM bytecode directly, so I don't have any direct dependence on teh LLVM sloproject.
- Error reporting
	- We don't currently handle multiline errors.
	- Some rarer errors are not handled, so instead the compiler panics.
	- Types in errors can be too large and unreadable.
- Hacky recursion, no mutual recursion.
- Add tests of more common features like multiline lambda or switch-case parsing.
- Interpreter
	- No caching in the interpreter.
	- No freeing memory in the interpreter (should replace with []u8 stack machine)

---

- [todo list](./TODO.md)
- [experiments](./EXPERIMENTS.md)
- [previous compiler written in Haskell](https://github.com/bingis-khan/KindaC)

