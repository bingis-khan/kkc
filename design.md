# Passes and no declaration order.

It's possible to compile kc in a single pass (with an exception of mutually recursive functions, where we need to parse multiple neighbouring functions to check if they mutually recurse), but it's not yet implemented.

---

ACTUALLY, we're remaking kc without unions yo. That's my PLAN!
This is to maybe unclog my mind (because I'm kinda stuck yo, especially on stuff that doesn't YET matter)

---

# dylibbing and linkLibC()

I had to link libc to be able to call dynamically loaded libc functions. That's crap and I hate it. I wonder if it's possible to avoid it.

I assume the problem is that libc has some shit in the process reserved, so I would have to run all libc calls in a different process? And copy data between them.


# Local external functions

external functions can also be polymorphic (easier handling of varargs functions for now.)


id (x a) -> a
  external cprintf(a) -> a
  return cprintf(x)

Right now, I'll only do 1.


# extern structs?

right now, all structs are ABI compatible with C, since I need it for external functions. I don't actually want it for structs that do not interact with external stuff, because I can rearrange struct members for best alignment and reduce redundant nested enums.

This is obviously incorrect when it comes to functions with environments.


# Temporary `fn` keyword

actually, parsing functions vs variables has gotten a bit complicated. For now, I'll introduce a keyword for functions to make parsing easier. I still want to make it like it was before, but I'll have to think about maybe saving lexing state? I want to make this language work as soon as possible, so I'll handle other shit later.
