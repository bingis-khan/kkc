# Passes and no declaration order.

It's possible to compile kc in a single pass (with an exception of mutually recursive functions, where we need to parse multiple neighbouring functions to check if they mutually recurse), but it's not yet implemented.

---

ACTUALLY, we're remaking kc without unions yo. That's my PLAN!
This is to maybe unclog my mind (because I'm kinda stuck yo, especially on stuff that doesn't YET matter)

---

# Local external functions

external functions can also be polymorphic (easier handling of varargs functions for now.)


id (x a) -> a
  external cprintf(a) -> a
  return cprintf(x)

Right now, I'll only do 1.
