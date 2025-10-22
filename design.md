# Passes and no declaration order.

It's possible to compile kc in a single pass (with an exception of mutually recursive functions, where we need to parse multiple neighbouring functions to check if they mutually recurse), but it's not yet implemented.

Here, I think I want free declaration order. But how many passes would it take then?

- late resolve names...
- generalization?

---

Actually an interesting challenge. I think I can do it in one pass with some funny stuff.

  Keep track of function calls. Store it in some sort of map: `Fn => [](Fn, ([]Type, Type))`. (key is the CALLEE)
  Then, when a function gets typechecked, just solve all of the dependencies.
  Whenether a dependency is solved, check if these are all the dependencies solved for the function. If so, recursively solve it too.
  At the end, if all is solved, then we're finished. If something is left over, signal those as an error, that the function was not defined.
