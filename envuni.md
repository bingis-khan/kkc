This will differ slightly to kc in this way: no unions, but the env will be associated with a type (kinda like Rust) and will be passed around. When a function takes in another function, this will also be instantiated.

Similarly, in this function:

add-1-or (fn)
  fun = if random(): fn else: (x: x + 1)
  return fun(1)


This will typecheck, but currently no function can be passed here, because the lambda's environment already already instantiated the thing. We might also implement environment equality, but it's "per need" basis. I don't see a need yet. I don't want to overcomplicate.
