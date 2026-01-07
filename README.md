# kkc

aka. Kinda Kinda C. A worse version of kc (KindaC), but stripped of all the special features.

## Goals

My hope is that this compiler will serve as a testing ground for the standard library for KindaC
and the compiler itself will serve as a reference implementation for the bootstrapped/imperative version of the compiler.

The syntax is the same on purpose, as writing a lexer and tokenizer for this is nontrivial due to its indentation syntax.

Also, I need to figure out the full Scheme/Match parallel (semantics how it works to help later turn this compiler to a proper kc compiler).
rn I'm instantiating envs when mapping types, but I should probably put the envs to instantiate in Scheme/Match, like unions in kc. (this is useful when monomorphising stuff)

BUT, unifying envs is currently scuffed and I need to think more about how it should actually work.


## Features

  - ADTs, typeclasses, lambdas and all that.
  - anonymous records (tbd in KindaC)
  - integers in types and operations (for typed matrix/tensor operations.), incl basic operations (know which?)
  - sensible STD
  - one pass: parsing, resolving, typechecking (that's the goal)


## Todo

- instantiate anonymous structs. right now, they are not instantiated. to properly instantiate them, I would need to add them to scheme (so I'd have to assign an ID to an anon struct)
  - i forgot, did I do it???
- add another namespace for other std modules? (basically, most std modules use the same set of em, so it's kinda annoying to import basic modules.)
- do man or boy test
- auto-import modules? eq. writing `Slice.get(...)` will trigger an import, like a `use Slice` statement. We can still later `use` it if we want to import modules in the namespace.


## Bugs

- [V] release=fast breaks (some incorrect pointer stuff) - dunno why.
  - fixed it when I ironed out value passing in interpreter and valgrinded some undefined behavior.
