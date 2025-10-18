# kkc

aka. Kinda Kinda C. A worse version of kc (KindaC), but stripped of all the special features.

My hope is that this compiler will serve as a testing ground for the standard library for KindaC
and the compiler itself will serve as a reference implementation for the bootstrapped/imperative version of the compiler.

---

The syntax is similar on purpose, as writing a lexer and tokenizer for this is nontrivial due to its indentation syntax.


## Features

  - ADTs, typeclasses, lambdas and all that.
  - anonymous record (tbd in KindaC)
  - sensible STD


## Differences with kc

t  - separate type for function-likes.
  - no arbitrary nesting (maybe I'll allow nested functions, but nested will actually act as anonymous lambdas.)
  - arbitrary declaration order!


## Closures?

The big feature of KindaC is that closures and environments are automatically copied and managed statically by the compiler in an expected way.
This one won't do it. So, no arbitrary nesting. All datatypes must be top level (? or maybe datatypes are the only nestable thing?)
