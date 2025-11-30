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


## Differences with kc
  - separate type for function-likes. (voldemort lambdas)

  note, that I want kkc code to be a superset of kc.


## Bugs

- release=fast breaks (some incorrect pointer stuff) - dunno why.
