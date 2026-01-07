# Charlike

Writing `'o'` creates an implicit `from-charlike(ConstStr) -> a`, with default value being `Char`.


# Eq typeclass is actually PartialEq to compare strings and more

Should I make PartialEq be the default and optionally allow `strict-eq (_, _) -> Bool` function for this stuff?
Or a custom operator like `=!=`? Or `===`? (gegegeg)

Or make a special operator for comparing strings (`streq`) which does that? (since the only real use is strings AND automatic Int -> Double conversions for comparison)

Or simply allow it and make sure the instances are not shit.

We don't need it for matching strings, because we can just make the compiler use `streq` under the hood.

*The problem is also that if only the second argument has a concrete type, nothing will be unified and there will be a bad error or stupid generalization.*

> I want to try the funny `==` one to see how it holds up. If it's annoying (see previous sentences), I'll revert it.

Also, we might as well allow string interpolation.


# Reference variables in `case` statement

eg.

```
  some-int = 420
  case x
    Tuple2(*some-int, value-we-care-about)
      println(value-we-care-about)
```

We want to use `some-int` from outer scope, so we mark it, so that instead of creating a new variable, it compares it.

Alternatively / additionally, maybe we can add tactical `if`s?
```
  some-int = 420
  case x
    Tuple2(l if l == some-int, r) if r == whatever()
      println('miau')
```
