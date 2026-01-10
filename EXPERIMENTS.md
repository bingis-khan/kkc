I - implemented

# (I) Charlike

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


# (I) Reference variables in `case` statement

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


# (I) IntoIter for prelude's Array type?

copied from `Array.kkc`:

> DESIGN: Be VERY careful with this.
> But it's just too useful to have....
> Maybe I'll make a different type for buffers, so I don't accidentally iterate over them by value?
> Since I want this for ListLike arrays, maybe make the default type be something like SmolArray, which has IntoIter, but default Array/Buffer does not? Since we want to be able to iterate over array literals, but not buffers.

<!-- Right now I just implemented IntoIter for the default Array, but I think I should go with this soon! Update this text when I do. -->

I've decided to implement a `SmolArray` type in prelude for just that. The only difference between it and normal array is that a smol array can be iterated over.

This is useful when using array literals in Makefile deps, which need to be coerced to iterators.
Is this good? Or should I just cave in and just implement an IntoIter for a normal array?

Since it's an experiment, I've decided on the funny option. But I'm thinking I'll remove the SmolArray in the future, and just let people iterate over big big arrays and let em reap what their ignorance has sown.


# Replace `for .. in` loop with a normal `for-each()` + multiline lambda?

That's an interesting idea, but the nice thing about `for` loops is is that I can break out of it easily. Still, it can stay in the back of my head (rent free)

```
  for x in 1 to (10)
    println(x)

  1 to (10) for–each(fn x @)
    println(x)
```

But is a `for .. in` loop that useful? I'm not sure (since we have lambdas and shii). The only advantage is breaking out of it, but I can also make a special function like `for-each-break()` where I can return early or continue or something. I'll see if it's usefule and if not really I might remove it~!

Maybe `for-each-break()` should use default values for structs, so:

```
  1 to (10) for-each-break(fn x @)
    println(x)
    return {}  # we must return it anyway, but at least we have a default value.
```

or with normal datatypes.

```
  1 to (10) for-each-break(fn x @)
    println(x)
    if x == 5
      return Break

    if x == 3
      return Continue  # i guess it's literally like `Noop`.
    return Noop
```

I guess we might also remove the `while` loop and replace it with `loop` and then have `break` statements to emulate?


# `else` clause for loops when it's not broken out of?

I found this pretty nice in zig. Duplicate this behavior? I'll wait until I need it in real code.
