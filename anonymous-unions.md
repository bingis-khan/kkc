# What

Basically, returning errors is annoying when we are dealing with multiple errors.

I can live without it, Rust has the same problem, but it's ... annoying there too - either we make our own error type which encompasses other errors or we use anyhow or something (the anyhow one also allocates iirc).

It would be nice to have anonymous errors for functions, but... i don't like them that much (except for errors) and also they are pretty bad when I have full type inference and tagged unions.

And even worse, it masks errors. Values cannot unify, we don't even know when.


# My proposed solution

Actually, I think there's a way to do it well: only allow anon sums during CONSTRUCTION. explicitly mark an expression as an anonymous sum type, eg:

```
  if !File.exists(name)
    return Left(.FileDoesNotExist)  # Notice the dot - this marks this expr's type as an anonymous union
```

so

```
  return case ...
    C1: Left(.Err1)  # let's mark this type as (Err1 |)
    C2: Left(.Err2)  # this also has a dot, so (Err2 |). Together, they become (Err1 | Err2)
    C3: Left(Err3)   # I forgot a `.` there. We can still unify anon sums and normal types and it works like a normal type unification.
                     # so Err3 = (Err1 | Err2), which fails
    C4: Left(@undefined)  # imagine C3 is not here. missing `.` here also. Then, unification is 'a = (Err1 | Err2), which succeeds, because 'a is a tyvar.

    # do you GET IT??? it's actually pretty cool.
```

How do I unify two error sets?

```
uni(errset, errset')

# or
uni(.errset, .errset') #? ((Err1 | Err2) | (Err3 | Err4)) ? Should we flatten it?

  
```


# Implementation

Actually, there are two implementation problems:

1. we should refrain from "reconstructing" sum types, so each type should get a unique tag! How do we determine these tags? or just globally?
2. how do we PARSE THIS??? I don't want to make another keyword, since as a user, we know what's going on.
