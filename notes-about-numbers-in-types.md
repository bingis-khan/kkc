```
transpose (Matrix m n) -> Matrix n m
  # ....
```

The problem I got: how do we define type constants?

Since they are types...

```
MyTensorSize = 69
x = ... as Vector MyTensorSize
```

How do we get a type value?

```
MyConst = 420

Term.println(MyConst)  # ??? kinda weird. but might be good.

fn miau (v Vector m)
  # how do we use that `m`?
  Term.println(m)  # ?
  Term.println(@m)  # ?
  Term.println(*m)  # ? might be confusing for retards coming from C
```

How do we mark the numeric type?

```
Vector m
Vector @m

MyConst = 120  # ???
@MyConst = 120  # ???

fn miau (v Vector m)
fn miau (v Vector @m)
fn miau (v Vector *m)
```


---

# List Deconstruction

(I've already done wrote about this in KindaC docs)

