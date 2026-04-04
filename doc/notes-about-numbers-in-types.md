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


```
SomeType ^count a
	SomeArray (Array count a)  # note, that we only specify that a type is a numeric type in the declaration?

Bufsize = 420
fn read-arr (arr Ptr (Array ^count a))  # maybe we should also specify that it's a numeric type for a declaration? or don't bother? we'll get better errors when we misspell `Array`.
	count println()
	Bufsize println()



buf = @undefined as Array 512 Int
buf Array.set(0, 1)
buf Array.get(0) println()
```

Also, a reference of a numeric type (`&^count`) should be treated as if we did `&420`, where it just allocates it on the stack.

Multiple `&BufSize` will produce different pointers!

---

# List Deconstruction

(I've already done wrote about this in KindaC docs)

*geg, i just implemented it using slices. get FUCKED.*
