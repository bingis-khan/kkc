
what should the environment even look like here?

```
fn hard-boss (x Int)
	fn soft-boss(y Int)
		if y == 0
			return 0
		else
			println(x)
			return hard-boss(y - 1)

	return soft-boss(x)
```

also, if we allow the above, we can now have mutual recursion without compiler support.

```
fn f1(x)
  fn f3(x)
    if x == 0
      return 1
    else
      return f1(x)
  fn f2(x)
    return f3(x+1)
```

interesting? kinda annoying? YES!
