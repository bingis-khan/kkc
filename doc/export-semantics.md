# exporting n shii

it's optional! by default, everything defined in the module gets exported.

....

actually, i kinda think visibility is very gay. when we export, should we, by default, export everything defined in the module?
like, it won't be possible to override it?


```
export
  *, Maybe(Just, None), Either(*)
  ListLike, Whatever.
```

also, the syntax should be the same as for `use`.
