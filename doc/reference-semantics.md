# Reference semantics !!!

(different than you think :OOOOOOOOO)


```
dt = Data(4, 5)
case dt
  Data(x, y)
    n = &x
    n <&= 1 # will dt's first field turn into 1???
```

It should turn into 1. I changed the semantics. I assume that if a reference was taken, it's treated as a reference to the original element. (Basically, we don't have to do Zig's or Rust's *x or &x declaration - we can just take a reference)
