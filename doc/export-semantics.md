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

also, should I maybe put it at the end actually?????

---


all export types:

```
export
  *
  Module.*  # reexport.
  Qualified.Module.*
  Module.SomeTypeOrData
  Module.variable-or-function
  Module.Qualified.{Type, fun}
  Type
  Record  # currently, all fields are public anyway. later we might support NOT importing fields (we can add information about which fields are public when we instantiate a member.)
  Data(Con1, Con2)  # export only two constructors.
  Data(*)  # export all constructors
  Data     # don't export constructors bruh.
  Data()   # export no constructors.
  Class(class-fun)  # what about class functions? should they be exportable like this?
  class-fun  # or like this? or both? and do we even need to export them when we export a class we can later use for writing an instance? should we export the class functions when we write an instance too?
  variable-or-function
```

i think we should also make it possible to write exports on one line:

```
export *, CharPrim.*
```

This should also have the same structure as imports (except the modules part.)

---

also, what about exporting stuff that was shadowed by an external definition? maybe just check each imported thing and check if it's from this scope?
