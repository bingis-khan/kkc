# Fake inference

This one typechecks and causes segfault:

```
.fun = try common.allocOne(self.arena, .{
    .fun = fun,
    .env = undefined, // TODO
}),
```

I specify the type of struct to fix it:

```
.fun = try common.allocOne(self.arena, Value.Type.Fun{
    .fun = fun,
    .env = undefined, // TODO
}),
```

Why?????

(note: before doing anything, I should check if this happens in newest Zig)


# Setting breakpoint on return error.CaseNotMatched does not actually trigger it.

Probably incorrect offset?
