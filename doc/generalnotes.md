notes for general stuff like std.
i'll try to put stuff I changed/added to remember why i did that. hopefully i'll remember asaaaa aasadkasjd

writeup.md is for more typechecking logic and stuff.

---

- changed ListLike's from-listlike so that it takes in a Ptr to Array. not sure why it wasn't like this before. now `Slice` has a ListLike instance!

- thinking about FromString. StrView is a reasonable default (unlike Haskell, which needs the fromString, because the default String is bad)
    (FromChar can eval to StrView and ConstStr too, so FromString would be natural.... but stuff like `['a', 'miau']` breaks and we then would need default resolution rules...)

- added string matching, but only for a type that has both FromString and Eq. uses FromString + Eq instances. so we create the same type as the compared string.
    - og idea was to use StrView and have a function like `streq()` to compare any type that has a Str instance?
    - good enough for now - works with strview and chars.
    - but not being able to match against formatted strings is ugly and "breaks" their "connection" to the language (i forgot the right vocab)
    - should I want polymorphic arguments? should I add a `eq-strview()` function maybe? (with a default impl.)
    - it seems that list deconstruction is partially broken after the Interpreter rewrite. I'll need to add test cases for this....

- added polymorphic number matching for case expressions. uses `FromIntegral`, `Eq` and optionally `Negation` if the number is negative.
    - also changed the default number type from I64 to Size, because it can't be negative by default.
    - makes me think about side effects - should I make any guarantees about functions being re-called, or can I just make it unspecified?

- just reordered stuff in the modules. Created `Prim.kkc` which contains basically everything in teh correct order. and later modules reexport it. this should create a nice interface for the user (all Str functions under `Str`), but it's super annoying.
    - currently split Str into StrInst and Str. basic modules use StrInst and Str can use StrBuilder, allocate, etc.
    - but it's empty - I realized the StrView module should have the Str allocating function.
    - so, I might merge Str and StrInst again.
    - im also thinking of Char depending on Array module. Array should be pretty basic, no? but it currently is one of the "outer" basic modules - it depends on a lot of other stuff, ONLY due to `or-fail`. I've moved Failable to Prim and now Char can depend on Array... but it does not have to use it yet, since all functions that might use it in the future (unscalarize) are also in Prim...
