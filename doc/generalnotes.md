notes for general stuff like std.
i'll try to put stuff I changed/added to remember why i did that. hopefully i'll remember asaaaa aasadkasjd

writeup.md is for more typechecking logic and stuff.

---

- changed ListLike's from-listlike so that it takes in a Ptr to Array. not sure why it wasn't like this before. now `Slice` has a ListLike instance!

- thinking about FromString. StrView is a reasonable default (unlike Haskell, which needs the fromString, because the default String is bad)
    (FromChar can eval to StrView and ConstStr too, so FromString would be natural.... but stuff like `['a', 'miau']` breaks and we then would need default resolution rules...)
