I've added barebones, incorrect (no grapheme clusters) utf8 handling, and Swift-style characters (Slices of the original string; as opposed to Rust's scalar values.)

When I first added utf8 handling I've experienced a severe performance degradation. I've discovered the reason:

  In Error, when I replace the "panic" function with the iterator version, performance is suddenly ultra bad. Why? more copying? Maybe I should profile?


Other stuff:
- [x] dynamic chars. the thing is we can't dynamically construct a char. what we can do is make a sentinel value which would make the char's value be extractable from the other member.
  We should also keep num-bytes correct...

  We can store a unicode scalar value in a pointer!
  Or even just store a 7 byte sequence in it!

  ```
    if ptr Mem.highest-byte-of-ptr() == 0x67
      return SmolStr(ptr Math.lshift(8))
  ```

  (a scalar value needs to be "prepared", so it can be pointlessly slow if we ever encode-decode them repeatedly)

  - [ ] write instance method? do we need it? or too much work? also, we didn't really test it in the alternative std, so maybe the performance is not that bad.

  - [X] grapheme clusters (scalars currently, cuz idc) in FromChar? or is 1-byte thing better?
      - the thing is, we will have to throw runtime errors in case a character does not fit in one byte (in case of AsciiChar).
  - [X] FromChar should have a (ptr Ptr U8, len Size) -> _ signature (or just Slice!) (a Slice would be nice, but it's easier to use primitive types :3)
  - [X] make a FromString method. (it will also have a guarantee, that the string is 0-termminated)
    - but do we actually need a FromString class? tbh, maybe it's too much. We don't have a problem like Haskell.
    - currently the type is also (ptr Ptr U8, len Size) -> _. The Str node is kinda weird now, as it can be both a Ptr U8 and ConstStr in other contexts. Maybe we should actually use StrView for this?
  - [X] stop using ConstStr. Replace it with DynStr (or equivalent)
    - new name for DynStr: StrView.
