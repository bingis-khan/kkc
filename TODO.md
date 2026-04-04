# TODO

- [ ] exports
- [ ] string matching in deconstruction.
- [ ] polymorphic number matching (use a combination of FromIntegral + Eq)
- [ ] mutual recursion
- [ ] recursive datatypes (incl Ptr which breaks depth checking).
- [ ] hidden tvars
- [ ] simpler/rethink Str instance (separate printing from char gen)
- [ ] better occurs check (iirc we init an FTV struct which is pretty slow, make a dedicated function for this)
- [ ] utf8 support (my own, inbuilt support!)
- [ ] crappy code - separate tvars into tvars and tnums
- [ ] reevaluate type defaulting (currently, the first encountered class default gets defaulted, which makes the process effectively stochastic)
- [ ] review the architecture of the type checker. (connected with "numbers in type parameters")
- [ ] apostrophe in string interpolation breaks it (actually tokenize string interpolation? allow actual expressions? or just track parens?)


# Finished

- [V] I/UXX integers + appropriate typeclasses
- [V] type synonyms (paramterized type synonyms!)
- [V] expression if
- [.] expression case
	- in case of case, it doesn't have to fit into every expression, just after assignment or mutation, but not in `if` statement for example
	- nah, ive made it fit everywhere, just like lambda! which means it's kinda bugged right now, but it's noted and the implementation is there.
- [V] chars
- [V] array literals
	- [V] FromList typeclass.
	- [V] smol cleanup (decide if I want to factor out the array allocation / sizing code)
- [V] numbers in type parameters
	- [V] parse ^identifier together: `^  identifier` would become invalid.
- [V] put typeclasses in place of types and it just works
- [V] basic type parameter number ops
