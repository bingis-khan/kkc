# TODO

- [x] exports
	- [x] `{..}` for multiple imports.
		- [ ] ~multiline export list in `{...}`~
		- [x] indented export list
	- [x] `*` wildcard (and `Class(*)`)
	- [ ] parse error recovery
- [x] import stuff
	- [x] wildcard
	- [x] on same line (comma separated list)
	- [x] `(`, `)`
- [x] import synonyms
- [ ] pointer deconstructions are a footgun
	```
		NextElem(next-elem)
			# if the line below is commented out, we overwrite memory. (the full example does `case p&.next-it-type`, and the next modification overwrites it.)
			# next-elem = next-elem
			p <&.next-it-type= NextSep
			return Just(next-elem)
	```
	- at the same time we need to do this to modify stuff. just make sure, that deconstructed lvalues get properly referenced (and add tests for this, i haven't done that in a while ㅠㅠ)
	```
	case &ed&.mode
		Ptr(Cmd(sb))
			StrBuilder.write-char(&sb, 'a')
	```
	- solution: make references explicit. something like `Cmd(*sb)` or `Cmd(&sb)`, which has a type `Ptr ...`. also make deconstruction respect lvalues.we can also emit an error if we're taking a reference to a non-lvalue.
		- smoler thing, an error/warning might be too annoying, especially if we're currently debugging, and we just maybe want to have a reference...
- [x] string matching in deconstruction. (currently only StrView)
	- [ ] polymorphic string matching? (what would be the correct way to handle this? should it be polymorphic? maybe an additional function in Str with default implementation like `strview-eq()`?)
- [x] polymorphic number matching (use a combination of FromIntegral + Eq)
- [ ] multi-backend tests, nicer (im thinking of something like the B language tests.)
- [ ] recursion (including nested recursion, which would make mutual recursion possible)
- [ ] recursive datatypes (incl Ptr which breaks depth checking).
- [ ] **unions** + typeclass-bound anonymous unions?
- [x] deconstruction on assignment
	- [ ] deconstruction without a `let` keyword.
- [ ] hidden tvars
- [ ] colored errors + prettier :3 + should I keep errors in stderr or should they be stdout?
- [ ] trailing commas in the syntax
- [ ] better occurs check (iirc we init an FTV struct which is pretty slow, make a dedicated function for this)
- [ ] crappy code - separate tvars into tvars and tnums
- [ ] reevaluate type defaulting (currently, the first encountered class default gets defaulted, which makes the process effectively stochastic. problem if a single type has two classes with type defaulting rules; or just throw out FromString and make StrView the one and only class.)
- [ ] review the architecture of the type checker. (connected with "numbers in type parameters")
- [ ] apostrophe in string interpolation breaks it (actually tokenize string interpolation? allow actual expressions? or just track parens?)
- [ ] module search should start at program path, not cwd.
- [ ] grapheme cluster parsing with surrogates (currently we only do single codepoints)
- [ ] colored errors (RED stuff) plus basic syntax highlighting in errors?
- [ ] I want this to work: (19.06.26: should it work tho? it's really stupid. it's cool with already written code tho)
		```
		fn sort'(slice Slice a, funcmp (a, a) -> Ordering)
		    inst Ord a
		        cmp (l, r): funcmp(l, r)
		    Slice.sort(slice)
		```

- [ ] external structs.
	- [ ] in external structs, don't scramble the field name by default.
	- also keep the C ordering by default. (I want that Zig ability of being able to reorder fields)
- [ ] this snippet confused me, although the error is obvious in hindsight:
	```
	MenuItem
		action () -> ()
	item = MenuItem { action: fn x: Unit  }
	```
	- the error was "mismatching param lens" and the thing is, action here has an EMPTY parameter list. I should add a special case for this to remind the user that this is indeed an empty param list and not a tuple -> tuple function.
- [ ] prevent overwriting of base file, eg. i compile the file `script` and the compiler should make a special name to not overwrite it. (append `.exe`? `.out`?)



# Other stuff

- I had two instances for `Eq StrView` left over, one with the wrong types. Somehow detect it and maybe signal that the instance is wrong OR tell the user that a different instance than always is surprisingly used.


# Finished

- [x] I/UXX integers + appropriate typeclasses
- [x] type synonyms (paramterized type synonyms!)
- [x] expression if
- [.] expression case
	- in case of case, it doesn't have to fit into every expression, just after assignment or mutation, but not in `if` statement for example
	- nah, ive made it fit everywhere, just like lambda! which means it's kinda bugged right now, but it's noted and the implementation is there.
- [x] chars
- [x] array literals
	- [x] FromList typeclass.
	- [x] smol cleanup (decide if I want to factor out the array allocation / sizing code)
- [x] numbers in type parameters
	- [x] parse ^identifier together: `^  identifier` would become invalid.
- [x] put typeclasses in place of types and it just works
- [x] basic type parameter number ops
- [x] simpler/rethink Str instance (separate printing from char gen)
- [x] utf8 support (my own, inbuilt support!)
