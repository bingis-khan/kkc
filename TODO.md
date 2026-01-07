# Features

- I/UXX integers + appropriate typeclasses
- [?] chars
	- not fully - type defaulting is kinda spaghetti (but behavior seems to be correct).
- utf8 support (my own, inbuilt support!)
- type synonyms
- [.] numbers in type parameters
	- crappy code - separate tvars into tvars and tnums
	- parse ^identifier together: `^  identifier` would become invalid.
- array literals
	- FromList typeclass.
- basic type parameter number ops
	- +
	- -
	- *
	- /
- expression if and expression case
	- in case of case, it doesn't have to fit into every expression, just after assignment or mutation, but not in `if` statement for example
- put typeclasses in place of types and it just works
- recursive datatypes + checking with the help of Ptr.
- (fast!) occurs check


# I need a place for all the funny errors

- empty function (without last pass) produces a compiler error
- undefined class in inst declaration produces a compiler error
- undefined type in inst declaration produces a compiler error
- loops forever
```
fn drop-while'(it, pred)
	it-copy = it
	while True
		case next(&it-copy)
			None
				return it

			Just(x)
				if not pred(x)
					return it

    # if we change it and it-copy, same thing
    # probably an infinite type
    # if we specify the type, no loop
		it <= it-copy  # loops due to this

	return @undefined
```
- writing `Tuple2(iter, Whatever iter)` in type declaration produces a compiler error.
- functions which were imported by name now visible outside (eg. it's possible to do Iter.Char, because it's defined in prelude, but it should be an error)
- apostrophe in string interpolation breaks it (actually tokenize string interpolation? allow actual expressions? or just track parens?)
