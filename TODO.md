# Features

- I/UXX integers + appropriate typeclasses
- type synonyms (paramterized type synonyms!)
- [V] array literals
	- [V] FromList typeclass.
	- add nice errors (currently unreachable + "TODO: errors")
	- smol cleanup (decide if I want to factor out the array allocation / sizing code)
- expression if and expression case
	- in case of case, it doesn't have to fit into every expression, just after assignment or mutation, but not in `if` statement for example
- [V] put typeclasses in place of types and it just works
	- but we generate a lot of slop constraints then. I think we shouldn't do that + it unnecessarily slows down the compiler when the user defines types for a function.
	- maybe make a special place for them? (see typechecker architecture review)
- recursive datatypes + checking with the help of Ptr.
- (fast!) occurs check
- [?] recursive functions!!!
	- [ ] basics work, but there are more complicated versions.
- [?] chars
	- not fully - type defaulting is kinda spaghetti (but behavior seems to be correct).
- [.] numbers in type parameters
	- crappy code - separate tvars into tvars and tnums
	- [V] parse ^identifier together: `^  identifier` would become invalid.
- [ ] review the architecture of the type checker. (connected with "numers in type parameters")
	- rethink "slop" constraints? We can split them into actionable and non-actionable constraints. One other non-actionable constraint is checking if integer fits in the inferred range! (this constraint must also be propagated further.)
	- Maybe it would be better if constraints were associated with tyvars (like fields)?
	- + constraints are automatically applied on demand.
	- + gathering constraints in generalization is easy: just do it while FTV-ing.
	- - we cannot track how many constraints are left module-wise? same problem with us not knowing how many free type variables are left. We at least need to track them to unify with a Unit.
- utf8 support (my own, inbuilt support!)
- basic type parameter number ops
	- +
	- -
	- *
	- /


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
- it seems like I didn't add record updates ????
- it seems like I didn't add `Joltage { first, second }` deconstruction syntax??
- OOM when function has no body
