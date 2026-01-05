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
