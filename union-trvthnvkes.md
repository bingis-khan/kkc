
## terms

- call union: in `fun{u}()` "u" is a call union of function `fun`
- type union: in `fun(x (){u} -> I32)` "u" is a type union. every union which is not a call union is a type union.
	- being a call/type union is relative - every union (except empty ones) is a call union to some function.
- scheme: a collection of types, unions and type associations uniquely identifying a function instantiation.
	- aka. stuff that can change when calling a function, generic stuff.
- match: application of that type scheme.
- association: type association, where we an instance of a typeclass is selected.

# trvths

1. whenever we instantiate a function, we MUST use a new union for its callsite

```
	fn a()
		pass
	_ = a{u1}()
	a{u2}()
	x = a{u3}
```

the reason is we don't know how it's gonna get used later.

> we can later possibly optimize for the common case of immediately calling a function.


2. due to **1**, if a function's parameter has a function type, we must also instantiate a new union for the parameter's union.
	- due to the same reason, this is because we also don't know what is gonna get unified later.
	```
		fun2 = fn x: x
		fun(fun2)

		fun2 <= fn x: x + y
	```

3. due to **1**, if the return type of a function is a function type, we must instantiate a new union for the return type's union.

4. function's return types and parameter types have no difference in the way they interact with the type system.
	- from here, whatever applied to the function's return type should apply for parameters as well.


5. despite **1**, when calling a function, the function does not care about its own call union and does not generalize over it.
	- this way, we can compile and reference a function only once, even though it's called in multiple places (and in multiple non-empty unions)
	```
		fn fun()
			pass
		fun{u1}()
		fun{u2}()  <- same function, different union.

	```

6. due to **2** and **3**, every union in the parameter or return will be part of the function's scheme.
	- that's because both can be "modified" by the caller.


--- assocs?
TODO: should I produce same unions or is it undefined, possibly optimization.

<!--
7. like/due to **1** a class function instantiation produces a new call union. and association.

TODO_8. a class function can later be unified with a new function. TODO: should it produce a new union? or unify with the current one? when it's in the same scope, it does not need to make a different union, but it doesn't really matter. when a new scope (env) is made, it must produce a new union - we may not unify - otherwise we modify the inner union for everyone.

9. unifying generalized class function instance with an instance Function instance should *not* modify the original union of the function.

<!-- 10. when an association's union does not interact with a function's scheme, its type is entirely dependent on the selected instance.
-->
