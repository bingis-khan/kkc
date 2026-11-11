
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


--- assocs? and other comments

TODO: should I produce same unions or is it undefined, possibly optimization.

the problem:

```
inst A I32
  # class fun uid: 24
  fun (self :: I32)[](2) -> Unit ## {||}
    pass
    return Unit
f (x :: a)[](2) -> Unit ## {a|{u0}|(1283)(a => ((a){u0} -> Unit) :$fun)}
  fun{u0}(x)
  return Unit
f{u1 (f)(<(I32 |] {u2 (fun)(<()>)} |> [fun <()>])>)}(1)
f{u3 (f)(<(I32 |] {u4 (fun)(<()>)} |> [fun <()>])>)}(2)
```

`fun` here has a different union cuz we instantiate it separately.
sheeesh neger

---

it's visible when we create a new function - we have to reassociate all the stuff here.

```
g (x :: c)[@f2(1) <(c |: {u3} |; 1285)>((c){u4 (f2)(<(c |: {u3} |; 1285)>)} -> Unit), @f2(1) <(c |: {u5} |; 1286)>((c){u6 (f2)(<(c |: {u5} |; 1286)>)} -> Unit)](2) -> Unit ## {c|{u3}, {u5}|(1285)(c => ((c){u3} -> Unit) :$fun), (1286)(c => ((c){u5} -> Unit) :$fun)}
  f2{u4 (f2)(<(c |: {u3} |; 1285)>)}(x)
  f2{u6 (f2)(<(c |: {u5} |; 1286)>)}(x)
  return Unit
g{u7 (g)(<(I32 |: {u8 (fun)(<()>)} {u9 (fun)(<()>)} |; [fun <()>] [fun <()>])>)}(1)
```

here, g has 2 unions and 2 associations, even tho it can be only 1, since it depends on the same type. If the class function also creates new associations, we might need to

*continued in my notes cuz it's gonna be less convoluted to link to stuff??????*

<!--
 when assoc-ing depends on the same type and evaluates to the same instance function, AND the arguments are of the same type, should use the same union.
 NOTE: if we pass in a function to an instance function, even doe the function might have the same types, it's gonna be a different type cuz of the union.
-->

<!--
7. like/due to **1** a class function instantiation produces a new call union. and association.

TODO_8. a class function can later be unified with a new function. TODO: should it produce a new union? or unify with the current one? when it's in the same scope, it does not need to make a different union, but it doesn't really matter. when a new scope (env) is made, it must produce a new union - we may not unify - otherwise we modify the inner union for everyone.

9. unifying generalized class function instance with an instance Function instance should *not* modify the original union of the function.

<!-- 10. when an association's union does not interact with a function's scheme, its type is entirely dependent on the selected instance.
-->
