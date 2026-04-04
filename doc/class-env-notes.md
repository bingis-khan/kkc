I removed class functions in environments, but I seem to have a problem. 

	class A
		dosth (x _) -> Unit
	inst A Int
		dosth (x) -> Unit
			return

	fn asd()
		fn out(x)
			fn inner()  <- what should be in here? dosth (class function?)
				dosth(x)

		out(4)

I think that was a mistake - its not known whether the association originated from "here" or from a function that was called previously.



----

What should be the end goal of typechecked environment?
Which one would be more useful / less complicated?
I think the second one! We would have to discard the environment after the first one.

f () [println a -> ()] ? [println Int -> (), println Bool -> ()]
	g (x a) [println a -> ()]
		println(x)

	g(123)
	g(True)
