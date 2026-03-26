There are some problems when you pass the environment out of the function.

1. Env escaping through parameters.

eg.

  fn a(x)  # as a return value
    fn b()
      _ = x

    return b

or.

  fn a(fun, x)  # as parameter
    fn b()
      _ = x

    fun == b

currently, because the environment is in Match, when making a scheme we check if the environment is "inner" and then at each generalization point we extract its tvars (and envs and assocs ig?)

This is addressed in `mkSchemeForFunction` (look for `env-escaping`)


2. Env escaping through function calls.

  fn outer(fun)
    fun()

  fn a(x)
    fn b()
      _ = x

    outer(b)  # here, we are calling an outer function that has no context of `a` with `b`. This trips up the monomorphiser.

the outer function is not always on top level:

  fn base(x)
    fn outer(fun)
      fun()

    fn a(y)
      fn b()
        _ = x
        _ = y

      outer(b)  # here, the function only calls the outer that's in the inner scope, but we should not generalize over `x`s type.

Note, that we can take care of this in the monomorphiser, because the environment should not trigger any type errors. (except structural equality, eg: [x t<a>], which MAY monomorphise to the correct type and the environment should be equal, but it'll fail. tbh i don't care, since we're gonna replace the thing with unions anyway and current env equality logic is scuffed.)


Note, that such environments can be found by searching through the "Match" of this function, since the Match is LOCAL to the callsite.


fn base(x)
  fn outer(fun)
    fun()

  fn a(y)
    fn b()
      _ = x
      _ = y

    fn c(z)
      outer(b)  # in this case wat shood I do?


smart solution: at each level check for tvars (and envs and stuff) from the current level and extend the environment's scheme.
funny solution: just check is inst.fun.level < env.level and just get all the tvars.
even more stupid solution: technically, any env passed to the matches of external functions should be reschemed, so do just that.


3. Env escaping through variables from the environment.

The harder part. We have to do it in the typechecker, because (currently!) this MIGHT fail.

x =& @undefined
fn a(n)
  fn b()
    _ = n
  x <&= b

a(1) # okay
a(2) # still okay
a(True) # ERROR

The current expected solution is in `mkSchemeForFunction` to go through the environment, check for `Var`s and check their type for any environments that are from this function*.
  Proposed (bad, not very specific) algo: Still in `mkSchemeForFunction`, 

  *(eg. by checking the level BUT what if there are two functions which assign the environment, how do we know to map the local one?)
