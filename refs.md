# Reference semantics !!!

(different than you think :OOOOOOOOO)


dt = Data(4, 5)
case dt
  Data(x, y)
    n = &x
    n <&= 1 # will dt's first field turn into 1???
    # lets assume, that for now it's copied.
    # (makes it easier to implement. not sure what makes sense more overall)
    # (maybe I can add reference syntax (like `Data(&x, y)`) to this tho, but maybe it would be comfier to just allow references to modify the original type (would not need the syntax))
