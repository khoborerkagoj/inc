# Conditionals (1.4)
The basic conditionals follow the template specified in the tutorial.
The challenge is in the `and` and `or` syntactic forms that are specified in 
the exercises.

As an example, the file [`andexpand.scm`](andexpand.scm) implements how
`and` would expand. One possibility is to use this to expand an `and`
expression and then use the code generation for `if` to generate the code.

For instance, try the expression `(and-expander '(and 2 3 4 #f))`. This
results in the expression
```scheme
(if 2 (if 3 (if 4 #f #f) #f) #f)
```
This seems quite inefficient, with all the `#f` values at the end.

We therefore use a different tack: use the definition of `and`:
- If no terms are provided, `and` returns `#t`.
- If there are arguments, evaluate each in turn. If any of them is `#f`, return
  `#f`. If not, return that argument.
- If we run out of arguments, the last one is the value of the expression.

And for `or`:
- If no arguments are provided, `or` returns `#f`.
- If arguments exist, evaluate each one in turn. If any of them is true (i.e. 
  is not `#f`), return it. Otherwise move to the last one.
- If we run out of arguments, return the last one.

This is what we implement for `and`. First initialize `eax` to `#t`, and then
load each argument (evaluated) into `eax`. If this compares true with `#f`,
`je` to the return argument. Otherwise, move on. At the end of the list, we
will "fall through" to the return argument in any case. Note that if we jump
to the end of the and expression, eax already has the last value in it.

The implementation of `or` is the same, except we initialize with `#f` and
check that the argument is **not** `#f` before moving to the next one.

# TODO
- We have not implemented the optimization in exercise 1.4.3 of the tutorial,
  namely the double comparison in a boolean expression: we first compare to 
  generate `#f` as the result of the boolean, and then compare the result to 
  `#f`. 
    - The fix would be to have a different definition of `emit-expr` which
      would just generate 1 or 0 as the result of the boolean expression. This
      alternate form would be called only from the test expression of an `if`,
      so would not affect results. This is not currently implemented.
