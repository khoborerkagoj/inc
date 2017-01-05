# Filtering tests
It is tiresome to run all the tests when you have a failure on test 153.
For this reason, we created an additional library `tests-filter.scm` which 
can show the "test categories" defined, allow you to filter them, and then
reset back to original. Once bugs are fixed, you should run all tests!

You will need to load this library explicitly.

    > (show-tests)
      ("integers" "immediate constants" "fxlognot" "not" "char?"
       "boolean?" "null?" "fxzero?" "fixnum?"
       "fixnum->char and char->fixnum" "fxsub1" "fxadd1" "and/or"
       "if" "binary primitives" "if" "fx>=" "fx>" "fx<=" "fx<"
       "fx=" "fxlogand and fxlogor" "fx*" "fx-" "fx+")

Filter to only have a few:

    > (filter-tests "binary primitives" "fx*" "boolean?")
    > (show-tests)
      ("boolean?" "binary primitives" "fx*")

You can then use `(test-all)` to run these tests. To reset to the original
list (perhaps to choose a different subset), run 

    > (reset-tests)
    > (show-tests)
      ("integers" "immediate constants" "fxlognot" "not" "char?"
       "boolean?" "null?" "fxzero?" "fixnum?"
       "fixnum->char and char->fixnum" "fxsub1" "fxadd1" "and/or"
       "if" "binary primitives" "if" "fx>=" "fx>" "fx<=" "fx<"
       "fx=" "fxlogand and fxlogor" "fx*" "fx-" "fx+")

# Binary Primitives (1.6)
## `startup.c`: adding a stack
This is a somewhat complicated section, since we have to set up the stack 
ahead of time. Additionally, the tutorial suggests a method of reserving a
page before and a page after the stack section which has no access: any
attempt to access it will cause a crash in the program (rather than a
mysterious error later, as stack overflows/underflows are wont to do).

The APIs presented are for Linux; the enclosed [startup.c](startup.c) suggests
one way to do it under Windows, using the `VirtualAlloc`, `VirtualProtect` and
`VirtualFree` functions. A short test in startup.c itself verifies that even
reading those pages causes a crash.

## Extending the compiler
For the binary primitives, we can follow the suggested method and put one 
argument on the stack. However, we go with the optimization suggested in
exercise 4 of the tutorial and check if either of the arguments is an immediate
value. If they are, we evaluate the other one and keep in eax. We then emit
the appropriate expression to evaluate the other argument (either its immediate
representation or the pointer to the stack where it is located) and return
to the caller. This is encoded in the procedure `emit-args-and-save*`.

Additionally, for binary comparisons, a procedure `emit-binary-compare` does
most of the work. We don't know which (if any) argument will be an immediate
value; so to check `<`, we use either `setl` if the first argument is in `eax`
and `setg` if the second argument is in `eax` (this checks if `arg2 > arg1`).
The other binary operations are similar.

Note that `fx=` and `char=` have exactly the same implementation and the same
for the other boolean-valued functions. At the end of the day, these are just
integer comparisons once the `immediate-rep` has been done.

## Extending tests
The file `tests-1.5-req.scm` has been extended with tests to cover the boolean
functions for `char`.

## Possible optimization
It is also possible to detect if there are two immediate values and simply
populate `eax` with the result of evaluation. For instance, given
`'(fx+ 2 3)`, we can emit the code
```scheme
(emit "mov eax, ~a" (immediate-rep (fx+ 2 3)))
```
However, we chose not to do this for a couple of reasons:

1. If we use some other Scheme which does not support every single primitive
   that we are creating, we will be in trouble.
2. In our tests, most expressions are of the "two-immediate" form, while
   those are rarer in real life. This is especially true since we have not
   defined things like lists, procedures and so on. If we output this
   optimization, we will produce correct results but not necessarily test
   our compiler. This can be deferred for a further step (perhaps once the
   compiler is complete).

# TODO
Implement the optimization discussed above.
