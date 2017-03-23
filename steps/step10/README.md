# Closures
We have now gone past the range of the tutorial, so must rely on a combination
of the paper and the test scripts. The first element to work with is
`closure`.

We first need to tranform the given expression into a form which recognizes
free variables within a &lambda; expression, and modifies the &lambda;
expression to incorporate the freevars. Then we "lift" the &lambda;
expressions out of wherever they are found, and put them in a top level
`labels` expression.

To do this, two procedures `transform` and `lift` are added. `lift` assumes
that the `lambda` forms have already been transformed, so the correct way to
call it would be `(lift (transform expr))`.

Additionally, rather than call the top level expression `labels` and the
actual `lambda` forms `(code ..)`, we simply choose to use the terms `letrec`
and `lambda`. While `letrec` is a proper use of the Scheme form, `lambda` has
been modified from its original definition. However, we have through this
process removed all `lambda`s from the code, and thus there are no `lambda`
forms left. This avoids name collisions if someone is using symbols
`labels` or `code`.

We can also allow `letrec` forms elsewhere in the code as well. We will just
have to make sure the top level `letrec` is handled differently; this is easy
as thanks to our transformation, it will always exist. Additionally, thanks to
the lifting, we can be sure that the only place a `lambda` will exist is in
the top level letrec. Every other location will have a closure instead of a
`lambda` expression.

## Top level code generation
Since we are now guaranteed to have a top level `letrec`, we can simply call
`emit-letrec` on the transformed and lifted expression. We also add a
`closure?` procedure, and an `emit-closure`. As a closure is just a data
element, `emit-tail-closure` calls `emit-closure` and then emits a `ret`
instruction.

## Closure expressions
To emit a closure, we first copy the label of the closure into `EBP`. In the
GNU Assembler (which clang is compatible with), emitting a `mov eax, L32`
(where `L32` is a label) emits an instruction to copy the *contents* of the
location pointed to by `L32` into `EAX`. This is obviously not what we want.
The two ways to copy the value of the label (the location it points to) are:
```assembler
lea EAX, L32
mov EAX OFFSET FLAT:L32
```
We use the first form (`lea`). See [this StackOverflow post](http://stackoverflow.com/questions/1897401/gnu-assembler-get-address-of-label-variable-intel-syntax) for more details.

Next, emit each of the arguments of the closure into successive `EBP` offsets
(`[EBP+4]`, `[EBP+8]` and so on). After this, we need to copy the value of
`EBP` into EAX as the return value, and then add to EBP to ensure that the
space is occupied. We also have to make sure we are at a multiple of 8.
Unlike, say, `make-vector`, we know at compile time the length of the closure.
Note that the arguments to `closure` *including the label* must be even, this
means that the length of the closure expression (including the symbol
`'closure`) must be odd. We take advantage of this to precompute the lengths,
using the following expression:
```scheme
  (let ([clen (length expr)])
    (emit "    add ebp, ~a"
          (* wordsize (if (odd? clen) (- clen 1) clen)))))
```

If the length of the closure expression is odd, this means it has an even
number of arguments, and the number of "word slots" is one less than the
length of the closure expression. If on the other hand, it is even, we have an
odd number of arguments and need to pad the number of expressions by one to
make it even: exactly the length of the closure expression!

## `emit-app` and `emit-tail-app`
Next we come to what is perhaps the hardest part: using the closures.

# TODO
* &#x2714; emit-lambda should be modified to account for free variables
* Need to write emit-closure
* Process emit-program differently, we can assume that there will always be a
  top-level letrec.
    * For now, don't allow letrec except for top level. We can add that later,
      and then we could consider splitting it up into emit-init-letrec (only
      contains &lambda;s) and emit-letrec (doesn't contain &lambda;s).
* Emit-app and emit-tail-app now only deal with closures
    * Later, we could try optimizing unclosed functions (functions with no
      free vars)-- these can still emit a standard function call.
* &#x2714; emit-variable-ref should account for non-negative values and
  reference those from edi instead of esp.
    * Note that this means that future variable references (just for define'd
      variables) must also be put on the stack. Otherwise we will have to
      rewrite emi-variable-ref to reference the heap or global data.
