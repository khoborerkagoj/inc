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
