# 1.7 Procedures

This section expands the vocabulary with the `letrec` form, which allows us to
make procedure definitions. However, there are differences with Scheme:

* Unlike Scheme, and unlike the `let` and `let*` forms, we only define the
  `letrec` at the top level, and at most once. This is what is used to define
  named procedures.
* We cannot define variables (non-procedures). `letrec` can have its bound
  variables refer to each other in their definition, but only within function
  calls. In other words, a variable defined in a binding cannot be made to
  be evaluated within the same set of bindings. However, a variable referred to
  within a variable definition is not evaluated at that time, so is allowed.
    * Note that a variable can be expressed as a function; the following are
      equivalent:
      ```scheme
         (letrec ((a 3)) (fx+ a 5))
         (letrec ((a (lambda () 3)) (fx+ (a) 5)))
      ```
* Lambdas are also only only definable within this top-level `letrec`.
* Closures are not allowed (yet).

For the implementation: we make two changes: both `lambda` and `letrec` are
allowed to have multiple forms in their body, with the results of all but the
last forms being discarded.  Secondly, the document refers to the `app` form,
which applies the function. We use the form used by Scheme, where a
parenthesized expression `(f 2 3)` applies the procedure `f` to the arguments
2 and 3. This is also what the tests assume.

## Implementing `emit-letrec`
For `emit-letrec`, we will first have to read the complete set of bindings for
the procedure. Since we have mandated that each binding value is a lambda
expression (and we even check for this), we can just read the set of bindings
and assign a unique label for each one. This is encapsulated in the
`make-initial-env` procedure.

Once the env is created, we emit code for each lambda expression. This is
presented in a curious fashion, with the expression
```scheme
(for-each (emit-lambda env) lambdas labels)
```
where the call `(emit-lambda env)` actually returns a procedure, closed over
the environment `env`, that actually emits the assembly code for that
particular lambda expression.  The procedure returned by `emit-lambda` takes
two arguments, the lambda expression and the label for it.

Without this construct, we would have to create a list of the same
length as `lambdas`, whose each element is `env` (which can be done with the
expression `(map (lambda (e) env) lambdas)`). We would then have to write
`emit-lambda` as a procedure of three arguments: the lambda expression, the
label, and the env:
```scheme```
(define (emit-lambda lexpr label env)
   <body>)
(for-each emit-lambda lambdas labels (map (lambda (e) env) lambdas))
```
This is possible, but could be inefficient. Alternatively, we would have to
iterate ourselves concurrently over the two lists and one `env`, which is
cumbersome.

In our implementation, the procedure returned by `emit-lambda` actually takes
**three** arguments, with the third one being the bound variable for that
lambda expression. This is completely optional, but we use it to generate a
comment with the name of the variable. This is useful when examining the
generated assembly code for debugging purposes, but serves no functional
purpose otherwise.

## The `emit-app` procedure
The code, as usual, follows the template provided in the tutorial. The
confusing portion is the stack adjustment, which is defined in the template
for `emit-app`.  For instance, assume that the value of `si` on entry to
`emit-app` is -8. This would mean that `[esp-4]` contains data from some
previous operation, and `[esp-8]` is the next location that stack data should
populate. Once we make the `call`, this is where the return address should go,
and any argument values to the procedure will be stored at locations starting
at `esp-12`. Therefore, when we start the `emit-arguments` procedure, we pass
it `(- si wordsize)` as the value of `si`.  In our example, this is -12.

Additionally, when we make the `call`, the CPU will first decrement `esp` and
then write the return address in the location that `esp` now points to. For
this location to be `esp-8` (as established in the previous paragraph), the
new value of `esp` needs to be the current value of `esp-4` (thus, once
decremented, it will point to the current value of `esp-8`). To do this, we
adjust the stack by -4, or in other words, `(+ si wordsize)`. After we return
from the procedure, we set the value of `esp` to it original value, or adjust
it by `(- (+ si wordsize))`.
