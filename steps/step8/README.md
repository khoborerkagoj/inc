#1.8 Iteration via Proper tail calls

For this section of the tutorial, we need to generate proper tail calls.
However, since we made several changes to the compiler, we need to compensate
for those.
* `let`, `let*`, `lambda` and `letrec` allow multiple body expressions, as
  does the main program. Therefore the tail optimization will apply to the
  last expression within any of these forms, rather than the "body of the
  procedure" as the tutorial states.
* Similarly, the recursive definition states: *if a `let` expression is in
  tail position, its body is also in tail position*. This is modified to
  *if a `let` expression is in tail position, the last body expression is also
  in tail position*. Similarly for the other clauses.
* We have implemented `and` and `or`. These are not covered in the
  instructions. For these forms, we could adopt the policy that the last
  expression passed to `and` is also in tail position if the `and` clause
  itself is. It does get complicated, though, since the previous clauses will
  have to jump to the return point while the last clause will not (if it is a
  tail call). For now, we don't apply the optimization to `and` and `or`, but
  simply return to the caller if it in tail position. In pseudo code, this is
  what the optimization would look like:
```C
// (and e1 e2 e3) ; assume e3 is a procedure call
  if (!e1)
    goto EXIT;
  if (!e2)
    goto EXIT;
  tail_call(e3); // this never returns to this point
EXIT:
  return #f;
```

# Code
Analogous to our procedure `emit-exprs`, we create an expression
`emit-tail-exprs` that applies `emit-expr` to all but the last expression, to
which it applies `emit-tail-expr`. Additionally, we combine some of the
procedures to handle both the `tail` form and the non-`tail` form.

`emit-lambda` is the same as before, except for using `emit-tail-exprs`
instead of `emit-exprs` for the body (this is where the whole tail-call
business is kicked off), and leaving out the `ret` at the end (since each of
expression calls will ultimately end with a `jmp` for a tail call or a `ret`
for the non-tail branch at recursion termination).

Two utility procedures `stack-` and `stack+` are created which replace the
multiple calls to `(- si wordsize)` and the like.

`emit-tail-app` is perhaps the most complicated part of this step. We emit the
arguments as before, but instead of adjusting the stack before and after the
recursive call, we call the procedure `copy-arguments` and emit a `jmp`
instead of a `call`.

# Enhancements
If the calling procedure before a tail call has no arguments, our procedure
`copy-arguments` will do a whole bunch of useless copies. We can skip the
copies in this case.
