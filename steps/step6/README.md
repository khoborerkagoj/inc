# Local variables (1.6)
The definitions for variables are straigthforward. Each variable's value is
saved in the stack, and is an alist whose entries have the form
`('variable-name . stack-index)`.  When referencing a variable, we read out the stack index and generate a stack load from that index.

For `let`, we mostly follow the instructions and code template given.
Normally, we process the bindings one at a time, each time extending the
environment that was supplied to us. However, when we look up a variable that
is part of the right hand side of a binding, we pass in the **original**
environment `env` that was supplied to us, and not the newly extended
environment `new-env`. For `let*`, we pass in the extended environment.

```scheme
;; from emit-let
(let ([b (first bindings)])
   (emit-expr si env rhs b)   ; be careful, this is env, not new-env
```

```scheme
;; from emit-let*
(let ([b (first bindings)])
   (emit-expr si new-env rhs b)   ; be careful, this is new-env, not env
```

When an already defined variable (in the same `let` expression) is referenced
in a `let`, the original definition of the variable is used (as it existed
before the `let`). For `let*`, the most recent definition wins:
```scheme
(let ([x 1] [y 2])
  (let [x y] [y (+ x 1)])  ; x,y on rhs have original values of 1,2
    (+ x y)))              ; x==2 and y==2; return value 4

(let ([x 1] [y 2])
  (let* [x y] [y (+ x 1)]) ; x,y on rhs use most recently defined values
    (+ x y)))              ; x==2 and y==3; return value 5
```

# TODO
* `emit-let` and `emit-let*` can be combined into one function.
