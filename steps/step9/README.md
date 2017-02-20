# Changes to `compiler.scm`
## Pairs and lists
* Added support for `pair?` as well as `cons`, `car` and `cdr`. The only thing
  to watch out for is to generate the code for both arguments to `cons` first,
  and then insert them into the pair. Otherwise, potential recursive calls to
  `cons` and other constructs may change the value of `EBP`. With this order,
  we add data to `[EBP]` and `[EBP+4]`, and increment `EBP` all at once. Any
  recursive calls will also have done the same.
* The next step is to implement `begin` and what the test calls
  "implicit-begin". The latter is what we have been doing all along:
  expressions in the body of a `let`, `let*`, `letrec` and `lambda` behave as
  if they were inside a `begin`. All that remains is to implement `begin`,
  which is straightforward.
    * As a future enhancement, we can switch the `let` family of expressions
      as well as the `lambda` to only handle emitting a single expression.
      Given the input expression, we can transform the body of these
      expressions to put the body expressions inside an explicit `begin`.
      In this way, only the procedures `emit-begin` and `emit-tail-begin`
      will have to handle multiple expressions.
* We also need to implement the primitive `eq?` which is used in the tests for
  `set-car!` and `set-cdr!`.  This is a simple implementation, which is
  identical to the implementation of `fx=` and `char=` (`eq?` is defined as
  returning `#t` when two objects are identical; when they have the same
  pointer).
    * It would be useful to implement aliases for a primitive: for instance,
      some Scheme implementations use `fx=?` instead of `fx=`. Chez Scheme
      appears to accept both. It would be useful to define `fx=?` as an alias
      for `fx=`, instead of redefining it. This can be done by extending the
      syntax for `define-primitive`. Once this is done, `fx=`, `char=`, and
      `eq?` can be aliases.
    * The previous bullet indicates a deficiency in our implementation: we do
      not check at runtime whether (say) `fx=` is actually passed fixnums. In
      fact, we do not check at compile time either, but there is always the
      possibility of a legal construct that the compiler may throw an error
      for, or an illegal construct that the compiler may not be able to catch.
* For `set-car!` and `set-cdr!`, we return the pointer to the pair in `EAX`,
  while the value that needs to be set as the `car` is returned as a stack
  pointer. In order to copy this to memory, we need to get the value into a
  register (something like `mov [eax-1], [esp-4]` is not allowed as it is a
  copy from memory to memory). Since we have just implemented contex save and
  restore for registers, we use the register `EDX` to do this. This is assumed
  to be a scratch register anyway (and actually is not part of the save and
  restore). The value is copied off the stack into `EDX`, and then moved from
  `EDX` into the appropriate pointer offset from `EAX`. For instance, for
  `set-car!`, the instruction is `mov [EAX-1], EDX`.
    * Note that we use EDX for this move, which works fine. However, in the 
      future, we must be careful that EDX does not get modified during any
      recursive code-generation call. Or rather, we should assume that it does
      get modified, and act accordingly. This is the same sort of handling
      that we (implicitly) have currently for EAX.

## Vectors
At first implementation of `make-vector`, we run into a problem: the tutorial
specifies that `make-vector` is to be called with an initializer like 
`(make-vector 4 #f)` but the tests specify the form without an initializer
such as `(make-vector 4)`. Scheme specifies both forms. Rather than lose the
implementation we did for `make-vector` with an initializer, we define a new
primitive `make-vector-init` which implements it with an initializer. At a
later point, we can merge the two.

We store the length as a fixnum. Note that when we are passed an index into
the vector (as for `vector-ref` or `vector-set!`), the resulting value that
the assembly code gets is 4 times actual index (i.e. for an index of 3, we get
the value 12). Coincidentally, this is exactly the offset (in bytes) that we
have to make from the base pointer, and we take advantage of the fact.

### Naive method for `4N+4` rounding
*I came up with this initially, but then got a better idea. See next section*

For vectors, we will have to allocate an area of size `4* N + 4`, but which 
should be rounded up to be a multiple of 4. We can do this by realizing that
`4*N + 4` is always a multiple of 4. Thus we check if bit 2 is set (which
indicates that it is a multiple of 4, but not a multiple of 8). If it is, we
add 4 to the value. For this we can use the instruction `BT`, which sets `CF`
to the value of a given bit. We can then use `JNC` (jump if not carry) to go
past the "rounding instruction". The rounding instruction is merely adding 4
(this will make something that is an odd multiple of 4 an even multiple of 4,
or in other words a multiple of 8).
```assembly
    BT EAX, 2      ; Check bit 2 and copy to CF
    JNC noRounding ; If CF=0, nothing to do
    ADD EAX, 4     ; Add 4 which makes it a multiple of 8.
noRounding:
    <Some other instructions>
```

## Updating vectors to fit in 8 byte multiples
For vectors, we should make sure our data fits in 8 byte multiples. The vector
is of length `4N+4`, which means that`N` needs to be odd to make sure `4N+4` is
a multiple of 8 (note that `4N+4 = 4(N+1)`, which is a multiple of 8 whenever
`N+1` is even).

Thus the code to do so (similar to the naive method above):
```assembly
    BT EAX, 0     ; check bit 1 and copy to CF
    JC noRounding ; if CF (= bit 0) is 1, EAX is odd-> nothing to do
    INC EAX       ; Now EAX is odd
noRounding:
    <Move on with the rest of instructions>
```


## Strings

## Bugs
* There is a bug in function definitions; we do not check that the lambda
  expression is actually a lambda expression! Consider the expression
  ```scheme
   (letrec ([f (lamba (n) (fx+ n 2))]) (f 2))
  ```
  Note the misspelling of `lambda` as `lamba`. This compiles and generates
  just fine, with the expected result of 4. This is due to the use of
  `let-expr?` for `letrec?`, which simply checks that bindings exist. On the 
  other hand, `letrec` assumes that `letrec?` has checked the form and does
  not check that the bindings are actually lambda expressions.

  Since we have the convenient function `lambda?` already defined, we use this
  in `letrec?` to ensure that the "bound" portion of `letrec` bindings are
  actually lambda expressions.

# Tests with "real code"
Now that we have both procedures and lists (albeit having to be specified with
`cons`), we can write some real tests. The file `fib.scm` calculates the first
35 Fibonacci numbers, and can be loaded with the previous utilities:
```scheme
(test-expr (expr-from-file "fib.scm"))
```
Note the use of `reverse` as well to reverse the resulting list; this also
comes out as a tail call.

# Other changes
## Moving `scheme_entry` to `lib.s`
With the introduction of the heap and the change to save and restore the
registers, the function `_scheme_entry` becomes more complicated. However, the
code for this function does not change at all from program to program. As a
result, we move the function to the file `lib.s`, which we have so far being
carrying around as an empty file.

We will now have to make a call from a function declared in one assembler file
(`scheme_entry` in `lib.s`) to another function declared in another assembler
file (`L_scheme_entry` in our generated file `stst.s`). In order to do this,
we have to rename the function `L_scheme_entry` to `R_scheme_entry`. See the
section "Assembly investigation" for more details.

We move `_scheme_entry` to `lib.s` and added context save and restore; also
move heap pointer to `EBP` and stack pointer to `ESP` as before.

# Save original pointer on stack
With our new implementation, we pass the context pointer in `ECX`, and hold it
there for the duration of execution of `R_scheme_entry`. Rather than doing
this, we can save `ECX` onto the stack of `R_scheme-entry` (that is, after we
have switched the stack pointer), and restore it from the stack after we
return.  Note that this puts a burden on us to make sure the Scheme stack is
at exactly the same value when it returns to `scheme_entry`. This is what we
have done so far, but if later we implement early returns (e.g. `call/cc` or
equivalent), we will have to pay attention to restore the stack pointer to
where it was when the continuation was created.

The actual save and restore is quite simple: right before `R_scheme_entry`, we
execute `push ECX` right before calling `R_scheme_entry`, and `pop ECX` right
after we return. This then allows us to use `ECX` freely in our generated
assembly.

### Assembly investigation
* Creating functions in a new assembly file and declaring them with
  `._globl` makes them accessible. Unlike other assemblers, CLANG does 
  not appear to need a directive `extrn` or similar to declare external
  symbols. We move `_scheme_entry` into `lib.s` and then don't emit it
  any more. Finally our `lib.s` has some content.
* Note that CLANG appears to have a restriction that labels starting with
  `L` are local, if we declare `.globl L_scheme_entry`, CLANG returns the
  error 
  ```
  stst.s:3:12: error: non-local symbol required in directive
  .globl L_scheme_entry
  ```

  Renaming this to `R_scheme_entry` fixes the issue (for "real scheme
  entry").

## `startup.c` changes
* Allocated heap in accordance with the tutorial.
* In `startup.c`, updated `getCharString()` somewhat; previously it was
  returning a `NULL` pointer when the character was not a special character,
  which the caller would handle. We now change this function to return a
  string representation of the character even for "standard" characters.
* Update `print_ptr()` to handle pairs. To do this, we have to make recursive
  calls to `print_ptr()` to handle lists and pairs. Thus we create a function
  `print_partial()` which can be used to print expressions without the
  trailing `\n`. This can be used in recursive calls.
* We add special handling for pair contents, so we can print lists like
  `(3 4 5)` instead of `(3 . (4 . (5 . ())))` and `(3 4 . 5)` instead of
  `(3 . (4 . 5))`. In order to do this, we create a function
  `print_pair_contents()`.  This function prints the `car` of a pair,
  and then looks at the `cdr`. If the `cdr` is a pair itself, a space is
  printed followed by a recursive call to `print_pair_contents()`. If the
  `cdr` is not a pair, we first print `" . "` followed by the `cdr` value.
  Both the `car` and the `cdr` (if not a pair) are printed by calls to
  `print_partial()`.
* Added implementation for printing vectors


# Todo
## 1.9.1 Cons and lists
* Macro transformer to make let/let\*/lambda single expression forms, and
  use begin to generate code?

## 1.9.2 Vectors
* vector?
* make-vector?
* vector-length
* vector-ref
* vector-set!

## 1.9.3 Strings
* string?
* make-string
* string-set!
* string-ref


## Other
* Create geck for more efficient code generation
* Create geck for save ecx in stack, if not otherwise done
* Try to use more registers
* aliases for primitives, e.g. fx> => fx>?
* Error checking geck: e.g. function call with incorrect # parameters still
  generates but obviously with incorrect answers.

# Passing quoted lists in (Petite) Chez Scheme
How would we process quoted lists in our compiler? Note that the expression we
pass in is itself a quoted list. Say we want to apply the procedure `f` to the
list `(a b c)`. This should be passed in as the expression
```scheme
(define e '(f '(a b c)))
```
We then find that the inner quote is represented as part of a list, i.e.
`(cadr e) => '(a b c)`. See comments in the snippet below.

```scheme
> (define e '(f '(a b c)))
> (car e)
f
> (cadr e)
'(a b c)
> (length e)  ; e is a list of 2 elements, first is 'f and second '(a b c)
2
> (define c (cadr e)) ; Investigate the second element '(a b c)
> c
'(a b c)
> (length c)          ; It appears to be a list of two elements!
2
> (car c)             ; Whose first is quote, but what is quote?
quote
> (symbol? (car c))   ; It is a symbol
#t
> (eqv? (car c) 'quote) ; With value 'quote
#t
> (symbol->string (car c))
"quote"
> (cadr c)            ; The second element is the list (a b c)
(a b c)
```

We can thus process lists not specified as the `(list ...)` form or the 
`quote` form.
