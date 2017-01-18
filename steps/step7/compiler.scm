(load "../../src/tests-driver.scm")
(load "../../src/tests-1.7-req.scm")
(load "../../src/tests-1.6-req.scm")
(load "../../src/tests-1.6-opt.scm")
(load "../../src/tests-1.5-req.scm")
(load "../../src/tests-1.4-req.scm")
(load "../../src/tests-1.3-req.scm")
(load "../../src/tests-1.2-req.scm")
(load "../../src/tests-1.1-req.scm")

;; ======================================================================
;; Primitives
;; ======================================================================
;; Primitive infrastructure
;; ========================
(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name si env arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count* (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
                ;; the special names si and env are reserved, not to be
                ;; otherwise used in primitive expressions
                (lambda (si env arg* ...) b b* ...)))]))

(define (primitive? x) (and (symbol? x) (getprop x '*is-prim*)))
(define (primitive-emitter x)
  (or (getprop x '*emitter*)
      (error 'primitive-emitter "No emitter defined" x)))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (emit-primcall si env expr)
  (let ([prim (car expr)]
        [args (cdr expr)])              ; args includes "si"
    (define (check-primcall-args prim args)
      (unless (= (length args) (getprop prim '*arg-count*))
              (error 'emit-primcall "Invalid # args"
                     ((length args) . (getprop prim '*arg-count*)))))
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) (cons si (cons env args)))))

;; Unary primitives definition
;; ===========================
;; The special names si and env are reserved, not to be otherwise used in
;; primitive expressions
(define-primitive (fxadd1 si env arg)
  (emit-expr si env arg)
  (emit "    add eax, ~s" (immediate-rep 1)))

(define-primitive (fxsub1 si env arg)
  (emit-expr si env arg)
  (emit "    sub eax, ~s" (immediate-rep 1)))

(define-primitive (char->fixnum si env arg)
  (emit-expr si env arg)
  (emit "    sar eax, 6"))

(define-primitive (fixnum->char si env arg)
  (emit-expr si env arg)
  (emit "    shl eax, 6")
  (emit "    or  eax, ~d" imm/char-tag))

(define-primitive (fxzero? si env arg)
  (emit-expr si env arg)
  (emit-compare= 0))

(define-primitive (null? si env arg)
  (emit-expr si env arg)
  (emit-compare= imm/null-val))

(define-primitive (fixnum? si env arg)
  (emit-expr si env arg)
  (emit-mask-compare= imm/fx-mask 0))    ; last two bits should be 00b

(define-primitive (boolean? si env arg)
  (emit-expr si env arg)
  (emit-mask-compare= imm/bool-mask imm/bool-false))

(define-primitive (char? si env arg)
  (emit-expr si env arg)
  (emit-mask-compare= imm/char-mask imm/char-tag))

(define-primitive (not si env arg)
  (emit-expr si env arg)
  ;; if (eax != #f) eax = #t
  (emit-compare= imm/bool-false))

(define-primitive (fxlognot si env arg)
  (emit-expr si env arg)
  (emit "   not eax")
  ;; mask with all but bottom two bits
  (emit "   and eax, ~d"
        (bitwise-xor (- (expt 2 wordbits) 1) imm/fx-mask)))

;; Binary primitives definition
;; ============================
;; The special names si and env are reserved, not to be otherwise used in
;; primitive expressions
(define-primitive (fx+ si env arg1 arg2)
  (let ([res (emit-args-and-save* si env arg1 arg2)])
    (emit "    add eax, ~a" (cdr res))))

(define-primitive (fx- si env arg1 arg2)
  ;; sub is not symmetric, so we need to pay attention to what is
  ;; in the register
  (let* ([res (emit-args-and-save* si env arg1 arg2)])
    (emit "    sub eax, ~a" (cdr res))
    (if (not (car res))                 ; we just found arg2 - arg1
        (emit "    neg eax"))))

(define-primitive (fx* si env arg1 arg2)
  ;; mul cannot deal with immediates
  (emit-args-and-save si env arg1 arg2)
  (emit "    sar eax, ~a" (- wordbits fx/bits))
  (emit "    mul dword ptr ~a" (esp-expr si)))

(define-primitive (fxlogand si env arg1 arg2)
  (let* ([res (emit-args-and-save* si env arg1 arg2)])
    (emit "    and eax, ~a" (cdr res))))

(define-primitive (fxlogor si env arg1 arg2)
  (let ([res (emit-args-and-save* si env arg1 arg2)])
    (emit "    or  eax, ~a" (cdr res))))

(define-primitive (fx= si env arg1 arg2)
  (let ([res (emit-args-and-save* si env arg1 arg2)])
    (emit-compare= (cdr res))))

(define-primitive (fx< si env arg1 arg2)
  (emit-binary-compare si env arg1 arg2 "setl" "setg"))

(define-primitive (fx<= si env arg1 arg2)
  (emit-binary-compare si env arg1 arg2 "setle" "setge"))

(define-primitive (fx> si env arg1 arg2)
  (emit-binary-compare si env arg1 arg2 "setg" "setl"))

(define-primitive (fx>= si env arg1 arg2)
  (emit-binary-compare si env arg1 arg2 "setge" "setle"))

;; The char expressions are exactly the same as fx expressions, given they
;; are simply integer comparisons
(define-primitive (char= si env arg1 arg2)
  (let ([res (emit-args-and-save* si env arg1 arg2)])
    (emit-compare= (cdr res))))

(define-primitive (char< si env arg1 arg2)
  (emit-binary-compare si env arg1 arg2 "setl" "setg"))

(define-primitive (char<= si env arg1 arg2)
  (emit-binary-compare si env arg1 arg2 "setle" "setge"))

(define-primitive (char> si env arg1 arg2)
  (emit-binary-compare si env arg1 arg2 "setg" "setl"))

(define-primitive (char>= si env arg1 arg2)
  (emit-binary-compare si env arg1 arg2 "setge" "setle"))

;; Helper functions used for primitives
;; ------------------------------------
(define (emit-binary-compare si env arg1 arg2 cmp/t cmp/f)
  ;; compare the two arguments for the binary primitive; use comparison
  ;; cmp/t if arg1reg is #t and cmp/f if arg1reg is #f
  (let ([res (emit-args-and-save* si env arg1 arg2)])
    ;; if #f, we check arg2 <= arg1
    (emit-compare (if (car res) cmp/t cmp/f) (cdr res))))

(define (emit-compare cmp val)
  ;; Use comparison instruction cmp to set return value to #t or #f
  (emit "    cmp eax,   ~a" val)        ; compare eax to val
  (emit "    ~a al" cmp)                ; 1 if condition
  (emit "    movsx eax, al")            ; extend to 32 bits with sign extend
  (emit "    sal eax, ~d" imm/bool-bit) ; #t and #f differ only in this bit
  (emit "    or  eax, ~s" imm/bool-false))

;; emit-compare= and emit-mask-compare= don't use si
(define (emit-compare= val)
  ;; Generate eax <- eax == val ? #t : #f. val should be a number.
  (emit-compare "sete" val))

(define (emit-mask-compare= mask val)
  ;; eax <- (eax & mask) == val ? 1 : 0
  (emit "    and eax, ~d" mask)
  (emit-compare= val))

(define (emit-args-and-save si env arg1 arg2)
  (emit-expr si env arg2)
  (emit-stack-save si)
  (emit-expr (- si wordsize) env arg1))

(define (emit-args-and-save* si env arg1 arg2)
  ;; Evaluates the args, taking care of immediates.
  ;;
  ;; Returns a cons cell of the form (arg1reg? . otherArgValue). If arg1reg is
  ;; #t, then arg1 is contained in eax, and otherArgValue is a string
  ;; representation of arg2's value. Alternatively, if arg1reg is #f, arg2 is
  ;; in eax, and otherArgValue is a string representation of arg1.
  (if (immediate? arg2)
      (begin
        (emit-expr si env arg1)         ; arg1 -> eax
        (cons #t (format "~a" (immediate-rep arg2))))
      (if (immediate? arg1)
          (begin
            (emit-expr si env arg2)     ; arg2 -> eax
            (cons #f (format "~a" (immediate-rep arg1))))
          (begin
            (emit-expr si env arg2)          ; arg2 -> eax
            (emit-stack-save si)             ; eax  -> stack
            (emit-expr (- si wordsize) env arg1) ; arg1 -> eax
            (cons #t (esp-expr si))))))

;; ======================================================================
;; Immediates
;; ======================================================================
;; fixnum is a signed 30 bit value with 2 LSB==0b00
;; Boolean is #f == 0b00101111 (0x2F); #t == 0b01101111 (0x6F)
;;   (mask := 0b10111111, result is 0b00101111) [bool & #f == #f]
;; Character (<charByte> << 8) | 0b00001111 (15, 0xF)
;; Empty list (): 0b00111111 (63, 0x3F)

(define imm/bool-true      #x6F)
(define imm/bool-false     #x2F)
;; bools are equal to #f in all bits except bit 6.
(define imm/bool-mask      #xFFFFFFBF)
(define imm/bool-bit       6)           ; only difference in this bit
;; a valid char should have top two bytes == 0, bottom byte == 0xF
(define imm/char-mask      #xFFFF00FF)
(define imm/char-tag  #x0F)
(define imm/null-val       #x3F)

(define (bool-rep b) (if b imm/bool-true imm/bool-false))

(define (char-rep c)
  (bitwise-ior (bitwise-arithmetic-shift (char->integer c) 8) imm/char-tag))

(define (immediate-rep x)
  ((cond
    [(null?    x) (lambda (x) imm/null-val)]
    [(fixnum?  x) fxnum-rep]
    [(boolean? x) bool-rep ]
    [(char?    x) char-rep ]
    [else (error 'immediate-rep "Unhandled type" x)]
    ) x))

;; Fixnums
;; =======
;; Fixnums. Note that our scheme uses 30 bit fixnums, while the
;; implementation language necessary does not. So we cannot rely on
;; the input being a fixnum.  Instead, we assume it is an integer, and
;; make sure it is within the max and min values.
(define wordsize 4)                     ; # bytes in a word
(define wordbits (* wordsize 8))        ; # bits  in a word
(define fx/bits 30)
(define fx/min-val (- 0 (bitwise-arithmetic-shift 1 (- fx/bits 1))))
(define fx/max-val (- (bitwise-arithmetic-shift 1 (- fx/bits 1)) 1))
(define imm/fx-mask (- (expt 2 (- wordbits fx/bits)) 1)) ; bottom 2 bits

(define (fixnum? x)
  (and (integer? x)
       (<= fx/min-val x fx/max-val)))
;; Convert to our fixnum
(define (fxnum-rep n) (bitwise-arithmetic-shift n 2))

(define immediate?
  (lambda (x) (or (null? x) (fixnum? x) (boolean? x) (char? x))))

(define (emit-immediate expr)
   (emit "    mov eax, ~s" (immediate-rep expr)))

;; ======================================================================
;; Local variables
;; ======================================================================
;; let-like expressions (controlled by name)
(define (let-expr? name expr)
  (define (binding? expr)
    ;; list of two elems whose first is a symbol
    (and (list? expr) (= (length expr) 2) (symbol? (car expr))))
  (define (bindings? expr)              ; a possibly empty list of bindings
    (and (list? expr) (for-all binding? expr)))
  (and (list? expr)
       (> (length expr) 2)              ; at least 3 elements
       (eqv? (car expr) name)
       (bindings? (cadr expr))))        ; 2nd element should be bindings

(define (let?    expr) (let-expr? 'let    expr))
(define (let*?   expr) (let-expr? 'let*   expr))
(define (letrec? expr) (let-expr? 'letrec expr))

;; A variable on the (run-time) stack will return a string representation
;; of the value. This can be used either for a load or a store
(define variable? symbol?)

;; The environment value for a variable is its si index. Thus processing a
;; variable reference is simply loading it into eax from its stack position.
(define (emit-vbl-ref env var)
  (let ([si (lookup-variable var env)])
    (if (integer? si) (emit-stack-load si)
        (error 'emit-variable "Variable not found" var))))

(define (emit-let si env expr)
  (define (let-body     let-expr) (cddr let-expr))
  (define (let-bindings let-expr) (cadr let-expr))
  ;; process bindings, one at a time
  (let pb ([bindings (let-bindings expr)]
           [si       si                 ]
           [new-env  env                ])
    (cond
     [(null? bindings) (emit-exprs si new-env (let-body expr))]
     [else
      (let ([b (car bindings)])         ; b -> '(var-name binding-expr)
        ;; Though we add bindings, pass the original env here. All env lookups
        ;; on the rhs of a binding expression happen in the original env. This
        ;; contrasts to let*, for which lookups are in the extended env.
        (emit-expr si env (cadr b))     ; binding value -> eax
        (emit-stack-save si)            ; save eax -> stack
        (pb (cdr bindings)              ; process the rest with new env
            (- si wordsize)
            (extend-env (car b) si new-env)))])))

(define (emit-let* si env expr)
  (define (let-body     let-expr) (cddr let-expr))
  (define (let-bindings let-expr) (cadr let-expr))
  ;; process bindings, one at a time
  (let pb ([bindings (let-bindings expr)]
           [si       si                 ]
           [new-env  env                ])
    (cond
     [(null? bindings) (emit-exprs si new-env (let-body expr))]
     [else
      (let ([b (car bindings)])         ; b -> '(var-name binding-expr)
        ;; Pass the extended environment here. Subsequent binding expressions
        ;; use the new environment (all bindings that have happened so far in
        ;; this let*) and thus use variables defined there.  This contrasts to
        ;; let, for which lookups are in the original env.
        (emit-expr si new-env (cadr b)) ; binding value -> eax
        (emit-stack-save si)            ; save eax -> stack
        (pb (cdr bindings)              ; process the rest with new env
            (- si wordsize)
            (extend-env (car b) si new-env)))])))
;; ======================================================================
;; Procedures
;; ======================================================================
(define (lambda? expr)
  ;; One or more variables
  ;;   TODO: (lambda x (length x)) form
  ;;   TODO: (lambda (x . z) (+ (length x) (length z))) form
  ;; One or more body expressions
  (and (list? expr) (> (length expr) 2)
       (equal? (car expr) 'lambda)
       (let ([vars (cadr expr)])
             (or (null? vars)
                 (and (list? vars) (for-all symbol? vars))))))

(define (emit-letrec expr)
  (define letrec-body cddr)
  (let* ([bindings (cadr expr)]
         [lvars    (map car bindings)]
         [lambdas  (map cadr bindings)]
         [labels   (map (lambda (n) (unique-label)) lvars)]
         [env      (make-initial-env lvars labels)])
    (for-each (emit-lambda env) lambdas labels lvars)
    (emit-scheme-entry env (letrec-body expr))))

(define (emit-lambda env)
  ;; Instead of directly generating a lambda, we generate a function that
  ;; can be used with the given environment. This allows it to be used in
  ;; the for-each in emit-letrec.
  (lambda (expr label lvar)
    ;; When processing the lambda expression, we can encounter either a
    ;; formal, which will be part of the stack (and whose env value will be
    ;; an integer) or a reference to another lambda (whose env value will be
    ;; its label). For a reference to a lambda, we will have to generate a
    ;; call to the given label. We first build up the list of formals with
    ;; the given environment and then generate the body expression with the
    ;; new environment. This is similar to a let expression.
    (emit "")
    (emit "// ~a: ~s" lvar (cadr expr))
    (emit-function-header label)
    (let ([fmls (cadr expr)]
          [body (cddr expr)])
      ;;(display (format "lambda fmls ~a body ~a env ~a\n" fmls body env))
      ;; si starts at -wordsize, since esp is updated for each call
      (let f ([fmls fmls] [si (- wordsize)] [env env])
        (if (null? fmls)
            (emit-exprs si env body)
            (f (cdr fmls) (- si wordsize) (extend-env (car fmls) si env)))))
    (emit "    ret")))

(define (emit-app si env expr)
  (define (emit-arguments si args)
    ;;(display (format "emit-args ~a ~a\n" si args))
    (if (null? args)
        si
        (begin
          (emit-expr si env (car args))
          (emit-stack-save si)
          (emit-arguments (- si wordsize) (cdr args)))))

  ;(display (format "\nemit-app ~a ~a ~a\n" si env expr))
  ;; We find si-wordsize to leave one spot for return address
  (let ([lbl (lookup-variable (car expr) env)])
    (emit-arguments (- si wordsize) (cdr expr))
    (emit-adjust-stack (+ si wordsize))
    (if (string? lbl)
        (emit "    call ~a" lbl)
        (error 'emit-app "Unknown procedure" (car expr)))
    (emit-adjust-stack (- (+ si wordsize)))
    ))

;; xxx should check if the function is already defined
(define (app? expr)
  (let ([res (and (pair? expr) (symbol? (car expr)))])
    res))

(define (make-initial-env symbols values)
  (map cons symbols values))

;; ======================================================================
;; Conditionals
;; ======================================================================
;; ====== if =====
(define (if? expr)
  (and (pair? expr) (symbol? (car expr)) (eqv? (car expr) 'if)))

(define (emit-if si env expr)
  (unless (= (length expr) 4)           ; if,test,conseq,alternative
          (error 'emit-if "Ill formed if expression" expr))
  (let ([test        (cadr   expr)]
        [consequent  (caddr  expr)]
        [alternative (cadddr expr)]
        [alt-label   (unique-label)]
        [end-label   (unique-label)])
    (emit-expr si env test)
    (emit "    cmp eax, ~s" imm/bool-false)
    (emit "    je  ~a" alt-label)
    (emit-expr si env consequent)
    (emit "    jmp ~a" end-label)
    (emit "~a:" alt-label)
    (emit-expr si env alternative)
    (emit "~a:" end-label)))
;; ===== and /or =====
(define (and-or? expr)
  (and (pair? expr) (symbol? (car expr))
       (or (eqv? (car expr) 'and) (eqv? (car expr) 'or))))

(define (emit-and-or si env expr)
  (let ([end-label (unique-label)]
        [cmp-instr (if (eqv? (car expr) 'and) "je" "jne")])
    ;; no need to initialize if and/or has arguments
    (if (null? (cdr expr))
        (emit "    mov eax, ~s"
              (if (eqv? (car expr) 'and) imm/bool-true imm/bool-false)))

    ;; and-term makes recursive call, thus needs a copy of si (as func args)
    (let and-term ([si   si        ]
                   [rest (cdr expr)])   ; car == 'and/'or
      (if (not (null? rest))
          (begin
            (emit-expr si env (car rest))
            ;; If (car rest) is the only term, and/or should return it.
            ;; Thus we compare to #f only if it not the last term
            (if (not (null? (cdr rest)))
                (begin
                  (emit "    cmp eax, ~s" imm/bool-false)
                  ;; if eax is not #f, it contains the return value
                  (emit "    ~a ~a" cmp-instr end-label)
                  (and-term si (cdr rest)))))))
    (emit "~a:" end-label)))

;; ======================================================================
;; Helper functions
;; ======================================================================
;; ==== Stack functions ====
(define (esp-expr si)
  ;; An expression representing a stack value, can be used for load/save
  ;; si is always negative in our case, but we are extra safe
  (format (if (< si 0) "[esp-~a]" "[esp+~a]") (abs si)))

(define (emit-stack-save si)
  (emit "    mov ~a, eax" (esp-expr si)))

(define (emit-stack-load si)
  (emit "    mov eax, ~a" (esp-expr si)))

(define (emit-adjust-stack inc)
  ;(display (format "Stack adj ~a\n" inc))
  (cond [(< inc 0) (emit "    sub esp, ~a" (- inc))]
        [(> inc 0) (emit "    add esp, ~a" inc)]))

;; ==== Utility functions ====
(define unique-label
  (let ([count 0])
    (lambda ()
      (let ([L (format "L_~s" count)])
        (set! count (+ 1 count))
        L))))

;; ==== Environments ====
;; An environment is represented by the stack offset it has. The lookup
;; function will return the stack offset, or #f if not found.
(define (lookup-variable var env)
  (let ([p (assoc var env)])
    (if p (cdr p) #f)))

(define (extend-env var si env)
  (cons (cons var si) env))

;; ======================================================================
;; Main program
;; ======================================================================
(define (emit-function-header name)
  (emit "~a:" name))

;; Call emit-expr for each element of the expression list exprs, passing in si
;; and env each time. Discard the result of all but the last expression.
(define (emit-exprs si env exprs)
  ;;(display (format "e-es ~a ~a ~a" si env exprs))
  (unless (null? exprs)
          (emit-expr  si env (car exprs))
          (emit-exprs si env (cdr exprs))))

;; all the cases except emit-immediate can possibly use the stack (if only
;; through recursive calls to emit-primcall). emit-immediate never makes a
;; recursive call, but we pass in si in any case.
(define (emit-expr si env expr)
  ;;(display (format "e-ex ~a ~a ~a" si env expr))
  (cond
   [(immediate? expr) (emit-immediate        expr)]
   [(variable?  expr) (emit-vbl-ref      env expr)]
   [(if?        expr) (emit-if        si env expr)]
   [(primcall?  expr) (emit-primcall  si env expr)]
   [(let?       expr) (emit-let       si env expr)]
   [(let*?      expr) (emit-let*      si env expr)]
   [(and-or?    expr) (emit-and-or    si env expr)]
   [(app?       expr) (emit-app       si env expr)]
   [else (error 'emit-expr "Unknown expression type" expr)]))

(define (emit-scheme-entry env exprs)
  (emit "_scheme_entry:")
  (emit "    mov ecx, esp")             ; ecx is a scratch register
  (emit "    mov esp, [esp + 4]")       ; stack base in argument
  (emit "    call L_scheme_entry")
  (emit "    mov esp, ecx")             ; restore original stack
  (emit "    ret")
  (emit "")
  (emit-function-header "L_scheme_entry")
  (emit-exprs (- wordsize) env exprs)
  (emit "    ret"))

(define (emit-program expr)
  (emit "    .text")
  (emit "    .intel_syntax noprefix")
  (emit "    .globl _scheme_entry")
  (if (letrec? expr)
      (emit-letrec expr)          ; which eventually calls emit-scheme-entry
      (emit-scheme-entry '() (list expr))))
