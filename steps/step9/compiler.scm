(load "../../src/tests-driver.scm")
(load "../../src/tests-1.9.2-req.scm")
(load "../../src/tests-1.9.1-req.scm")
(load "../../src/tests-1.8-req.scm")
(load "../../src/tests-1.7-req.scm")
(load "../../src/tests-1.6-req.scm")
(load "../../src/tests-1.6-opt.scm")
(load "../../src/tests-1.5-req.scm")
(load "../../src/tests-1.4-req.scm")
(load "../../src/tests-1.3-req.scm")
(load "../../src/tests-1.2-req.scm")
(load "../../src/tests-1.1-req.scm")

(define emit-comment? #t)

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

(define (emit-tail-primcall si env expr)
  (emit-primcall si env expr) (emit-ret))

(define (alias-primitive from to)
  (unless (primitive? from)
          (error 'alias-primitive "Not a primitive" from))
  (let addprop ([l (property-list from)])
    (unless (null? l)
            (putprop to (car l) (cadr l))
            (addprop (cddr l)))))

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

;; Pairs and other data types
;; --------------------------
(define-primitive (pair? si env arg)
  (emit-expr si env arg)
  (emit-mask-compare= data-mask pair-tag))

(define-primitive (closure? si env arg)
  (emit-expr si env arg)
  (emit-mask-compare= data-mask closure-tag))

(define-primitive (symbol? si env arg)
  (emit-expr si env arg)
  (emit-mask-compare= data-mask symbol-tag))

(define-primitive (vector? si env arg)
  (emit-expr si env arg)
  (emit-mask-compare= data-mask vector-tag))

(define-primitive (string? si env arg)
  (emit-expr si env arg)
  (emit-mask-compare= data-mask string-tag))

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
  (emit "    mul dword ptr ~a" (esp-ptr si)))

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

(define-primitive (eq? si env arg1 arg2)
  (let ([res (emit-args-and-save* si env arg1 arg2)])
    (emit-compare= (cdr res))))

;; Pairs and other data types
;; --------------------------
(define-primitive (cons si env arg1 arg2)
  (emit "    // args ~a ~a for cons-ing" arg1 arg2)
  (emit-args-and-save si env arg1 arg2)
  (emit "    // (cons ~a ~a)" arg1 arg2)
  ;; arg1 is in EAX, arg2 in stack
  (emit "    mov [ebp], eax")
  (emit "    mov eax, ~a" (esp-ptr si))
  (emit "    mov [ebp+~a], eax" wordsize)
  (emit "    mov eax, ebp")             ; return value of pair
  (emit "    or eax, ~a" pair-tag)      ; mark it as a pair
  (emit "    add ebp, ~a" (* 2 wordsize))) ; bump ebp past the pair

(define-primitive (car si env arg1)
  (emit-expr si env arg1)
  (emit "    mov eax, ~a" (reg-ptr "eax" pair-first)))
(define-primitive (cdr si env arg1)
  (emit-expr si env arg1)
  (emit "    mov eax, ~a" (reg-ptr "eax" pair-second)))
(define-primitive (set-car! si env pair car-val)
  (emit-args-and-save si env pair car-val)
  ;; pair is in EAX, car-val on stack
  ;; We use EDX and clobber it in the process.
  (emit "    mov edx, ~a" (esp-ptr si))
  (emit "    mov ~a, edx" (reg-ptr "eax" pair-first)))
(define-primitive (set-cdr! si env pair cdr-val)
  (emit-args-and-save si env pair cdr-val)
  ;; pair is in EAX, cdr-val on stack
  ;; We use EDX and clobber it in the process.
  (emit "    mov edx, ~a" (esp-ptr si))
  (emit "    mov ~a, edx" (reg-ptr "eax" pair-second)))

;; Vectors
;; -------

;; While the tutorial suggests make-vector with an initializer, the tests use
;; the make-vector without an initializer. Scheme supports both forms.  In
;; order to not lose this, we rename to make-vector-init and implement
;; make-vector separately.
(define-primitive (make-vector-init si env len val)
  ;; We use EDX and EBX as scratch registers
  ;; We could emit-args one at a time and avoid the stack, but we cannot
  ;; be sure that the second call to emit-args will not use EBX.
  (emit-args-and-save si env len val)   ; len in EAX, val on stack
  (emit "    mov ebx, ~a" (esp-ptr si)) ; val in ebx
  (emit "    mov edx, eax")             ; EDX is the working copy
  (emit "    mov [ebp], edx")           ; [EBP] gets the length (as fixnum)
  ;; Now that EAX is no longer used, we can use it for the return value
  (emit "    mov eax, ebp")
  (emit "    or eax, ~a" vector-tag)
  (emit "    add ebp, 4")           ; Now bump ebp
  ;; We need edx as an int now
  (emit "    shr edx, ~a //fx->int" fx/shift)
  ;; Update len to be odd (so len+1 is even)
  (emit "    bt  edx, 0")               ; copy low bit to CF
  (let ([bt-lbl   (unique-label)]
        [done-lbl (unique-label)])
    (emit "    jc ~a // already odd" bt-lbl)
    (emit "    inc edx")
    (emit "~a:" bt-lbl)                 ; bt-lbl used both for loop and bt
    (emit "    cmp edx, 0")
    (emit "    jz ~a" done-lbl)
    (emit "    mov [ebp], ebx")         ; copy the value
    (emit "    add ebp, 4")
    (emit "    dec edx")
    (emit "    jmp ~a // loop" bt-lbl)
    (emit "~a:" done-lbl)))

(define-primitive (make-vector si env len)
  ;; We use EDX and EBX as scratch registers
  (emit-expr si env len)                ; EAX <- len
  (emit "    mov edx, eax")             ; EDX has the working copy of len
  (emit "    mov [ebp], edx")           ; update [EBP] with len (as fixnum)
  ;; As EAX is a fixnum, convert it into C integer
  (emit "    sar eax, ~a // fixnum->int" fx/shift)
  ;; Now that EAX is no longer used, we can use it for the return value
  (emit "    mov eax, ebp")             ; EAX has return value
  (emit "    or eax, ~a" vector-tag)    ; Tag it as a vector
  ;; Update len to be odd (so len+1 is even)
  (emit "    bt  edx, 0")               ; copy low bit to CF
  (let ([bt-lbl   (unique-label)])
    (emit "    jc ~a // already odd" bt-lbl)
    (emit "    inc edx")
    (emit "~a:" bt-lbl)                 ; if no adjustment required
    ;; EDX has the (integer) adjusted length, move EBP past the vector
    (emit "    lea ebp, [ebp + 4*edx + 4]")))

(define-primitive (vector-length si env vec)
  (emit-expr si env vec)                ; vec is in EAX
  ;; Length is already a fixnum
  (emit "    mov eax, [eax-~a]" vector-tag)) ; Change to pointer and deref

(define-primitive (vector-ref si env vec idx)
  (emit-comment-list 'vector-ref vec idx)
  ;; intentionally pass args in reverse order to get idx into eax
  (emit-args-and-save si env idx vec) ; eax <- idx; [esp-si] <- vec
  (emit "    mov edx, ~a" (esp-ptr si)) ; edx has list pointer
  (emit "    add eax, ~a" (fxnum-rep 1)) ; go past length
  (emit "    mov eax, [edx - ~a + eax]" vector-tag)
  )

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

;; Evaluate arg2 and put it on stack. Return arg1 in EAX
(define (emit-args-and-save si env arg1 arg2)
  (emit-expr si env arg2)
  (emit-stack-save si)
  (emit-expr (stack- si) env arg1))

(define (emit-args-and-save* si env arg1 arg2)
  ;; Evaluates the args, taking care of immediates.
  ;;
  ;; Returns a cons cell of the form (arg1reg? . otherArgValue). If arg1reg?
  ;; is #t, then arg1 is contained in eax, and otherArgValue is a string
  ;; representation of arg2's value. Alternatively, if arg1reg? is #f, arg2 is
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
            (emit-expr (stack- si) env arg1) ; arg1 -> eax
            (cons #t (esp-ptr si))))))

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
(define fx/bits 30)                     ; # effective bits in fixnum
(define fx/shift (- wordbits fx/bits))  ; # bits fixnum is shifted by from int
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
(define (emit-tail-immediate expr)
  (emit-immediate expr)
  (emit-ret))

;; ======================================================================
;; Pairs and other data structures
;; ======================================================================
(define data-mask   #b111)
(define pair-tag    #b001)
(define closure-tag #b010)
(define symbol-tag  #b011)
(define vector-tag  #b101)
(define string-tag  #b110)

(define pair-first  (- pair-tag))
(define pair-second (+ pair-first wordsize))

(define (begin? expr)
  (and (pair? expr) (not (null? expr)) (eqv? (car expr) 'begin)))

(define (emit-begin      si env expr)   ; car is 'begin, cdr are expressions
  (emit-exprs      si env (cdr expr)))
(define (emit-tail-begin si env expr)
  (emit-tail-exprs si env (cdr expr)))

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
(define (letrec? expr) (and (let-expr? 'letrec expr)
                            (for-all (lambda (b)
                                       (lambda? (cadr b))) (cadr expr))))

;; A variable on the (run-time) stack will return a string representation
;; of the value. This can be used either for a load or a store
(define variable? symbol?)

;; The environment value for a variable is its si index. Thus processing a
;; variable reference is simply loading it into eax from its stack position.
(define (emit-vbl-ref env var)
  (let ([si (lookup-variable var env)])
    (if (integer? si) (emit-stack-load si)
        (error 'emit-variable "Variable not found" var))))
(define (emit-tail-vbl-ref env var)
  (emit-vbl-ref env var) (emit-ret))

;; emit-let or emit-let* depending on is*? (is*? == #t => emit-let*)
;; emitter defines whether we process it as a tail expression. Note that
;; emitter should process a list of expressions, not a single expression.
(define (emit-let/* si env expr is*? emitter)
  (define (let-body     let-expr) (cddr let-expr))
  (define (let-bindings let-expr) (cadr let-expr))
  ;; process bindings, one at a time
  (let pb ([bindings (let-bindings expr)]
           [si       si                 ]
           [new-env  env                ])
    (cond
     [(null? bindings) (emitter si new-env (let-body expr))]
     [else
      (let ([b (car bindings)])         ; b -> '(var-name binding-expr)
        ;; The only difference between let and let* is that a bound value is
        ;; resolved in the original env for let, while for let* the resolution
        ;; can happen through a variable in the partially expanded/updated env
        ;; (including where a binding just defined overrides one in the
        ;; original env).
        (emit-expr si (if is*? new-env env) (cadr b)) ; binding value -> eax
        (emit-stack-save si)                          ; save eax -> stack
        (pb (cdr bindings)              ; process the rest with new env
            (stack- si)
            (extend-env (car b) si new-env)))])))

(define (emit-let  si env expr) (emit-let/* si env expr #f emit-exprs))
(define (emit-let* si env expr) (emit-let/* si env expr #t emit-exprs))
(define (emit-tail-let  si env expr)
  (emit-let/* si env expr #f emit-tail-exprs))
(define (emit-tail-let* si env expr)
  (emit-let/* si env expr #t emit-tail-exprs))

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

      ;; si starts at -wordsize, since esp is updated for each call
      (let f ([fmls fmls] [si (- wordsize)] [env env])
        (if (null? fmls)
            (emit-tail-exprs si env body)
            (f (cdr fmls) (stack- si) (extend-env (car fmls) si env)))))))

(define (emit-app si env expr)
  (define (emit-arguments si args)
    (unless (null? args)
            (emit-expr si env (car args))
            (emit-stack-save si)
            (emit-arguments (stack- si) (cdr args))))

  (let ([lbl (lookup-variable (car expr) env)])
    (unless (string? lbl)               ; else not a lambda expression
            (error 'emit-app "Unknown procedure" (car expr)))
    ;; We find si-wordsize to leave one spot for return address
    (emit-arguments (stack- si) (cdr expr))
    (emit-adjust-stack (stack+ si))
    (emit "    call ~a" lbl)
    (emit-adjust-stack (- (stack+ si)))))

(define (emit-tail-app si env expr)
  (define (emit-arguments si args)
    (unless (null? args)
            (emit-expr si env (car args))
            (emit-stack-save si)
            (emit-arguments (stack- si) (cdr args))))
  ;; xxx todo: what if dest == src?
  (define (copy-arguments dest src exprs)
    (unless (null? exprs)
            (emit-stack-load src)
            (emit-stack-save dest)
            (copy-arguments (stack- dest) (stack- src) (cdr exprs))))

  (let ([lbl (lookup-variable (car expr) env)])
    (unless (string? lbl)               ; else not a lambda expression
            (error 'emit-app "Unknown procedure" (car expr)))
    ;; We don't need to keep any space for the return address; thus use si
    (emit-arguments si (cdr expr))
    (copy-arguments (- wordsize) si (cdr expr))
    (emit "    jmp ~a // tail call" lbl)
    (emit "")))

(define (app? expr)
  (and (pair? expr) (symbol? (car expr))))

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

(define (emit-tail-if si env expr)
  (unless (= (length expr) 4)           ; if,test,conseq,alternative
          (error 'emit-if "Ill formed if expression" expr))
  (let ([test        (cadr   expr) ]
        [consequent  (caddr  expr) ]
        [alternative (cadddr expr) ]
        [alt-label   (unique-label)])
    (emit-expr si env test)
    (emit "    cmp eax, ~s" imm/bool-false)
    (emit "    je  ~a" alt-label)
    (emit "    // consequent for ~s" test)
    (emit-tail-expr si env consequent)
    ;; no need for a jump to the end since it is a tail call
    (emit "~a:" alt-label)
    (emit "    // alternative for ~s" test)
    (emit-tail-expr si env alternative)
    ;; No need for end label either
    (emit "")))

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

(define (emit-tail-and-or si env expr) (emit-and-or si env expr) (emit-ret))
;; ======================================================================
;; Helper functions
;; ======================================================================
;; ==== Utilities ====
(define (expr-from-file fn)
  ;; Read the expression from file with name fn
  (with-input-from-file fn (lambda () (eval (read)))))

;; ==== Stack functions ====
(define (esp-ptr si)
  (reg-ptr "esp" si))

(define (reg-ptr reg si)
  ;; Expression for a pointer to a register, with an offset.
  (format (if (< si 0) "[~a-~a]" "[~a+~a]") reg (abs si)))

(define (emit-stack-save si)
  (emit "    mov ~a, eax" (esp-ptr si)))

(define (emit-stack-load si)
  (emit "    mov eax, ~a" (esp-ptr si)))

(define (emit-adjust-stack inc)
  (cond [(< inc 0) (emit "    sub esp, ~a" (- inc))]
        [(> inc 0) (emit "    add esp, ~a" inc)]))

(define (stack- si) (- si wordsize))
(define (stack+ si) (+ si wordsize))

;; ==== Miscellaneous functions ====
(define (emit-ret) (emit "    ret") (emit ""))

;; Just like emit, but does nothing
(define (emit-null . x) #f)

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
  (unless (null? exprs)
          (emit-expr  si env (car exprs))
          (emit-exprs si env (cdr exprs))))
;; Call emit-expr for each element of the list exprs. Call emit-tail-expr for
;; the last element. Assumes exprs has at least one expression.
(define (emit-tail-exprs si env exprs)
  (if (null? (cdr exprs))
      (emit-tail-expr si env (car exprs))
      (begin
        (emit-expr si env (car exprs))
        (emit-tail-exprs si env (cdr exprs)))))

;; all the cases except emit-immediate can possibly use the stack (if only
;; through recursive calls to emit-primcall). emit-immediate never makes a
;; recursive call, but we pass in si in any case.
(define (emit-expr si env expr)
  (cond
   [(immediate? expr) (emit-immediate        expr)]
   [(variable?  expr) (emit-vbl-ref      env expr)]
   [(if?        expr) (emit-if        si env expr)]
   [(begin?     expr) (emit-begin     si env expr)]
   [(primcall?  expr) (emit-primcall  si env expr)]
   [(let?       expr) (emit-let       si env expr)]
   [(let*?      expr) (emit-let*      si env expr)]
   [(and-or?    expr) (emit-and-or    si env expr)]
   [(app?       expr) (emit-app       si env expr)]
   [else (error 'emit-expr "Unknown expression type" expr)]))

;; all the cases except emit-immediate can possibly use the stack (if only
;; through recursive calls to emit-primcall). emit-immediate never makes a
;; recursive call, but we pass in si in any case.
(define (emit-tail-expr si env expr)
  (cond
   [(immediate? expr) (emit-tail-immediate        expr)]
   [(variable?  expr) (emit-tail-vbl-ref      env expr)]
   [(if?        expr) (emit-tail-if        si env expr)]
   [(begin?     expr) (emit-tail-begin     si env expr)]
   [(primcall?  expr) (emit-tail-primcall  si env expr)]
   [(let?       expr) (emit-tail-let       si env expr)]
   [(let*?      expr) (emit-tail-let*      si env expr)]
   [(and-or?    expr) (emit-tail-and-or    si env expr)]
   [(app?       expr) (emit-tail-app       si env expr)]
   [else (error 'emit-tail-expr "Unknown expression type" expr)]))

(define (emit-scheme-entry env exprs)
  (emit-function-header "R_scheme_entry")
  (emit-exprs (- wordsize) env exprs)
  (emit-ret))

(define (emit-program expr)
  ;; file header
  (emit "    .text")
  (emit "    .intel_syntax noprefix")
  (emit "    .globl R_scheme_entry")
  (if (letrec? expr)
      (emit-letrec expr)          ; which eventually calls emit-scheme-entry
      (emit-scheme-entry '() (list expr))))
