(load "../../src/tests-driver.scm")
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
    [(_ (prim-name si arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count* (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
                (lambda (si arg* ...) b b* ...)))]))

(define (primitive? x) (and (symbol? x) (getprop x '*is-prim*)))
(define (primitive-emitter x)
  (or (getprop x '*emitter*)
      (error 'primitive-emitter "No emitter defined" x)))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (emit-primcall si expr)
  (let ([prim (car expr)]
        [args (cdr expr)])              ; args includes "si"
    (define (check-primcall-args prim args)
      (unless (= (length args) (getprop prim '*arg-count*))
              (error 'emit-primcall "Invalid # args"
                     ((length args) . (getprop prim '*arg-count*)))))
    ;;(display (list prim args (length args)))
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) (cons si args))))

;; Unary primitives definition
;; ===========================
(define-primitive (fxadd1 si arg)
  (emit-expr si arg)
  (emit "    add eax, ~s" (immediate-rep 1)))

(define-primitive (fxsub1 si arg)
  (emit-expr si arg)
  (emit "    sub eax, ~s" (immediate-rep 1)))

(define-primitive (char->fixnum si arg)
  (emit-expr si arg)
  (emit "    sar eax, 6"))

(define-primitive (fixnum->char si arg)
  (emit-expr si arg)
  (emit "    shl eax, 6")
  (emit "    or  eax, ~d" imm/char-tag))

(define-primitive (fxzero? si arg)
  (emit-expr si arg)
  (emit-compare= 0))

(define-primitive (null? si arg)
  (emit-expr si arg)
  (emit-compare= imm/null-val))

(define-primitive (fixnum? si arg)
  (emit-expr si arg)
  (emit-mask-compare= imm/fx-mask 0))    ; last two bits should be 00b

(define-primitive (boolean? si arg)
  (emit-expr si arg)
  (emit-mask-compare= imm/bool-mask imm/bool-false))

(define-primitive (char? si arg)
  (emit-expr si arg)
  (emit-mask-compare= imm/char-mask imm/char-tag))

(define-primitive (not si arg)
  (emit-expr si arg)
  ;; if (eax != #f) eax = #t
  (emit-compare= imm/bool-false))

(define-primitive (fxlognot si arg)
  (emit-expr si arg)
  (emit "   not eax")
  ;; mask with all but bottom two bits
  (emit "   and eax, ~d"
        (bitwise-xor (- (expt 2 wordbits) 1) imm/fx-mask)))

;; Binary primitives definition
;; ============================
(define-primitive (fx+ si arg1 arg2)
  (let ([res (emit-args-and-save* si arg1 arg2)])
    (emit "    add eax, ~a" (cdr res))))

(define-primitive (fx- si arg1 arg2)
  ;; sub is not symmetric, so we need to pay attention to what is
  ;; in the register
  (let* ([res (emit-args-and-save* si arg1 arg2)])
    (emit "    sub eax, ~a" (cdr res))
    (if (not (car res))                 ; we just found arg2 - arg1
        (emit "    neg eax"))))

(define-primitive (fx* si arg1 arg2)
  ;; mul cannot deal with immediates
  (emit-args-and-save si arg1 arg2)
  (emit "    sar eax, ~a" (- wordbits fx/bits))
  (emit "    mul dword ptr ~a" (esp-expr si)))

(define-primitive (fxlogand si arg1 arg2)
  (let* ([res (emit-args-and-save* si arg1 arg2)])
    (emit "    and eax, ~a" (cdr res))))

(define-primitive (fxlogor si arg1 arg2)
  (let ([res (emit-args-and-save* si arg1 arg2)])
    (emit "    or  eax, ~a" (cdr res))))

(define-primitive (fx= si arg1 arg2)
  (let ([res (emit-args-and-save* si arg1 arg2)])
    (emit-compare= (cdr res))))

(define-primitive (fx< si arg1 arg2)
  (emit-binary-compare si arg1 arg2 "setl" "setg"))

(define-primitive (fx<= si arg1 arg2)
  (emit-binary-compare si arg1 arg2 "setle" "setge"))

(define-primitive (fx> si arg1 arg2)
  (emit-binary-compare si arg1 arg2 "setg" "setl"))

(define-primitive (fx>= si arg1 arg2)
  (emit-binary-compare si arg1 arg2 "setge" "setle"))

;; The char expressions are exactly the same as fx expressions, given they
;; are simply integer comparisons
(define-primitive (char= si arg1 arg2)
  (let ([res (emit-args-and-save* si arg1 arg2)])
    (emit-compare= (cdr res))))

(define-primitive (char< si arg1 arg2)
  (emit-binary-compare si arg1 arg2 "setl" "setg"))

(define-primitive (char<= si arg1 arg2)
  (emit-binary-compare si arg1 arg2 "setle" "setge"))

(define-primitive (char> si arg1 arg2)
  (emit-binary-compare si arg1 arg2 "setg" "setl"))

(define-primitive (char>= si arg1 arg2)
  (emit-binary-compare si arg1 arg2 "setge" "setle"))


;; Helper functions used for primitives
;; ------------------------------------
(define (emit-binary-compare si arg1 arg2 cmp/t cmp/f)
  ;; compare the two arguments for the binary primitive; use comparison
  ;; cmp/t if arg1reg is #t and cmp/f if arg1reg is #f
  (let ([res (emit-args-and-save* si arg1 arg2)])
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

(define (esp-expr si)
  ;; si is always negative in our case, but we are extra safe
  (format (if (< si 0) "[esp-~a]" "[esp+~a]") (abs si)))

(define (emit-save-eax si)
  (emit "    mov ~a, eax" (esp-expr si)))

(define (emit-args-and-save si arg1 arg2)
  (emit-expr si arg2)
  (emit-save-eax si)
  (emit-expr (- si wordsize) arg1))

(define (emit-args-and-save* si arg1 arg2)
  ;; Evaluates the args, taking care of immediates.
  ;;
  ;; Returns a cons cell of the form (arg1reg? . otherArgValue). If arg1reg is
  ;; #t, then arg1 is contained in eax, and otherArgValue is a string
  ;; representation of arg2's value. Alternatively, if arg1reg is #f, arg2 is
  ;; in eax, and otherArgValue is a string representation of arg1.
  (if (immediate? arg2)
      (begin
        (emit-expr si arg1)             ; arg1 -> eax
        (cons #t (format "~a" (immediate-rep arg2))))
      (if (immediate? arg1)
          (begin
            (emit-expr si arg2)         ; arg2 -> eax
            (cons #f (format "~a" (immediate-rep arg1))))
          (begin
            (emit-expr si arg2)         ; arg2 -> eax
            (emit-save-eax si)          ; eax  -> stack
            (emit-expr (- si wordsize) arg1) ; arg1 -> eax
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

(define immediate?
  (lambda (x) (or (null? x) (fixnum? x) (boolean? x) (char? x))))

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

;; ======================================================================
;; Conditionals
;; ======================================================================
(define unique-label
  (let ([count 0])
    (lambda ()
      (let ([L (format "L_~s" count)])
        (set! count (+ 1 count))
        L))))

;; ====== if =====
(define (if? expr)
  (and (pair? expr) (symbol? (car expr)) (eqv? (car expr) 'if)))

(define (emit-if si expr)
  (unless (= (length expr) 4)           ; if,test,conseq,alternative
          (error 'emit-if "Ill formed if expression" expr))
  (let ([test        (cadr   expr)]
        [consequent  (caddr  expr)]
        [alternative (cadddr expr)]
        [alt-label   (unique-label)]
        [end-label   (unique-label)])
    (emit-expr si test)
    (emit "    cmp eax, ~s" imm/bool-false)
    (emit "    je  ~a" alt-label)
    (emit-expr si consequent)
    (emit "    jmp ~a" end-label)
    (emit "~a:" alt-label)
    (emit-expr si alternative)
    (emit "~a:" end-label)))
;; ===== and /or =====
(define (and-or? expr)
  (and (pair? expr) (symbol? (car expr))
       (or (eqv? (car expr) 'and) (eqv? (car expr) 'or))))

(define (emit-and-or si expr)
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
            (emit-expr si (car rest))
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
;; Main program
;; ======================================================================
(define (emit-function-header name)
  (emit "")                             ; blank line
  (emit "~a:" name))

(define (emit-immediate si expr)
   (emit "    mov eax, ~s" (immediate-rep expr)))

;; all the cases except emit-immediate can possibly use the stack (if only
;; through recursive calls to emit-primcall. emit-immediate never makes a
;; recursive call, but we pass in si in any case.
(define (emit-expr si expr)
  (cond
   [(immediate? expr) (emit-immediate si expr)]
   [(if?        expr) (emit-if        si expr)]
   [(primcall?  expr) (emit-primcall  si expr)]
   [(and-or?    expr) (emit-and-or    si expr)]
   [else (error 'emit-expr "Neither immediate nor primcall" expr)]))

(define (emit-program expr)
  (emit "    .text")
  (emit "    .intel_syntax noprefix")
  (emit "    .globl _scheme_entry")
  (emit "_scheme_entry:")
  (emit "    mov ecx, esp")             ; ecx is a scratch register
  (emit "    mov esp, [esp + 4]")       ; stack base in argument
  (emit "    call L_scheme_entry")
  (emit "    mov esp, ecx")             ; restore original stack
  (emit "    ret")
  (emit-function-header "L_scheme_entry")
  (emit-expr (- wordsize) expr)         ; si initialized to -4
  (emit "    ret"))
