(load "../../src/tests-driver.scm")
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
                (lambda (arg* ...) b b* ...)))]))

(define (primitive? x) (and (symbol? x) (getprop x '*is-prim*)))
(define (primitive-emitter x)
  (or (getprop x '*emitter*)
      (error 'primitive-emitter "No emitter defined" x)))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (emit-primcall expr)
  (let ([prim (car expr)] [args (cdr expr)])
    (define (check-primcall-args prim args)
      (unless (= (length args) (getprop prim '*arg-count*))
              (error 'emit-primcall "Invalid # args"
                     ((length args) . (getprop prim '*arg-count*)))))
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) args)))

;; Primitives definition
;; =====================
(define-primitive (fxadd1 si arg)
  (emit-expr arg)
  (emit "    add eax, ~s" (immediate-rep 1)))

(define-primitive (fxsub1 si arg)
  (emit-expr arg)
  (emit "    sub eax, ~s" (immediate-rep 1)))

(define-primitive (char->fixnum si arg)
  (emit-expr arg)
  (emit "    sar eax, 6"))

(define-primitive (fixnum->char si arg)
  (emit-expr arg)
  (emit "    shl eax, 6")
  (emit "    or  eax, ~d" imm/char-tag))

(define-primitive (fxzero? si arg)
  (emit-expr arg)
  (emit-compare 0))

(define-primitive (null? si arg)
  (emit-expr arg)
  (emit-compare imm/null-val))

(define-primitive (fixnum? si arg)
  (emit-expr arg)
  (emit-mask-compare imm/fx-mask 0))    ; last two bits should be 00b

(define-primitive (boolean? si arg)
  (emit-expr arg)
  (emit-mask-compare imm/bool-mask imm/bool-false))

(define-primitive (char? si arg)
  (emit-expr arg)
  (emit-mask-compare imm/char-mask imm/char-tag))

(define-primitive (not si arg)
  (emit-expr arg)
  ;; if (eax != #f) eax = #t
  (emit-compare imm/bool-false))

(define-primitive (fxlognot si arg)
  (emit-expr arg)
  (emit "   not eax")
  ;; mask with all but bottom two bits
  (emit "   and eax, ~d"
        (bitwise-xor (- (expt 2 wordsize) 1) imm/fx-mask)))

;; Helper functions used for primitives
;; ------------------------------------
(define (emit-compare val)
  ;; Generate eax <- eax == val ? 1 : 0. val should be a number.
  (emit "    cmp eax,   ~d" val)        ; compare eax to val
  (emit "    sete al")                  ; 1 if equal
  (emit "    movsx eax, al")            ; extend to 32 bits with sign extend
  (emit "    sal eax, ~d" imm/bool-bit) ; #t and #f differ only in this bit
  (emit "    or  eax, ~s" imm/bool-false))

(define (emit-mask-compare mask val)
  ;; eax <- (eax & mask) == val ? 1 : 0
  (emit "    and eax, ~d" mask)
  (emit-compare val))

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
(define wordsize 32)
(define fx/bits 30)
(define fx/min-val (- 0 (bitwise-arithmetic-shift 1 (- fx/bits 1))))
(define fx/max-val (- (bitwise-arithmetic-shift 1 (- fx/bits 1)) 1))
(define imm/fx-mask (- (expt 2 (- wordsize fx/bits)) 1)) ; bottom 2 bits

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

(define (emit-if expr)
  (unless (= (length expr) 4)           ; if,test,conseq,alternative
          (error 'emit-if "Ill formed if expression" expr))
  (let ([test        (cadr   expr)]
        [consequent  (caddr  expr)]
        [alternative (cadddr expr)]
        [alt-label   (unique-label)]
        [end-label   (unique-label)])
    (emit-expr test)
    (emit "    cmp eax, ~s" imm/bool-false)
    (emit "    je  ~a" alt-label)
    (emit-expr consequent)
    (emit "    jmp ~a" end-label)
    (emit "~a:" alt-label)
    (emit-expr alternative)
    (emit "~a:" end-label)))
;; ===== and /or =====
(define (and-or? expr)
  (and (pair? expr) (symbol? (car expr))
       (or (eqv? (car expr) 'and) (eqv? (car expr) 'or))))

(define (emit-and-or expr)
  (let ([end-label (unique-label)]
        [cmp-instr (if (eqv? (car expr) 'and) "je" "jne")])
    ;; no need to initialize if and/or has arguments
    (if (null? (cdr expr))
        (emit "    mov eax, ~s"
              (if (eqv? (car expr) 'and) imm/bool-true imm/bool-false)))
    (let and-term ([rest (cdr expr)])   ; car == 'and/'or
      (if (not (null? rest))
          (begin
            (emit-expr (car rest))
            ;; If (car rest) is the only term, and/or should return it.
            ;; Thus we compare to #f only if it not the last term
            (if (not (null? (cdr rest)))
                (begin
                  (emit "    cmp eax, ~s" imm/bool-false)
                  ;; if eax is not #f, it contains the return value
                  (emit "    ~a ~a" cmp-instr end-label)
                  (and-term (cdr rest)))))))
    (emit "~a:" end-label)))

;; ======================================================================
;; Main program
;; ======================================================================
(define (emit-function-header name)
  (emit "")                             ; blank line
  (emit "~a:" name))

(define (emit-immediate expr)
   (emit "    mov eax, ~s" (immediate-rep expr)))

(define (emit-expr expr)
  (cond
   [(immediate? expr) (emit-immediate expr)]
   [(if?        expr) (emit-if        expr)]
   [(primcall?  expr) (emit-primcall  expr)]
   [(and-or?    expr) (emit-and-or    expr)]
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
  (emit-expr expr)
  (emit "    ret"))
