(load "../../src/tests-driver.scm")
(load "../../src/tests-2.1-req.scm")
(load "../../src/tests-1.9.3-req.scm")
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

(define-primitive (procedure? si env arg)
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

(define-primitive (fx/ si env arg1 arg2)
  (emit-args-and-save si env arg1 arg2) ; arg1 in eax, arg2 in esp
  ;; ebx and edx get clobbered
  (emit "    mov ebx, ~a" (esp-ptr si))
  (emit "    xor edx, edx")
  ;; edx:eax / ebx => eax <- Quotient, edx <- remainder
  (emit "    div ebx")
  (emit "    shl eax, ~a" (- wordbits fx/bits)))

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
  ;; Now that EAX is no longer used, we can use it for the return value
  (emit "    mov eax, ebp")             ; EAX has return value
  (emit "    or eax, ~a" vector-tag)    ; Tag it as a vector
  ;; Update len to be odd (so len+1 is even)
  (emit "    bt  edx, 2")               ; copy low bit to CF
  (let ([bt-lbl   (unique-label)])
    (emit "    jc ~a // already odd" bt-lbl)
    (emit "    add edx, 4")
    (emit "~a:" bt-lbl)                 ; if no adjustment required
    ;; EDX has the (integer) adjusted length, move EBP past the vector
    ;; Note edx has the fixnum length, so bytes are 4*len, as we need
    (emit "    lea ebp, [ebp + edx + 4]")))

(define-primitive (vector-length si env vec)
  (emit-expr si env vec)                ; vec is in EAX
  ;; Length is already a fixnum
  (emit "    mov eax, [eax-~a]" vector-tag)) ; Change to pointer and deref

(define-primitive (vector-ref si env vec idx)
  (emit-comment-list 'vector-ref vec idx)
  (emit-args-and-save si env vec idx) ; eax <- vec; [esp-si] <- idx
  (emit "    mov edx, ~a" (esp-ptr si)) ; edx has idx
  ;; Need to go past one element (to go past the "length" item before applying
  ;; the offset of 'idx'. We also need to subtract the vector-tag (0x5) before
  ;; we can treat it as a pointer.  We do both in one shot: add wordsize and
  ;; subtract vector-tag.
  (emit "    mov eax, ~a"
        (reg-ptr "edx+eax" (- wordsize vector-tag))))

(define-primitive (vector-set! si env vec idx val)
  (emit-comment-list 'vector-set)
  (emit-args-and-save si env idx val)   ; eax <- idx, val <- [esp-si]
  ;; We cannot guarantee that emit-expr for vec will not use our
  ;; register. Have to thus save idx onto the stack as well.
  (emit-stack-save (word- si))         ; [esp-si-4] <- idx
  ;; As a side effect, we return the vector
  (emit-expr (word- (word- si)) env vec) ; eax <- vec ptr
  (emit "    mov edx, ~a" (esp-ptr (word- si))) ; edx <- idx (fixnum)
  (emit "    mov ebx, ~a" (esp-ptr si))          ; ebx <- val
  ;; Use (wordsize - string-tag) to go past the length item
  (emit "    mov ~a, ebx" (reg-ptr "edx+eax" (- wordsize vector-tag))))

;; Strings
;; -------
(define-primitive (make-string si env len)
  (emit-comment-list 'make-string len)
  ;; We use EDX and EBX as scratch registers
  (emit-expr si env len)                ; EAX <- len
  (emit "    mov edx, eax")             ; EDX has the working copy of len
  (emit "    mov [ebp], edx")           ; update [EBP] with len (as fixnum)
  ;; Now that EAX is no longer used, we can use it for the return value
  (emit "    mov eax, ebp")             ; EAX has return value
  (emit "    or eax, ~a" string-tag)    ; Tag it as a string
  (emit "    shr edx, 4")               ; convert EDX -> int
  ;; Update len to be a multiple of 4
  (emit "    test edx, ~a" 3)           ; are bottom two bits 0?
  (let ([twobits-lbl (unique-label)]
        [odd4-lbl    (unique-label)])
    (emit "    jz ~a" twobits-lbl)      ; they are, skip to next step
    (emit "    and edx, 0xFFFFFFFC")    ; else remove bottom two bits ...
    (emit "    add edx, 4")             ; ... and add 4
    (emit "~a: // multiple of 4" twobits-lbl) ; if no adjustment required
    ;; Now we have a multiple of 4. Check that it is an odd multiple.
    (emit "    bt edx, 2")              ; if bit 2 of edx is ...
    (emit "    jc ~a" odd4-lbl)         ; ... 1, nothing to do
    (emit "    add edx, 4")             ; else make it one
    (emit "~a: // odd multiple" odd4-lbl))
  ;; We now have edx that is an odd multiple of 4, ebp still points to the
  ;; original value. Bump EBP past the length and string
  (emit "    lea ebp, [ebp+edx+4]"))

(define-primitive (string-length si env str)
  (emit-expr si env str)                     ; str is in EAX
  ;; Length is already a fixnum
  (emit "    mov eax, [eax-~a]" string-tag)) ; Change to pointer and deref

(define-primitive (string-ref si env str idx)
  (emit-comment-list 'string-ref str idx)
  (emit-args-and-save si env str idx) ; eax <- str; [esp-si] <- idx
  (emit "    mov edx, ~a" (esp-ptr si)) ; edx has idx (as fixnum)
  (emit "    shr edx, ~a" fx/shift)     ; edx has idx in bytes
  (emit "    xor ebx, ebx")             ; set ebx to 0 before stuffing bl
  ;; Need to go past one element (to go past the "length" item before applying
  ;; the offset of 'idx'. We also need to subtract the string-tag before
  ;; we can treat it as a pointer.  We do both in one shot: add wordsize and
  ;; subtract string-tag.
  (emit "    mov bl, ~a"
        (reg-ptr "edx+eax" (- wordsize string-tag)))
  ;; We no longer need the string pointer (EAX)
  (emit "    mov eax, ebx")
  ;; Now convert eax to a char
  (emit "    shl eax, 8")
  (emit "    or  eax, ~a" imm/char-tag))

(define-primitive (string-set! si env str idx val)
  (emit-comment-list 'string-set 's idx val)
  (emit-args-and-save si env idx val)   ; eax <- idx, val <- [esp-si]
  ;; We cannot guarantee that emit-expr for vec will not use our
  ;; register. Have to thus save idx onto the stack as well.
  (emit-stack-save (word- si))         ; [esp-si-4] <- idx
  ;; As a side effect, we return the vector
  (emit-expr (word- (word- si)) env str) ; eax <- vec ptr
  (emit "    mov edx, ~a" (esp-ptr (word- si))) ; edx <- idx (fixnum)
  (emit "    shr edx, ~a" fx/shift)              ; ebx -> int

  (emit "    mov ebx, ~a" (esp-ptr si))          ; ebx <- val
  ;; Convert to a byte
  (emit "    shr ebx, 8")
  ;; as we use bl below, we don't need to AND (only put the lowest byte)
  ;; Use (wordsize - string-tag) to go past the length item
  (emit "    mov ~a, bl" (reg-ptr "edx+eax" (- wordsize string-tag))))

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
  (emit-expr (word- si) env arg1))

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
            (emit-expr (word- si) env arg1) ; arg1 -> eax
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

(define data-width (* 2 wordsize))      ; how many bytes a heap element uses

(define pair-first  (- pair-tag))
(define pair-second (+ pair-first wordsize))

(define (begin? expr)
  (tagged-list? 'begin expr))

(define (emit-begin      si env expr)   ; car is 'begin, cdr are expressions
  (emit-exprs      si env (cdr expr)))
(define (emit-tail-begin si env expr)
  (emit-tail-exprs si env (cdr expr)))

;; ======================================================================
;; Local variables
;; ======================================================================
(define (let-body     let-expr) (cddr let-expr))
(define (let-bindings let-expr) (cadr let-expr))

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
                                       (closure? (cadr b))) (cadr expr))))

;; A variable on the (run-time) stack will return a string representation
;; of the value. This can be used either for a load or a store
(define variable? symbol?)

;; The environment value for a variable is its si index. Thus processing a
;; variable reference is simply loading it into eax from its stack position.
(define (emit-vbl-ref env var)
  (let ([si (lookup-variable var env)])
    (if (integer? si)
        ;; emit-stack-load generalized; negative env values come from esp,
        ;; others from edi (closure free vars)
        (emit "    mov eax, ~a" (reg-ptr (if (< si 0) "esp" "edi") si))
        (error 'emit-variable "Variable not found" var))))
(define (emit-tail-vbl-ref env var)
  (emit-vbl-ref env var) (emit-ret))

;; emit-let or emit-let* depending on is*? (is*? == #t => emit-let*)
;; emitter defines whether we process it as a tail expression. Note that
;; emitter should process a list of expressions, not a single expression.
(define (emit-let/* si env expr is*? emitter)
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
            (word- si)
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

(define (emit-letrec-top expr)
  (define letrec-body cddr)
  (let* ([bindings (cadr expr)]
         [lvars    (map car bindings)]
         [lambdas  (map cadr bindings)]
         [labels   (map (lambda (n) (unique-label)) lvars)]
         [env      (make-initial-env lvars labels)])
    (for-each (emit-lambda env) lambdas labels lvars)
    (emit-scheme-entry env (letrec-body expr))))

(define (emit-letrec si env expr)
  (define letrec-body cddr)
  (let* ([bindings (cadr expr)]
         [lvars    (map car    bindings)]
         [closures (map cadr   bindings)]
         [lengths  (map closure-space closures)])
    (let ebp ([si si]
              [env env]
              [lvars lvars]
              [ac 0]
              [lengths lengths])
      (if (null? lvars)
          ;; At this point both si and env are the new values
          (let ([si
                 ;; This also sets ebp to the right value
                 (fold-left (lambda (si cl)
                              (emit-closure si env cl)
                              (emit-stack-save si)
                              (word- si)) si closures)])
            (emit-exprs si env (letrec-body expr)))
          (begin
            (emit "    // variable ~a" (car lvars))
            (emit "    lea eax, ~a" (reg-ptr "ebp" (+ ac closure-tag)))
            ;;(emit "    or eax, ~a" closure-tag)
            (emit "    mov ~a, eax" (esp-ptr si))
            (ebp (word- si) (extend-env (car lvars) si env) (cdr lvars)
                 (fx+ ac (car lengths)) (cdr lengths)))))))

(define (emit-lambda env)
  ;; Instead of directly generating a lambda, we generate a function that
  ;; can be used with the given environment. This allows it to be used in
  ;; the for-each in emit-letrec.
  (lambda (expr label lvar)
    ;; When processing the lambda expression, we can encounter:
    ;; * a formal, which is part of the stack (and whose env value is a
    ;;   negative integer)
    ;; * a free variable, whose env value is a non-negative integer
    ;; * a reference to another lambda (whose env value will be
    ;;   its label).
    ;;
    ;; For a reference to a lambda, we will have to generate a call to the
    ;; given label. We first build up the list of formals and free vars with
    ;; the given environment and then generate the body expression with the
    ;; new environment. This is similar to a let expression.
    (emit "")
    (emit "// ~a: ~s" lvar (cadr expr))
    (emit-function-header label)
    (let ([fmls (cadr  expr)]
          [fvs  (caddr expr)]
          [body (cdddr expr)])

      ;; si starts at -wordsize, since esp is updated for each call
      (let fm ([fmls fmls] [si (- wordsize)] [env env])
        (if (null? fmls)
            ;; Once fmls are exhausted, go on to freevars
            ;; env for freevars starts at wordsize, since the first cell
            ;; is for the location
            (let fv ([fvs fvs ] [bp wordsize] [env env])
              (if (null? fvs)
                  ;; Generate code for body
                  (emit-tail-exprs si env body)
                  ;; keep extending env
                  (fv (cdr fvs) (word+ bp) (extend-env (car fvs) bp env))))
            (fm (cdr fmls) (word- si) (extend-env (car fmls) si env)))))))

;; The first argument of expr should be the function pointer. This will either
;; be a closure, or a symbol (variable) representing the closure. Either way,
;; we can evaluate the argument and then eax will contain the pointer to the
;; closure.  Then dump the value of edi into the stack and copy eax to edi.
;; While evaluating the arguments, if we have to make another procedure call,
;; the value of edi will be saved and restored. Thus at the end of argument
;; processing, all variables will be on the stack and EDI will have the
;; correct value. Still need to figure out what to do for a tail call.
(define (emit-app si env expr)
  (define (emit-arguments si args)
    (unless (null? args)
            (emit-expr si env (car args))
            (emit-stack-save si)
            (emit-arguments (word- si) (cdr args))))

  ;; The first argument of expr is the function, which is either a closure
  ;; expression or a variable referring to a closure expression (on the heap
  ;; or the stack). In either case, we emit the expressions including the
  ;; first expression. This will cause the closure pointer to be generated in
  ;; si. As we have to leave two slots in the stack before populating the
  ;; function arguments, we decrement the stack pointer before generating the
  ;; expression. This means we will have one free slot, one slot used by the
  ;; closure pointer, and then the function arguments (which are thus in the
  ;; correct position).
  ;;
  ;; We then adjust the stack appropriately and push the old value of edi onto
  ;; the stack (which esp now points to), using up one of the two free slots.
  ;; [esp+4] then contains the new edi value, which we move into edi. We can
  ;; load the value of edi and jump to it.
  (emit-comment (format "call ~s" expr))
  (emit-arguments (word- si) expr)
  (emit-adjust-stack (word+ si))
  (emit "    push edi")
  (emit "    mov edi, ~a" (esp-ptr (- wordsize)))
  (emit "    sub edi, ~a" closure-tag)
  (emit "    call [edi]")
  (emit "    pop edi")
  (emit-adjust-stack (- (word+ si))))

(define (emit-tail-app si env expr)
  ;; (See also comments in emit-app for context)
  ;;
  ;; For tail app, we are necessarily coming from another call to emit-app or
  ;; to emit-tail-app. This means that the value of edi is saved away from the
  ;; previous call. When we return from this call, we should jump back to the
  ;; caller's return point, and also restore their value of edi. This means we
  ;; do not need to save edi on the stack; all we need to do is to find the
  ;; address to jump to and return there. However, we don't keep any gaps in
  ;; the stack as we do for emit-app (which we utilized to store the function
  ;; pointer). We could do what we did for emit-app (which offsets the formal
  ;; variables by one), and copy-arguments would take care of it. However,
  ;; this would always require a copy of the arguments, which may not be
  ;; necessary when doing a tail call after a procedure with no arguments.
  ;;
  ;; In order to avoid this, we emit the closure pointer at the end, and make
  ;; sure it is stored in eax. This requires a different definition of
  ;; emit-arguments from the one in emit-app.
  (define (emit-arguments si args fun)
    (if (null? args)
        (emit-expr si env fun)       ; fun (closure pointer) is stored in eax
        (begin
          (emit-expr si env (car args))
          (emit-stack-save si)
          (emit-arguments (word- si) (cdr args) fun))))
  (define (copy-arguments dest src exprs)
    ;; If dest==src, nothing to do
    (unless (eqv? dest src)
       (unless (null? exprs)
          (emit-stack-load src)
          (emit-stack-save dest)
          (copy-arguments (word- dest) (word- src) (cdr exprs)))))

    ;; We don't need to keep any space for the return address; thus use si
    (emit-arguments si (cdr expr) (car expr))
    ;; arguments are on stack, closure is in eax
    (emit-comment "copy closure pointer")
    (emit "    mov edi, eax")
    (emit "    sub edi, ~a" closure-tag)
    (copy-arguments (- wordsize) si (cdr expr))
    (emit "    jmp [edi] // tail call")
    (emit ""))

(define (closure? expr)
  (and (tagged-list? 'closure expr) (> (length expr) 1)))

(define (closure-space expr)
  ;; The amount of space used by each closure, always a multiple of 2*wordsize
  ;; The length of the closure includes the symbol 'closure. If this is odd,
  ;; then the number of arguments is even, and is one less than the length. If
  ;; this is even, the number of arguments is odd, and the desired length is
  ;; the length of the closure expression.
  (let ([clen (length expr)])
    (* wordsize (if (odd? clen) (- clen 1) clen))))

(define (emit-closure si env expr)
  (emit-comment-list 'closure expr)
  ;; Need lea here; otherwise we get the value of where the label points to.
  (emit "    lea eax, ~a" (lookup-variable (cadr expr) env))
  (emit "    mov [ebp], eax")
  (let cl-args ([vars (cddr expr)] [bp (word+ 0)])
    (unless (null? vars)
            (emit-comment-list "lookup " (car vars))
            (emit "//ENV: ~a" env)
            (emit-stack-load (lookup-variable (car vars) env))
            (emit "    mov ~a, eax" (reg-ptr "ebp" bp))
            (cl-args (cdr vars) (word+ bp))))
  (emit "    mov eax, ebp")
  (emit "    or eax, ~a" closure-tag)
  (emit "    add ebp, ~a" (closure-space expr)))

(define (emit-tail-closure si env expr)
  (emit-closure si env expr)
  (emit-ret))

(define (app? expr)
  (and (pair? expr) (symbol? (car expr))))

(define (make-initial-env symbols values)
  (map cons symbols values))

;; ======================================================================
;; Conditionals
;; ======================================================================
;; ====== if =====
(define (if? expr)
  (tagged-list? 'if expr))

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
  (or (tagged-list? 'and expr) (tagged-list? 'or expr)))

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
  (with-input-from-file fn (lambda () (read))))

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

(define (word- si) (- si wordsize))
(define (word+ si) (+ si wordsize))

;; ==== Miscellaneous functions ====
(define (emit-ret) (emit "    ret") (emit ""))

;; Just like emit, but does nothing
(define (emit-null . x) #f)

;; ==== Utility functions ====
(define unique-count
  (let ([count 0])
    (lambda ()
      (set! count (+ 1 count))
      count)))

(define (unique-label)
  (format "L_~s" (unique-count)))

(define (unique-symbol)
  (string->symbol (format "fqz_~s" (unique-count))))

(define (tagged-list? tag expr)
  (and (list? expr) (pair? expr) (symbol? (car expr)) (eqv? tag (car expr))))

;; Just like map, but only for one list. Also deals with improper lists and
;; non-lists (atoms)
(define (map* f l)
  (cond
   [(pair? l) (cons (f (car l)) (map* f (cdr l)))]
   [(null? l) '()]
   [else      (f l)]))

(define (flatmap f . lst)
  (apply append (apply map f lst)))

;; ==== Environments ====
;; An environment is represented by the stack offset it has. The lookup
;; function will return the stack offset, or #f if not found.
(define (lookup-variable var env)
  (let ([p (assoc var env)])
    (if p (cdr p) #f)))

(define (extend-env var si env)
  (cons (cons var si) env))

;; ==== Transformation ====
(define specials '(if and or cond lambda begin
                      define let let* letrec letrec*))

;; Return a list with only the unique elements in l, as determined by eqv?
(define (unique l)
  (if (null? l) l
      (cons (car l) (unique (remv (car l) (cdr l))))))

(define (transform expr)
  ;; These apply only to the untransformed lambda.
  (define lambda-fmls cadr)
  (define lambda-body cddr)
  ;; Finds freevars in the given expression, and appends the found freevars to
  ;; our already known lis of free vars 'fv'
  (define (freevars fmls fv expr)
    ;; if expr is a lambda, skip it
    ;; if it is a list, recurse on it
    ;; if it is a symbol, not a primcall, or a special form
    ;;   (if and or cond lambda begin define), add to freevars if not formal
    (cond [(lambda? expr)
           ;; We look at freevars in the lambda that don't otherwise appear in
           ;; its formals list. However, they may appear in our formals list
           ;; or our freevars list. So, if we are in a nested lambda L, we add
           ;; a free var if it does not appear in L's formals, our formals or
           ;; our already known freevars. Note that we are looking for free
           ;; variables that are defined in a nested lambda, but these should
           ;; be "free" in our expression. The nested lambda will figure out
           ;; its own free variables through the transformation process.
           (freevars (append fmls (lambda-fmls expr)) fv (lambda-body expr))]
           ;; We have to get the freevars out of the first expression,
           ;; and pass the extended list to the cdr
           [(let? expr)
            (let ([fv
                   (unique
                    (flatmap (lambda (e) (freevars fmls fv (cadr e)))
                             (let-bindings expr)))]
                  [bound (map car (let-bindings expr))])
              ;; We don't want the let to pick up the variables bound in the
              ;; let, but we don't want to add those in the freevars of the
              ;; lambda. Thus we add them to the formals, they get discarded
              ;; afterwards.
              (freevars (append bound fmls) fv (let-body expr)))]
           [(pair? expr) (let ([fv (freevars fmls fv (car expr))])
                           (freevars fmls fv (cdr expr)))]
           [(and (symbol? expr)
                 (not (primitive? expr))
                 (not (memv expr specials))
                 (not (memv expr fmls))
                 (not (memv expr fv))) (cons expr fv)]
           [else fv]))
  (define (transform-lambda expr)
    (let ([fmls (cadr expr)] [body (cddr expr)])
      (append
       (list 'lambda (cadr expr) (freevars fmls '() body)) (transform body))))
  (cond
   [(lambda? expr) (transform-lambda expr)]
   [(pair? expr) (map* transform expr)]
   [else expr]))

(define (lift-closures expr)
  (define lbl-list '())
  (define (lift-rec expr)
    (cond
     [(lambda? expr)
      (let* ([lbl   (unique-symbol)]
             [fmls  (cadr  expr)]
             [fv    (caddr expr)]
             [lbody (lift-rec (cdddr expr))]
             [nlamb (append (list 'lambda fmls fv) lbody)])
        (set! lbl-list (cons (cons lbl (list nlamb)) lbl-list))
        (cons 'closure (cons lbl fv)))     ; return value
      ]
     [(pair? expr) (map* lift-rec expr)]
     [else expr]))
  (set! lbl-list '())
  (let ([lexpr (lift-rec expr)])
    (cons 'letrec (cons lbl-list (list lexpr)))))
(define (lift expr) (lift-closures (transform expr)))

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
   [(closure?   expr) (emit-closure   si env expr)]
   [(if?        expr) (emit-if        si env expr)]
   [(begin?     expr) (emit-begin     si env expr)]
   [(primcall?  expr) (emit-primcall  si env expr)]
   [(let?       expr) (emit-let       si env expr)]
   [(let*?      expr) (emit-let*      si env expr)]
   [(letrec?    expr) (emit-letrec    si env expr)]
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
   [(closure?   expr) (emit-tail-closure   si env expr)]
   [(if?        expr) (emit-tail-if        si env expr)]
   [(begin?     expr) (emit-tail-begin     si env expr)]
   [(primcall?  expr) (emit-tail-primcall  si env expr)]
   [(let?       expr) (emit-tail-let       si env expr)]
   [(let*?      expr) (emit-tail-let*      si env expr)]
   [(letrec?    expr) (emit-tail-letrec    si env expr)]
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
  (emit-letrec-top (lift expr)))
  ;(if (letrec? expr)
  ;    (emit-letrec expr)          ; which eventually calls emit-scheme-entry
  ;    (emit-scheme-entry '() (list expr))))
