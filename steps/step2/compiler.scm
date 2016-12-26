(load "../../src/tests-driver.scm")
(load "../../src/tests-1.2-req.scm")
(load "../../src/tests-1.1-req.scm")

;; fixnum is a signed 30 bit value with 2 LSB==0b00
;; Boolean is #f == 0b00101111 (0x2F); #t == 0b01101111 (0x6F)
;;   (mask := 0b10111111, result is 0b00101111) [bool & #f == #f]
;; Character (<charByte> << 8) | 0b00001111 (15, 0xF)
;; Empty list (): 0b00111111 (63, 0x3F)

;; Fixnums. Note that our scheme uses 30 bit fixnums, while the
;; implementation language necessary does not. So we cannot rely on
;; the input being a fixnum.  Instead, we assume it is an integer, and
;; make sure it is within the max and min values.
(define fx-bits 30)
(define fx-min-val (- 0 (bitwise-arithmetic-shift 1 (- fx-bits 1))))
(define fx-max-val (- (bitwise-arithmetic-shift 1 (- fx-bits 1)) 1))
(define (fixnum? x)
  (and (integer? x)
       (<= fx-min-val x fx-max-val)))
;; Convert to our fixnum
(define (fxnum-rep n) (bitwise-arithmetic-shift n 2))

;; Booleans
(define (bool-rep b) (if b #x6F #x2F))

;; Chars
(define (char-rep c)
  (bitwise-ior (bitwise-arithmetic-shift (char->integer c) 8) #xF))

(define (immediate-rep x)
  ((cond
    [(null?    x) (lambda (x) #x3F)]
    [(fixnum?  x) fxnum-rep]
    [(boolean? x) bool-rep ]
    [(char?    x) char-rep ]
    [else (error 'immediate-rep "Unhandled type" x)]
    ) x))

(define (emit-program x)
 (emit "     .text")
 (emit "     .intel_syntax noprefix")
 (emit "     .globl _scheme_entry")
 (emit "_scheme_entry:")
 (emit "    mov eax, ~s" (immediate-rep x))
 (emit "    ret"))
