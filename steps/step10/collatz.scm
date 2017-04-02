(letrec ([even?   (lambda (n) (fx= (fxlogand n 1) 0))]
          [next    (lambda (n) (if (even? n) (fx/ n 2) (fx+ (fx* n 3) 1)))]
          [collatz (lambda (n num)
                     (if (fx= n 1) num (collatz (next n) (fx+ num 1))))]
          [seq     (lambda (n ac)
                     (if (fx= n 0) ac
                         (seq (fx- n 1) (cons (collatz n 0) ac))))]
          [coll-seq (lambda (n) (seq n ()))])
   (coll-seq 72))
;; (letrec ([fqz_27 (lambda (n num)
;;                    (next collatz)
;;                    (if (fx= n 1) num (collatz (next n) (fx+ num 1))))]
;;          [fqz_26 (lambda (n)
;;                    (even?)
;;                    (if (even? n) (fx/ n 2) (fx+ (fx* n 3) 1)))]
;;          [fqz_25 (lambda (n) () (fx= (fxlogand n 1) 0))])
;;   (letrec ([even? (closure fqz_25)]
;;            [next (closure fqz_26 even?)]
;;            [collatz (closure fqz_27 next collatz)])
;;     (collatz 31 0)))
