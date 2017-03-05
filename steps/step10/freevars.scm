;; Assume p,q,y are defined outside the letrec
(letrec ([f (lambda (x) (fx+ y (g x)))]
         [g (lambda (z) (fx+ z (* p q)))])
  (cons (g 5) (f 2)))

;; transformed to
(letrec ([f (code (x) (y g) (fx+ y (g x)))]
         [g (code (z) (p q) (fx+ z (* p q)))])
  (cons (g 5) (f 2)))

;; transformed to
(labels ([f0 (code (x) (y g) (fx+ y (call g x)))] ;f0,f1 are unique labels
         [f1 (code (z) (p q) (fx+ z (* p q)))])
        ;; Note that g is defined after f, can this be resolved when
        ;; generating code for the letrec?
        (letrec ([f (closure f0 y g)]
                 [g (closure f1 p q)])
          (cons (call g 5) (call f 2))))



