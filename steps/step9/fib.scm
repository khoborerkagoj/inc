'(letrec ([cadr (lambda (l) (car (cdr l)))]
          [fib (lambda (n) (f (fx- n 2) (cons 1 (cons 0 ()))))]
          [rev-ac (lambda (l ac)
                    (if (null? l)
                        ac
                        (rev-ac (cdr l) (cons (car l) ac))))]
          [reverse (lambda (l) (rev-ac l ()))]
          [f (lambda (n l)
               (if (fxzero? n)
                   l
                   (f (fx- n 1) (cons (fx+ (car l) (cadr l)) l))))])
   (reverse (fib 35)))
