(let ([f (lambda ()
           (let ([g (lambda () (fx+ 2 3))]
                 [h 2])
             (fx* (g) h)))])
  (fx+ (f) (f)))

