
(define (and-expander expr)
  (if (and (pair? expr) (eqv? (car expr) 'and))
      (let ([expr (cdr expr)])
	(cond
	 [(null? expr) #t]
	 [(null? (cdr expr)) (car expr)]
	 [else (list 'if (and-expander (car expr))
		     (and-expander (cons 'and (cdr expr))) #f)]))
      expr))
