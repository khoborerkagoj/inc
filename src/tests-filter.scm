;; utilities to filter the all-tests variable to aid in running tests

(define all-tests-orig '())

(define (show-tests)
  ;; show the categories of all the tests loaded
  (map car all-tests))

(define (reset-tests)
  (if (not (null? all-tests-orig))
      (set! all-tests all-tests-orig)))

(define (filter-tests . l)
  ;; l a list of all the tests we want to include, sets all-tests to only
  ;; those
  (define (deep-copy l)
    (if (pair? l)
        (cons (deep-copy (car l)) (deep-copy (cdr l)))
        l))

  ;; don't clobber all-tests-orig if already set
  (if (null? all-tests-orig)
      (set! all-tests-orig (deep-copy all-tests)))
  (set! all-tests (filter (lambda (e) (member (car e) l)) all-tests)))
