;ex3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x))
(define z '(a b c d))
z
(make-cycle z)
(last-pair z)
;infinity loop
