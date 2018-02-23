;ex3.16
(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))
(count-pairs '(a b c))
;Value: 3
(define p (cons 'a 'b))
(count-pairs (cons 'a (cons p p)))
;Value: 4
(define l (cons 'a (cons 'b 'c)))
(count-pairs (cons l l))
;Value: 5
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define c (make-cycle (list 'a 'b 'c)))
(count-pairs c)
;Aborting!: maximum recursion depth exceeded
