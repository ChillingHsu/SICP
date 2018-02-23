;ex3.17
(define (count-pairs x)
  (let ((visited-list '()))
    (define (visitor elt)
      (if (and (pair? elt)
               (false? (memq elt visited-list)))
          (begin (set! visited-list (cons elt visited-list))
                 (visitor (car elt))
                 (visitor (cdr elt)))
          "leaf"))
    (visitor x)
    (length visited-list)))
(count-pairs '(a b c))
;Value: 3
(define p (cons 'a 'b))
(count-pairs (cons 'a (cons p p)))
;Value: 3
(define l (cons 'a (cons 'b 'c)))
(count-pairs (cons l l))
;Value: 3
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define c (make-cycle (list 'a 'b 'c)))
(count-pairs c)
;Value: 3
