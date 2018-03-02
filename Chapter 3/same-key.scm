;ex3.24
(define (same-key? key records)
  (define tolerance 0.1)
  (cond ((null? records) false)
        ((> tolerance
            (abs (- (caar records)
                    key)))
         (car records))
        (else (same-key? key (cdr records)))))
(define assoc same-key?)

(load "2-d-table.scm")
(put 3 4 7)
(get 2.95 4.01)
