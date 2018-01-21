#lang sicp
;ex2.23
;for-each的实现
(define (for-each f items)
    (cond ((null? (cdr items)) (f (car items)))
            (else (f (car items))
              (for-each f (cdr items)))))
(for-each (lambda (x) (display x) (newline))
          (list 12 34 56))
