#lang sicp
(define (deep-reserve items)
    (cond ((null? items) items)
        ((not (pair? items)) items)
        (else (append (deep-reserve (cdr items))
                      (list (deep-reserve (car items)))))));这里过程的顺序很重要
(define x (list (list 1 2) (list 3 4)))
(deep-reserve x)
