#lang sicp
;ex2.49
(#%require sicp-pict)
(define outline
  (segments->painter
    (list
     (make-segment (make-vect 0 0) (make-vect 0 1))
     (make-segment (make-vect 0 0.99) (make-vect 0.99 0.99))
     (make-segment (make-vect 0.99 0.99) (make-vect 0.99 0))
     (make-segment (make-vect 1 0) (make-vect 0 0)))))
(define x-painter
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 1 1))
    (make-segment (make-vect 1 0) (make-vect 0 1)))))
(define diamond
  (segments->painter
   (list
    (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
    (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
    (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
    (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))
(define wave 
   (segments->painter
    (list 
     (make-segment (make-vect 0 0.65) (make-vect 0.14 0.39))
     (make-segment (make-vect 0.14 0.39) (make-vect 0.29 0.58))
     (make-segment (make-vect 0.29 0.58) (make-vect 0.34 0.49))
     (make-segment (make-vect 0.34 0.49) (make-vect 0.24 0))
     (make-segment (make-vect 0.4 0) (make-vect 0.49 0.28))
     (make-segment (make-vect 0.49 0.28) (make-vect 0.59 0))
     (make-segment (make-vect 0.71 0)  (make-vect 0.59 0.45))
     (make-segment (make-vect 0.59 0.45) (make-vect 0.99 0.15))
     (make-segment (make-vect 0.99 0.35) (make-vect 0.74 0.64))
     (make-segment (make-vect 0.74 0.64) (make-vect 0.59 0.64))
     (make-segment (make-vect 0.74 0.64) (make-vect 0.59 0.64))
     (make-segment (make-vect 0.59 0.64) (make-vect 0.64 0.85))
     (make-segment (make-vect 0.64 0.85) (make-vect 0.59 1))
     (make-segment (make-vect 0.39 1) (make-vect 0.34 0.85))
     (make-segment (make-vect 0.34 0.85) (make-vect 0.39 0.64))
     (make-segment (make-vect 0.39 0.64) (make-vect 0.29 0.64))
     (make-segment (make-vect 0.29 0.64) (make-vect 0.14 0.6))
     (make-segment (make-vect 0.14 0.6) (make-vect 0 0.84)))))
(paint outline)
(paint x-painter)
(paint diamond)
(paint wave)