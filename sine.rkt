#lang sicp
;ex1.15
;利用三倍角定义sine
(define (cube x) (* x x x))
(define (sine x)
  (define (formula a)
    (- (* 3 a) (* 4 (cube a))))
  (if (< x 0.0001)
      x
      (formula (sine (/ x 3)))))
