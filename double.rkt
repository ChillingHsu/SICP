#lang sicp
;ex1.41
(define (double f)
    (lambda (x) (f (f x))))
(((double (double (double double))) inc) 0)
;实际效果为执行了16次inc(Attention: double 是 ^2 的意义而不是 *2)
(((double (double double)) inc) 0)
