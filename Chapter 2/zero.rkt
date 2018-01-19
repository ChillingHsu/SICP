#lang sicp
;ex2.6
;不使用数定义整数
;将zero，one，two这些数视为执行n次单参数过程f的的过程
(define zero
    (lambda (f) (lambda (x) x)))
(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))
(define one
    (lambda (f) (lambda (x) (f x))))
(define two
    (lambda (f) (lambda (x) (f (f x)))))
(define (add a b)
    (lambda (f) (lambda (x) ((a f) ((b f) x)))))
(((add one two) dec) 3)
