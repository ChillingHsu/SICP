#lang sicp
;ex2.4
;序对的一种过程性表示方式
; (define (cons x y)
;     (lambda (m) (m x y)))
; (define (car z)
;     (z (lambda (p q) p)))
; (define (cdr z)
;     (z (lambda (p q) q)))


(define (power a n)
    (if (> n 0)
        (* a
          (power a (dec n)))
        1))
;ex2.5
;序对的非负整数算数运算表示方式
(define (cons a b)
    (* (power 2 a)
       (power 3 b)))
(define (car z)
    (define (car-iter z a)
        (if (= 0 (remainder z 2))
            (car-iter (/ z 2) (inc a))
            a))
    (car-iter z 0))
(define (cdr z)
    (define (cdr-iter z b)
        (if (= 0 (remainder z 3))
            (cdr-iter (/ z 3) (inc b))
            b))
    (cdr-iter z 0))
(car (cons 3 10))
(cdr (cons 3 10))
