#lang sicp
;ex2.20
;不定参数过程的定义
;定义一个含有一个或者多个整数作为参数的过程，返回所有与第一个参数有相同奇偶性的参数形成的表。
(define (same-parity x . y)
    (define (same-parity-of-list a items)
    (if (null? items)
        (cons a nil)
        (if (= (remainder a 2)
            (remainder (car items) 2))
            (cons a (same-parity-of-list (car items) (cdr items)))
            (same-parity-of-list a (cdr items)))))
    (same-parity-of-list x y))
(same-parity 2 3 4 5 6 7 8)
