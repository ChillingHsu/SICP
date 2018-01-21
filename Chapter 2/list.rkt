#lang sicp
(define one-through-four (list 1 2 3 4))
(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7 9))

(cadr one-through-four)
;2.2.1
;返回表中第n项
(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (dec n))))

(list-ref squares 3)
(define (length items)
    (if (null? items)
        0
        (inc (length (cdr items)))))
(length odds)
(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1)
              (append (cdr list1) list2))))
(append squares odds)
;ex2.17
;定义过程last-pair，返回给定非空表里最后一个元素的表
(define (last-pair items)
    (if (null? items)
        (error "empty list -- last-pair" items)
        (if (null? (cdr items))
            items
            (last-pair (cdr items)))))
(last-pair odds)
;ex2.18
;定义过程reverse，以一个表为参数，返回表中所包含的元素与参数表相同，但顺序相反。
(define (reverse items)
    (if (null? items)
        items
        (append (reverse (cdr items)) (cons (car items) nil))))
(define rodd (reverse odds))
rodd
