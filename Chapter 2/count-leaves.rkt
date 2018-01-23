#lang sicp
(define x (cons (list 1 2) (list 3 4)))
(length x)
(define (count-leaves tree)
    (cond ((null? tree) 0)
          ((not (pair? tree)) 1)
          (else (+ (count-leaves (car tree))
                   (count-leaves (cdr tree))))))
(count-leaves x)
(list x x)
(length (list x x))
(count-leaves (list x x))
;ex2.24
;list过程构造的都是同层次的结构，给定数据形成了如下的树形结构。可见第一级list的共有2个元素，总共有4个叶子元素。
; o
; |\
; 1 o
;   |\
;   2 o
;     |\
;     3 4
(define t (list 1 (list 2 (list 3  4))))
(length t)
(count-leaves t)
;ex2.25
;使用car和cdr从下列各表中取出7
;(1 3 (5 7) 9)
(define a (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr a)))))
;((7))
(define b (list (list 7)))
(car (car b))
;(1 (2 (3 (4 (5 (6 7))))))
(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))
;ex2.26
;解释操作结果
(define list-x (list 1 2 3))
(define list-y (list 4 5 6))
;(append list-x list-y)会返回一个list (1 2 3 4 5 6)
(append list-x list-y)
;(cons list-x list-y)会返回一个包含两个list的pair (cons (1 2 3) (4 5 6))
(cons list-x list-y)
;(list list-x list-y)会返回一个包含两个list的list ((1 2 3) (4 5 6))
(list list-x list-y)
