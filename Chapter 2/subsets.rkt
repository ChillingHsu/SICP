#lang sicp
;ex2.32
;求一个给定集合的子集的过程
;一个给定集合s的子集：
;若s为空集，则子集为空集
;若s不为空集，令去掉其中任意一个元素a的集合为s'，则子集为s'的子集（不选a）并上a与s'各子集并（选a）
(define (subsets s)
    (if (null? s)
        (list '())
        (let ((rest (subsets (cdr s))))
          (append rest (map (lambda (item)
                              (cons (car s) item))
                            rest)))))
(subsets (list 1 2 3))
