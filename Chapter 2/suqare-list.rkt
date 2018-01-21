#lang sicp
;ex2.21
;定义过程square-list
(define (square x)
    (* x x))

; (define (square-list items)
;     (if (null? items)
;         nil
;         (cons (square (car items)) (square-list (cdr items)))))

; (define (square-list items)
;     (map square items))

;ex2.22
;顺序相反是因为每次添加都是在原有列表之前添加而不是之后，所以会导致这个问题。
;交换cons原有参数后会导致序列链性质改变（每个序对的第一个元素为前一个序对）
;正确的做法是使用append函数
(define (square-list items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                  (append answer (cons (square (car things)) nil)))))
    (iter items nil))
(square-list (list 2 3 4 5 6))
