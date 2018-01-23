#lang sicp
;ex2.28
;实现一个提取树的边缘的过程fringe
(define (fringe items)
    (if (pair? items)
        (append (let ((result (fringe (car items))));不同于deep-reserve的递归处理
                    (if (pair? result)
                        result
                        (list result)))
                (fringe (cdr items)))
        items))
(fringe (list (list 1 2) 3 4 (list 5 (list 6))))
