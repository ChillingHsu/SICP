#lang sicp
;ex1.11
;定义
;f(n)=n, n<3
;f(n)=f(n-1)+2f(n-2)+3f(n-3), else
;递归计算过程
(define (f-recursion n)
  (if (< n 3)
      n
      (+ (f-recursion (- n 1))
         (* 2 (f-recursion (- n 2)))
         (* 3 (f-recursion (- n 3))))))
;迭代计算过程
(define (f-iteration n)
  (define (f-iter a b c iter)
    (if (= iter n)
        c
        (f-iter (+ a
                   (* 2 b)
                   (* 3 c))
                a
                b
                (+ iter 1))))
  (f-iter 2 1 0 0))
