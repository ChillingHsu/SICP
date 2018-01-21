#lang sicp
;ex2.19
;接受list作为参数的零钱兑换程序count-change
;coin-values的排列顺序不影响count-change给出的回答，因为count-change总是会将整个递归计算树访问一遍，不同的顺序只是会影响计算的先后顺序。又因为加法有交换律，所以不会影响最终答案。
(define us-coins (list 1 50 25 10 5))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (count-change amount coin-values)
    (define (no-more? cv)
        (null? cv))
    (define except-first-denomination cdr)
    (define first-denomination car)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else
            (+ (count-change amount
                            (except-first-denomination coin-values))
                (count-change (- amount (first-denomination coin-values))
                            coin-values)))))
(count-change 100 us-coins)
