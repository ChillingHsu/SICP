#lang sicp
;1.2.6
;使用费马小定理快速检查素数
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (if (= (remainder (square (expmod base (/ exp 2) m)) m) 1)
             0
             (remainder (square (expmod base (/ exp 2) m)) m)))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (square x)
  (* x x))

(define (miller-rabin-test n)
  (define (test a)
    (= (expmod a n n) a))
  (test (inc (random (dec n)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))
