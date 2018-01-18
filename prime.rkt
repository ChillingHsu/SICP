#lang sicp
;1.2.6
;素数检测

;平方过程
(define (square x)
  (* x x))
;整除判词
(define (divides? a b)
  (= (remainder b a) 0))
;找到n的最小的因子
(define (smallest-divisor n)
  (find-divisor n 2))
;测试因子
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
;素数判词
(define (prime? n)
  (= n (smallest-divisor n)))
;计时素数测试
(define (timed-prime-test n)
    (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
    (cond ((prime? n)
           (display n)
           (report-prime (- (runtime) start-time)))))
(define (report-prime elapsed-time)
  (display "**PRIME**")
  (display elapsed-time)
  (newline))

(define (prime-test-timetable n)
  (prime-test-iter 3 n))

(define (prime-test-iter cur count)
  (timed-prime-test cur)
  (if (< cur count)
      (prime-test-iter (inc cur) count)))
(prime-test-timetable 1000)