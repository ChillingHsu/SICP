#lang sicp
;最大公约数
(define (my-gcd a b)
  (if (= b 0)
      a
      (my-gcd b (remainder a b))))