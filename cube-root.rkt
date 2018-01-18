#lang sicp
;ex1.8
;牛顿法求立方根
(define (cube-root x)
  (guess-cube-root 1.0 x))
(define (guess-cube-root guess x)
  (if (good-enough? guess x)
      guess
      (guess-cube-root (improve guess x) x)))
(define (improve guess x)
  (/ (+ (/ x (* guess guess))
        guess guess)
     3.0))
(define (good-enough? guess x)
  (< (abs (- (/ (* guess guess guess) x) 1))
     0.000000000000001))
(define (cube x)
  (* x x x))

(cube (cube-root 8))
