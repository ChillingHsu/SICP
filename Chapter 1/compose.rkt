#lang sicp
;ex1.42
;compose的实现
(define (compose f g)
    (lambda (x) (f (g x))))
(define (square x)
    (* x x))
((compose square inc) 6)
;ex1.43
;repeated的实现
(define (repeated f count)
    (lambda (x)
        ((if (> count 1)
            (compose f (repeated f (dec count)))
            f)
        x)))
((repeated square 2) 5)
;ex1.44
;函数的n次平滑
(define (smooth f)
    (lambda (x)
        (define dx 0.00001)
        (define (average a b c)
            (/ (+ a b c) 3.0))
        (average (f (- x dx))
                 (f x)
                 (f (+ x dx)))))
(((repeated smooth 2) square) 0)
;ex1.45
; 1.3.3
; 函数不动点
; 误差定义
(define tolerance 1e-20)
; 平均值
(define (average a b)
 (/ (+ a b) 2.0))
; 求不动点过程
(define (fixed-point f first-guess)
; 相等判词
 (define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))
; 比较过程
 (define (try guess)
  (display guess)
  (newline)
  (let ((next (f guess)))
   (if (close-enough? guess next)
    next
    (try next))))
 (try first-guess))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;函数在某种变换下的不动点
(define (fixed-point-of-transform g transform guess)
    (fixed-point (transform g) guess))
;平均阻尼变换
(define (average-damp-transform g)
    (lambda (x) (average x (g x))))
;使用平均阻尼的平方根求法
(define (sqrt-average-damp x)
    (fixed-point-of-transform (lambda (y) (/ x y))
                             average-damp-transform
                             1.0))
;定义求幂过程
(define (power x n)
    ((repeated (lambda (a) (* a x)) n) 1))
;求x的n次方根
;需要做n-1次平均阻尼
(define (root x n)
    (fixed-point-of-transform (lambda (y) (/ x (power y (dec n))))
                              (repeated average-damp-transform (dec n))
                              1.0))

(power (root 1024 11) 11)
