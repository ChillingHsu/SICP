#lang sicp
; 1.3.3
; 函数不动点
; 误差定义
(define tolerance 0.000000000000001)
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
;1.3.4
;牛顿法求根
(define (square x)
    (* x x))
;求导
(define (deriv g)
    (define dx 0.00001)
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
;牛顿法变形
(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
    (fixed-point (newton-transform g) guess))
;牛顿法求平方根
(define (square-root x)
    (newton-method (lambda (y) (- (square y) x)) 1.0))
(define (cubic x)
    (* x x x))

;ex1.40
;三次方程
(define (cubic-formula a b c)
    (lambda (x) (+ (cubic x) (* a (square x)) (* b x) c)))
; (newton-method (cubic-formula 1 2 3) 1.0)
(define (fixed-point-of-transform g transform guess)
    (fixed-point (transform g) guess))
(define (average-damp-transform g)
    (lambda (x) (average x (g x))))
(define (sqrt-average-damp x)
    (fixed-point-of-transform (lambda (y) (/ x y))
                             average-damp-transform
                             1.0))
(define (sqrt-newtons-transform x)
    (fixed-point-of-transform (lambda (y) (- (square y) x))
                              newton-transform
                              1.0))
(sqrt-average-damp 99)
(newline)
(sqrt-newtons-transform 99)
