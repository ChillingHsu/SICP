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

; (fixed-point cos 1.0)
;(define find-root
;  (fixed-point (lambda (x) (/ (log 1000) (log x))) 2))
;(define find-root-damp
;  (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2))

; 平均阻尼
(define (fixed-point-damp f first-guess)
  (define (average-damp f)
    (lambda (x) (average x (f x))))
  (fixed-point (average-damp f) first-guess))

(define (square-root x)
  (fixed-point-damp (lambda (y) (/ x y)) 1.0))

(square-root 10)

(define (cubic n)
  (* n n n)
(define (cubic-root x)
  (fixed-point-damp (lambda (y) (/ x (cubic y))) 1.0))
;Attention: cannot converge. see ex1.45 for reason
;(cubic-root 10)

;(define find-root-damp
;  (fixed-point-damp (lambda (x) (/ (log 1000) (log x))) 2))
;(define golden-ratio
;  (fixed-point-damp (lambda (x) (+ 1 (/ 1.0 x))) 1.6))
