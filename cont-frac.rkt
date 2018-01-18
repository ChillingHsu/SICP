#lang sicp
;ex1.37
;无穷连分式

;(define (cont-frac n d k)
;  (define (cont-frac-item count)
;    (if (> count k)
;        0
;        (/ (n count)
;           (+ (d count) (cont-frac-item (inc count))))))
;  (cont-frac-item 1))
(define (cont-frac n d k)
  (define (cont-frac-iter count result)
    (if (= count 0)
        result
        (cont-frac-iter (dec count)
                        (/ (n count)
                           (+ (d count)
                              result)))))
  (cont-frac-iter k 0))

(define (golden-ratio k)
  (/ 1
    (cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           k)))
(display "golden-ratio = ")
(golden-ratio 100)

;ex1.38
(define (e k)
  (+ 2
     (cont-frac (lambda (i) 1.0)
             (lambda (i)
               (let ((n (/ (inc i) 3)))
                 (if (= (remainder (inc i) 3) 0)
                     (* n 2)
                     1)))
             k)))
(display "e = ")
(e 100)

;ex1.39
(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- ((lambda (a) (* a a)) x))))
             (lambda (i)
               (- (* 2 i) 1))
             k))
(display "tan-cf = ")
(tan-cf 3.1415926535 100)

;ex1.37a
; 指定精度
(define tolerance 0.0001)
; 找出最小参数k，使函数f的k阶有限连分式的结果达到`toler`精度
(define (smallest-k f toler)
  (define (iter cur)
    (let ((prev-value (f (dec cur)))
          (cur-value (f cur)))
      (define err (abs (- prev-value cur-value)))
      (display err)
      (newline)
      (if (< err toler)
          cur
          (iter (inc cur)))))
  (iter 2))
; 给定精度，求函数f的有限连分式
(define (iter-at-precision f toler)
  (f (smallest-k f toler)))

(iter-at-precision e tolerance)
