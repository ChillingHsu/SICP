#lang sicp
;1.3.3
;折半法求根
(define (search f neg-point pos-point)
  (let ((mid-point ((lambda (a b) (/ (+ a b) 2.0)) neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        mid-point
        (let ((test-value (f mid-point)))
          (cond ((positive? test-value)
                 (search f neg-point mid-point))
                ((negative? test-value)
                 (search f mid-point pos-point))
                (else mid-point))))))
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sigh." a b)))))
(half-interval-method sin 2.0 4.0)
(half-interval-method (lambda (x) (- (* x x x) (* x 2) 3)) 1.0 2.0)
