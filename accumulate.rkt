#lang sicp

(define (accumulate combiner null-value term a next b)
  (define (iter cur result)
    (if (> cur b)
        result
        (iter (next cur) (combiner result (term cur)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (self x) x)
(sum self 1 inc 10)
