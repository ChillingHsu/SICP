;ex3.1
(define (make-accumulator value)
  (define (change amount)
    (begin (set! value (+ value amount))
           value))
  (lambda (amount) (change amount)))
(define A (make-accumulator 5))
(A 10)
(A 20)
