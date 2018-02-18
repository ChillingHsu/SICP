;ex3.8
(define f
  (let ((value 1))
    (lambda (multiplicand)
      (set! value (* value multiplicand))
      value)))
(+ (f 0) (f 1))
