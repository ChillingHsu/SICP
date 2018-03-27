(load "stream-lib.scm")

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

;; ex3.73
(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (integral (scale-stream i (/ 1.0 C))
                           v0
                           dt))))

(define RC1 (RC 5 0.1 0.5))

(display-stream (RC1 ones 0) 10)

;; ex3.74
(define (sign-change-detector new-val lst-val)
  (if (< (* new-val lst-val) 0)
      (if (< new-val 0) -1 1)
      0))

(define sense-data (stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))
(define zero-crossings
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))
(display-stream zero-crossings 11)
