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
  (if (<= (* new-val lst-val) 0)
      (if (< new-val 0) -1 1)
      0))

(define sense-data (stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))
(define zero-crossings
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))
(display-stream zero-crossings 10)

;; ex3.75
(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))

(display-stream (make-zero-crossings sense-data 0 0) 10)
