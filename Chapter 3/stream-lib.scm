(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (integers-starting-from n)
  (cons-stream n
               (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (display-line elt)
  (newline)
  (display elt))

(define (display-stream stream lines)
  (display-line (stream-car stream))
  (if (= lines 1)
      'done
      (display-stream (stream-cdr stream) (- lines 1))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-stream s1 s2)
  (stream-map * s1 s2))

(define (partial-sums s)
  (define (partial-sums s accum)
    (cons-stream (+ (stream-car s) accum)
                 (partial-sums (stream-cdr s) (+ (stream-car s) accum))))
  (partial-sums s 0))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((< s2car s1car)
                  (cons-stream s2car (merge (stream-cdr s2) s1)))
                 (else
                  (cons-stream s1car (merge (stream-cdr s1)
                                            (stream-cdr s2)))))))))

(define ones (cons-stream 1. ones))

(define (div-stream s1 s2)
  (stream-map / s1 s2))

(define (integrate-series power-series)
  (div-stream power-series
              (integers-starting-from 1)))
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series
  (cons-stream 1 (scale-stream
                  (integrate-series sine-series)
                  -1)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams
                (scale-stream (stream-cdr s2) (stream-car s1))
                (mul-series (stream-cdr s1) s2))))
(define one (add-streams (mul-series cosine-series cosine-series)
                         (mul-series sine-series sine-series)))

(define (reciprocal s)
  (let ((c (stream-car s))
        (sr (stream-cdr s)))
    (cons-stream (/ 1 c)
                 (mul-series (reciprocal s)
                             (scale-stream sr (- (/ 1 c)))))))

(define (div-series s1 s2)
  (mul-series s1 (reciprocal s2)))

(define tan-series (div-series sine-series cosine-series))

(define (evaluate series x)
  (define (evaluate series coeff accum)
    (let ((new-accum (+ accum (* coeff (stream-car series)))))
      (cons-stream new-accum
                   (evaluate (stream-cdr series) (* x coeff) new-accum))))
  (evaluate series 1 0))

(define (average a b) (/ (+ a b) 2))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define (stream-limit stream tolerance)
  (if (< (abs (- (stream-car (stream-cdr stream))
                 (stream-car stream)))
         tolerance)
      (stream-car (stream-cdr stream))
      (stream-limit (stream-cdr stream) tolerance)))

(define alternating-series
  (cons-stream 1 (stream-map - alternating-series)))
