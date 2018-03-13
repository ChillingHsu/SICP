;There will be some junk output in loading.
(load "stream.scm")

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))
(display-stream
  (stream-filter (lambda (pair)
                   (prime? (+ (car pair) (cadr pair))))
                 (pairs integers integers))
  10)

;ex3.66
(define (index-of s t)
  (if (= s t)
      (- (expt 2 s) 1)
      (+ (index-of s s)
         (expt 2 (- s 1))
         (* (- (- t s) 1) (expt 2 s)))))
(display-stream (pairs integers integers) 10)

(stream-ref (pairs integers integers) (- (index-of 1 100) 1))
;设F(s,t)是(s,t)在整个序列中出现的序数。显然F(1,1) = 1, F(1,2) = 2。类似可以推出等式
;F(s,t) = 2^s - 1 , s = t
;       = 2^s - 1 + 2^(s-1) + (t-s-1) * 2^s
(- (index-of 1 100) 1)
;(1 100)之前有197个序对
;Value: 197
(- (index-of 100 100) 1)
;(100 100)之前有1267650600228229401496703205374个序对
;Value: 1267650600228229401496703205374
(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))
(display-stream
  (stream-filter (lambda (pair)
                   (prime? (+ (car pair) (cadr pair))))
                 (pairs integers integers))
  10)
