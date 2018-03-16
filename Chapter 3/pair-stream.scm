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

;ex3.67
(define (all-pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (interleave
        (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
        (stream-map (lambda (x) (list x (stream-car t)))
                  (stream-cdr s)))
      (all-pairs (stream-cdr s) (stream-cdr t)))))
;another implementation
; (define (all-pairs s t)
;   (cons-stream
;     (list (stream-car s) (stream-car t))
;     (interleave
;       (stream-map (lambda (x) (list (stream-car s) x))
;                   (stream-cdr t))
;       (all-pairs (stream-cdr s) t))))

(display-stream
  (stream-filter (lambda (pair)
                   (prime? (+ (car pair) (cadr pair))))
                 (pairs integers integers))
  10)

;ex3.68
;不能，这样做会导致interleave会不断的调用自己产生无限循环。

(define (tripes s t u)
  (cons-stream
    (list (stream-car s)
          (stream-car t)
          (stream-car u))
    (interleave
      (stream-map (lambda (x) (cons (stream-car s) x))
                  (stream-cdr (pairs t u)))
      (tripes (stream-cdr s)
              (stream-cdr t)
              (stream-cdr u)))))
(display-stream (stream-filter
                  (lambda (tri)
                    (= (+ (square (list-ref tri 0))
                          (square (list-ref tri 1)))
                       (square (list-ref tri 2))))
                  (tripes integers integers integers)) 10)
