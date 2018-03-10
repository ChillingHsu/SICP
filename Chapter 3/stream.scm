; To implement stream in Scheme,you may need tot know about https://en.wikipedia.org/wiki/Eager_evaluation
;素数检测

;平方过程
(define (square x)
  (* x x))
;整除判词
(define (divides? a b)
  (= (remainder b a) 0))
;找到n的最小的因子
(define (smallest-divisor n)
  (find-divisor n 2))
;测试因子
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
;素数判词
(define (prime? n)
  (= n (smallest-divisor n)))

; (define (delay exp) (lambda () exp))
; (define (force delayed-object) (delayed-object))

; (define (cons-stream a b) (cons a (delay b)))
; (define (stream-car stream) (car stream))
; (define (stream-cdr stream) (force (cdr stream)))
; (define the-null-stream '())
; (define (null-stream? s) (null? s))

; (define (stream-ref s n)
;   (if (null-stream? s)
;       the-null-stream
;       (if (= n 0)
;           (stream-car s)
;           (stream-ref (stream-cdr s) (- n 1)))))
; (define (stream-map proc s)
;   (if (null-stream? s)
;       the-null-stream
;       (cons-stream (proc (stream-car s))
;                    (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))
; (define (stream-filter pred s)
;   (if (null-stream? s)
;       the-null-stream
;       (if (pred (stream-car s))
;           (cons-stream (stream-car s)
;                        (stream-filter pred (stream-cdr s)))
;           (stream-filter pred (stream-cdr s)))))

(define (show x)
  (display-line x)
  x)

(stream-car
  (stream-cdr
    (stream-filter prime?
                   (stream-enumerate-interval 10000 1000000))))
;ex3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))
(define s (stream-map + (stream 1     2   3)
                        (stream 100 200 300)
                        (stream 50   60  70)))
(stream-ref s 2)
(stream-for-each
  display-line
  s)
;ex3.51
(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
; 1
; 2
; 3
; 4
; 5
; ;Value: 5
(stream-ref x 7)
; 6
; 7
; ;Value: 7

;ex3.52
(define (display-stream s)
  (stream-for-each display-line s))
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 0 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
sum ;Value: 0
(stream-ref y 7)
sum ;Value: 120
(display-stream z)
sum ;Value: 210
;如果不使用memo-proc所提供的优化，每次访问stream中元素都会重新计算，然后通过accum累加到sum上。

;3.5.1
(define (integers-starting-from n)
  (cons-stream n
               (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))


(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve (stream-filter
            (lambda (x)
              (not (divisible? x (stream-car stream))))
            (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))


(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(define (add-streams s1 s2)
  (stream-map + s1 s2))
;ex3.53
(define s (cons-stream 1 (add-streams s s)))
;stream 1 2 4 8 16 32 ...

;ex3.54
(define (mul-stream s1 s2)
  (stream-map * s1 s2))
(define s (cons-stream 1 (mul-stream s integers)))
(stream-ref s 10)
;ex3.55
(define (partial-sums s)
  (define (partial-sums s accum)
    (cons-stream (+ (stream-car s) accum)
                 (partial-sums (stream-cdr s) (+ (stream-car s) accum))))
  (partial-sums s 0))
(define s (partial-sums integers))
(stream-ref s 4)
(display-stream s)
