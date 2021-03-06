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
(define (display-stream stream lines)
  (display-line (stream-car stream))
  (if (= lines 1)
      'done
      (display-stream (stream-cdr stream) (- lines 1))))

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

;ex3.56
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
(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))
(display-stream S 10)

;ex3.57
;如果stream是用来memo-proc的记忆优化，对于斐波那契流来说，计算Fib(n)需要做n-1次加法。
;Fib(0) = 0
;Fib(1) = 0
;Fib(n) = n - 1

;对于没有优化的流来说，计算Fib(n)需要Fib(n-1) + Fib(n-2) + 1次加法，也就是大约为黄金分割为基数的指数函数，即O(1.618^n)
;如下为验证过程
; 全1流
(define ones (cons-stream 1. ones))
; 定义两个流的除法
(define (div-stream s1 s2)
  (stream-map / s1 s2))
;
; 定义加法次数流，即
; Fib(n)需要Fib(n-1) + Fib(n-2) + 1
; Fib(0) = 0
; Fib(1) = 0
(define s (cons-stream 0
            (cons-stream 0
                         (add-streams s
                                      (add-streams (stream-cdr s)
                                                   ones)))))
; 为了方便计算，去掉流中前两个为0的项
(define s (stream-cdr (stream-cdr s)))
; 使用除法定义流，即Fib(n)/Fib(n-1)
(define ratio (div-stream (stream-cdr s) s))

(display-stream ratio 100)

;ex3.58
(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))
;求在radix位进位制下分数num/den的各位数位流
(display-stream (expand 1 7 10) 10)
; 1
; 4
; 2
; 8
; 5
; 7
; 1
; 4
; 2
; 8
;Value: done
; 10进制下1/7 = 0.142857...
(display-stream (expand 3 8 10) 10)
; 3
; 7
; 5
; 0
; 0
; 0
; 0
; 0
; 0
; 0
;Value: done
; 10进制下3/8 = 0.375000...

;ex3.59
;a)
(define (integrate-series power-series)
  (div-stream power-series
              (integers-starting-from 1)))
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

;b)
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series
  (cons-stream 1 (scale-stream
                  (integrate-series sine-series)
                 -1)))
(display-stream cosine-series 10)

;ex3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams
                 (scale-stream (stream-cdr s2) (stream-car s1))
                 (mul-series (stream-cdr s1) s2))))
(define one (add-streams (mul-series cosine-series cosine-series)
                         (mul-series sine-series sine-series)))
(display-stream one 10)
;ex3.61
(define (reciprocal s)
  (let ((c (stream-car s))
        (sr (stream-cdr s)))
    (cons-stream (/ 1 c)
                 (mul-series (reciprocal s)
                             (scale-stream sr (- (/ 1 c)))))))
;ex3.62
(define (div-series s1 s2)
  (mul-series s1 (reciprocal s2)))
(define tan-series (div-series sine-series cosine-series))
(define (evaluate series x)
  (define (evaluate series coeff accum)
    (let ((new-accum (+ accum (* coeff (stream-car series)))))
      (cons-stream new-accum
                   (evaluate (stream-cdr series) (* x coeff) new-accum))))
  (evaluate series 1 0))

(display-stream (evaluate tan-series 1.) 10)
(tan 1)

(define (average a b) (/ (+ a b) 2))
(define (sqrt-stream x)
  (define (sqrt-improve guess x)
    (average guess (/ x guess)))
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                              guesses)))
  guesses)
(display-stream (sqrt-stream 2) 10)

(define (pi-summands n)
  (cons-stream (/ 1 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1.)) 4))
(display-stream pi-stream 10)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))
(display-stream (euler-transform pi-stream) 10)

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))
(display-stream (accelerated-sequence euler-transform pi-stream) 10)

;ex3.63
;Louis的实现会让sqrt-stream在每次递归时都重新构造一个新的流，会让时间复杂度由O(n)退化到O(n^2)。而如果没有memo-proc优化，两种实现都会是O(n^2)的复杂度，在效率方面就没有差异了。

;ex3.64
(define (stream-limit stream tolerance)
  (if (< (abs (- (stream-car (stream-cdr stream))
                 (stream-car stream)))
         tolerance)
      (stream-car (stream-cdr stream))
      (stream-limit (stream-cdr stream) tolerance)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
(sqrt 2 0.000001)

;ex3.65
(define (ln2 n)
  (cons-stream (/ 1. n)
               (stream-map - (ln2 (+ 1 n)))))
(define alternating-series
  (cons-stream 1 (stream-map - alternating-series)))
;another implementation
(define ln2
  (mul-stream alternating-series
              (div-stream ones
                          integers)))
(display-stream (accelerated-sequence euler-transform (partial-sums ln2)) 10)
(log 2)
