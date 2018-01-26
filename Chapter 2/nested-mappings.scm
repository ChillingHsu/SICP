(define nil '())
(define (inc a) (+ a 1))
(define (dec a) (- a 1))
(define (accmulate op initial sequence)
    (if (null? sequence)
        nil
        (op (car sequence)
            (accmulate op initial (cdr sequence)))))
(define (flatmap proc seq)
    (accmulate append
               nil
               (map proc seq)))
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

(define (filter predicate sequence)
    (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (inc low) high))))
(define (prime-sum? pair)
    (prime? (+ (car pair)
               (cadr pair))))
(define (make-pair-sum pair)
    (list (car pair)
          (cadr pair)
          (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
    (map make-pair-sum
        (filter prime-sum?
            (flatmap
             (lambda (i)
               (map (lambda (j) (list i j))
                    (enumerate-interval 1 (dec i))))
             (enumerate-interval 1 n)))))
(prime-sum-pairs 6)
;flatmap的作用是将给定序列的映射用append连接起来
(define (remove item sequence)
    (filter (lambda (x) (not (= x item))) sequence))
(define (permutation s)
    (if (null? s)
        (list nil)  ;空集的所有排列为包含一个空集的集合
        (flatmap
            (lambda (x)
                (map (lambda (p) (cons x p))
                    (permutation (remove x s))))
            s)))
;ex2.40
(define (unique-pairs n)
    (flatmap (lambda (i)
                (map (lambda (j) (list i j))
                     (enumerate-interval 1 (dec i))))
             (enumerate-interval 1 n)))
(define (prime-sum-pairs n)
    (map make-pair-sum
        (filter prime-sum?
            (unique-pairs n))))
(prime-sum-pairs 6)
;ex2.41
;两种实现
(define (distinct-triples n)
    (filter (lambda (triple)
                (let ((i (car triple))
                      (j (cadr triple))
                      (k (cadr (cdr triple))))
                    (and (not (= i j))
                         (not (= i k))
                         (not (= j k)))))
            (flatmap
            (lambda (i)
                (flatmap (lambda (j)
                           (map (lambda (k) (list i j k))
                                (enumerate-interval 1 n)))
                         (enumerate-interval 1 n)))
            (enumerate-interval 1 n))))
(distinct-triples 4)
(define (enumerate-n-demension-interval n low high)
    (if (< n 1)
        (list nil)
        (flatmap
          (lambda (rest-d)
            (map (lambda (i) (cons i rest-d))
                 (enumerate-interval low high)))
          (enumerate-n-demension-interval (dec n) low high))))
(define (in? i items)
    (cond ((null? items) #f)
          ((= i (car items)) #t)
          (else (in? i (cdr items)))))
(define (unique-list? items)
    (cond ((null? items) #t)
          ((in? (car items) (cdr items)) #f)
          (else (unique-list? (cdr items)))))
(define (distinct-triples n s)
    (filter (lambda (triple)
                (let ((i (car triple))
                      (j (cadr triple))
                      (k (cadr (cdr triple))))
                     (= s
                        (+ i j k))))
            (filter unique-list?
                    (enumerate-n-demension-interval 3 1 n))))
(distinct-triples 20 10)
