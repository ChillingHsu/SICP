#lang sicp
;2.1.4
;区间算数
;ex2.7
(define (make-interval a b)
    (cons (min a b) (max a b)))
(define (lower-bound interval)
    (car interval))
(define (upper-bound interval)
    (cdr interval))

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))
;ex2.8
;定义区间减法
(define (sub-interval x y)
    (make-interval (- (lower-bound x) (lower-bound y))
                   (- (upper-bound x) (upper-bound y))))
; (define (mul-interval x y)
;     (let ((p1 (* (lower-bound x) (lower-bound y)))
;           (p2 (* (lower-bound x) (upper-bound y)))
;           (p3 (* (upper-bound x) (lower-bound y)))
;           (p4 (* (upper-bound x) (upper-bound y))))
;         (make-interval (min p1 p2 p3 p4)
;                        (max p1 p2 p3 p4))))
;判断区间类别
(define (negative-interval? interval)
    (< (upper-bound interval) 0))
(define (span-zero-interval? interval)
    (and (not (> (lower-bound interval) 0))
         (not (< (upper-bound interval) 0))))
(define (positive-interval? interval)
    (> (lower-bound interval) 0))
;ex2.11
;一个区间可以按照端点分类为负区间，横跨零区间，正区间
;按减区间和被减区间分别的三种类型，将区间乘法分为 3*3 九种类型
;每种类型所需的乘法都不超过两次
;改进区间乘法
(define (mul-interval x y)
    (cond ((negative-interval? x)
        (cond ((negative-interval? y)
            (make-interval (* (upper-bound x) (upper-bound y))
                           (* (lower-bound x) (lower-bound y))))
              ((span-zero-interval? y)
            (make-interval (* (lower-bound x) (upper-bound y))
                           (* (lower-bound x) (lower-bound y))))
              ((positive-interval? y)
            (make-interval (* (lower-bound x) (upper-bound y))
                           (* (upper-bound x) (lower-bound y))))))
        ((span-zero-interval? x)
        (cond ((negative-interval? y)
            (make-interval (* (upper-bound x) (lower-bound y))
                           (* (lower-bound x) (lower-bound y))))
            ((span-zero-interval? y)
            (make-interval (min (* (upper-bound x) (lower-bound y))
                                (* (lower-bound x) (upper-bound y)))
                           (max (* (lower-bound x) (lower-bound y))
                                (* (upper-bound x) (upper-bound y)))))
            ((positive-interval? y)
            (make-interval (* (lower-bound x) (upper-bound y))
                           (* (upper-bound x) (upper-bound y))))))
        ((positive-interval? x)
        (cond ((negative-interval? y)
            (make-interval (* (upper-bound x) (lower-bound y))
                           (* (lower-bound x) (upper-bound y))))
            ((span-zero-interval? y)
            (make-interval (* (upper-bound x) (lower-bound y))
                           (* (upper-bound x) (upper-bound y))))
            ((positive-interval? y)
            (make-interval (* (lower-bound x) (lower-bound y))
                           (* (upper-bound x) (upper-bound y))))))))
;以中心-宽度的方法定义区间
(define (make-center-width c w)
    (make-interval (- c w) (+ c w)))
(define (center interval)
    (/ (+ (upper-bound interval) (lower-bound interval)) 2.0))
(define (width interval)
    (/ (- (upper-bound interval) (lower-bound interval)) 2.0))
;ex2.12
;以中心-百分比的方法定义区间
(define (make-center-percent c r)
    (make-center-width c (* c r)))
(define (percent interval)
    (/ (width interval) (center interval)))
;ex2.13
;当两个区间都为正区间时，其成绩的百分数误差值为
; (/ (- (upper-bound interval) (lower-bound interval))
;    (+ (upper-bound interval) (lower-bound interval)))

;ex2.10
;一个区间可以按照端点分类为负区间，横跨零区间，正区间
;重新定义区间除法
(define (div-interval x y)
    (if (span-zero-interval? y)
        (error "divide span-zero-interval -- DIV" y)
        (mul-interval x
            (make-interval (/ 1.0 (upper-bound y))
                           (/ 1.0 (lower-bound y))))))
; (define (width-interval interval)
;     (/ (- (upper-bound interval)
;           (lower-bound interval))
;         2.0))
(define (printf-interval interval)
    (display "[")
    (display (lower-bound interval))
    (display ",")
    (display (upper-bound interval))
    (display "] width = ")
    (display (width interval))
    (newline))

(define intv-a
    (make-interval -1 1))
(define intv-b
    (make-interval 5 15))
(printf-interval (add-interval intv-a
                               (mul-interval intv-a intv-a)))
;ex2.9
;区间宽度及其组合区间的宽度函数
(printf-interval (sub-interval intv-a intv-b))
(width (sub-interval intv-a intv-b))
;对于加减法而言，组合区间的宽度width = (abs intv-a.width +- intv-b.width)
(printf-interval (mul-interval intv-a intv-b))
(width (mul-interval intv-a intv-b))
;对于乘除法而言组合区间的宽度与原区间的宽度并没有显然的函数关系
(percent (make-center-percent 6.8 0.1))

;ex2.14
;Lem是对的，1/(1/A)的结果为1，但是(A/A)的结果不为1
(define intv-one
    (make-interval 1 1))
(define A
    (make-center-percent 10000 0.01))
(define B
    (make-center-percent 5 0.01))

(mul-interval A intv-one)
(div-interval intv-one (div-interval intv-one A))
(center (div-interval A A))
(percent (div-interval A A))
;ex2.15
;说法正确
;一个公式可以写成一种形式，其中具有非准确性的变量不重复出现。那么这个公式的结果更精确。
;因为多次出现的具有非准确性的变量，可能在每次求值中具有不同的值，比如一个区间A为[4,6]
;在同一个公式的多次出现分别取类似于4.5,5.5这样的值，然而在实际生活中这样的情况并不会出现。
;为了避免这样的情况导致的误差，就应该尽量少的在同一个公式中出现多次相同的具有非准确性的变量。
(define (par1 r1 r2)
    (div-interval (mul-interval r1 r2)
                  (add-interval r1 r2)))
(define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
        (div-interval one
                      (add-interval (div-interval one r1)
                                    (div-interval one r2)))))
;ex2.16
;不能做到。
;等价的代数表达式导致不同的计算结果，主要是由于区间算数（Interval arithmetic）中的依赖问题（Dependency problem）。
;依赖问题是区间算数应用中的一个主要障碍。尽管区间算数可以非常精确的确定基本算术运算和函数的范围，但是对于更为复杂的函数，区间算数并不能得到非常精确的结果。如果同一个区间作为参数在计算过程中出现了几次，并且每一次出现都独立取得不同的值，呢么就会导致结果区间会出现不期望的扩张。
;The so-called dependency problem is a major obstacle to the application of interval arithmetic. Although interval methods can determine the range of elementary arithmetic operations and functions very accurately, this is not always true with more complicated functions. If an interval occurs several times in a calculation using parameters, and each occurrence is taken independently then this can lead to an unwanted expansion of the resulting intervals.
;通常来说，能够证明如果每个区间变量在函数f中仅出现一次且函数f在区间中连续，那么就能得到一个精确的结果区间。然而不是每个函数都能写作每个区间变量只出现一次的形式。
;In general, it can be shown that the exact range of values can be achieved, if each variable appears only once and if f is continuous inside the box. However, not every function can be rewritten this way.
;依赖问题导致区间结果会包含一个过大的范围，这会导致结果失去了精确性。
;The dependency of the problem causing over-estimation of the value range can go as far as covering a large range, preventing more meaningful conclusions.
;https://en.wikipedia.org/wiki/Interval_arithmetic
