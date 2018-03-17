;;There will be some junk output in loading.
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

;;ex3.66
(define (index-of s t)
  (if (= s t)
      (- (expt 2 s) 1)
      (+ (index-of s s)
         (expt 2 (- s 1))
         (* (- (- t s) 1) (expt 2 s)))))
(display-stream (pairs integers integers) 10)

(stream-ref (pairs integers integers) (- (index-of 1 100) 1))
;;设F(s,t)是(s,t)在整个序列中出现的序数。显然F(1,1) = 1, F(1,2) = 2。类似可以推出等式
;; F(s,t) = 2^s - 1 , s = t
;;        = 2^s - 1 + 2^(s-1) + (t-s-1) * 2^s
(- (index-of 1 100) 1)
;; (1 100)之前有197个序对
;; Value: 197
(- (index-of 100 100) 1)
;; (100 100)之前有1267650600228229401496703205374个序对
;; Value: 1267650600228229401496703205374

;; ex3.67
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
;; another implementation
;;  (define (all-pairs s t)
;;    (cons-stream
;;      (list (stream-car s) (stream-car t))
;;      (interleave
;;        (stream-map (lambda (x) (list (stream-car s) x))
;;                    (stream-cdr t))
;;        (all-pairs (stream-cdr s) t))))

(display-stream
 (stream-filter (lambda (pair)
                  (prime? (+ (car pair) (cadr pair))))
                (pairs integers integers))
 10)

;; ex3.68
;; 不能，这样做会导致interleave会不断的调用自己产生无限循环。

;; ex3.69
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
                 (tripes integers integers integers)) 1)
;; ex3.70
;; a)
(define (merge-weighted weight s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s2)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (let ((s1w (weight s1car))
                 (s2w (weight s2car)))
             (cond ((< s1w s2w)
                    (cons-stream s1car (merge-weighted weight
                                                       (stream-cdr s1)
                                                       s2)))
                   ((> s1w s2w)
                    (cons-stream s2car (merge-weighted weight
                                                       s1
                                                       (stream-cdr s2))))
                   (else
                    (cons-stream s1car
                                 (cons-stream s2car
                                              (merge-weighted weight
                                                              (stream-cdr s1)
                                                              (stream-cdr s2)))))))))))
(define (pair-weight pair)
  (+ (car pair) (cadr pair)))

(define (weighted-pairs pair-weight s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted pair-weight
                   (stream-map (lambda (x)
                                 (list (stream-car s) x))
                               (stream-cdr t))
                   (weighted-pairs pair-weight (stream-cdr s)
                                   (stream-cdr t)))))
(display-stream (weighted-pairs pair-weight integers integers) 10)

;; b)
(define (235-pair-weight pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* 2 i)
       (* 3 j)
       (* 5 i j))))
(display-stream (weighted-pairs 235-pair-weight S S) 10)
;; ex3.71
(define (cube x) (* x x x))
(define (ramanujan-weight pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (cube i)
       (cube j))))

(define (get-repeat-weighted-items weight stream prev-item)
  (if (or (= (weight (stream-car stream))
             (weight (stream-car (stream-cdr stream))))
          (if prev-item
              (= (weight (stream-car stream))
                 (weight prev-item))
              false))
      (cons-stream (stream-car stream)
                   (get-repeat-weighted-items weight (stream-cdr stream) (stream-car stream)))
      (get-repeat-weighted-items weight (stream-cdr stream) (stream-car stream))))
(define ramanujan-pairs (get-repeat-weighted-items
                         ramanujan-weight
                         (weighted-pairs ramanujan-weight integers integers)
                         false))
(display-stream (stream-map (lambda (x) (cons (ramanujan-weight x) x))
                            p                           ramanujan-pairs) 12)
;; 前六个Ramanujan数是
;; 1729
;; 4104
;; 13832
;; 20683
;; 32832
;; 39312

;; ex3.72
(define (square-weight pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (square i)
       (square j))))

(define square-pairs (get-repeat-weighted-items
                      square-weight
                      (weighted-pairs square-weight integers integers)
                      false))
(display-stream (stream-map (lambda (x) (cons (square-weight x) x))
                            square-pairs) 10)
(define (get-substream stream weight)
  (if (null? stream)
      '()
      (if (null? (stream-cdr stream))
          (cons-stream (stream-car stream)
                       '())
          (if (= (weight (stream-car stream))
                 (weight (stream-car (stream-cdr stream))))
              (cons-stream (stream-car stream)
                           (get-substream (stream-cdr stream) weight))
              (cons-stream (stream-car stream)
                           '())))))
(define (get-reststream stream weight)
  (if (= (weight (stream-car stream))
         (weight (stream-car (stream-cdr stream))))
      (get-reststream (stream-cdr stream) weight)
      (stream-cdr stream)))

(define (split-stream stream weight)
  (cons-stream (stream->list (get-substream stream weight))
               (split-stream
                (get-reststream stream weight)
                weight)))
(define tripe-square-pairs
  (stream-filter
   (lambda (x) (= (length x) 3))
   (split-stream square-pairs square-weight)))
(display-stream (stream-map
                 (lambda (x)
                   (cons (square-weight (car x))
                         x))
                 tripe-square-pairs) 10)
