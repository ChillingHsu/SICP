(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;ex2.59
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1) (adjoin-set (car set1) set2))))
;ex2.60
;allow duplicates
;; same procedure (element-of-set?) O(n)
;; same procedure (intersection-set set1 set2) O(n^2)
(define (adjoin-set x set) ;; O(1)
  (cons x set))
(define (union-set set1 set2) ;; O(n)
  (append set1 set2))
(intersection-set '(1 2 3) '(2 3 4 4 2 3 1))
