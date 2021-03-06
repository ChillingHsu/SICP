(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set x (cdr set)))))
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2) (intersection-set (cdr set1) set2))
              ((> x1 x2) (intersection-set set1 (cdr set2)))))))
;ex2.61
;最坏情况下，要加入的元素是集合中的最大元素，此时所需要的步骤和采用未排序的表示时一样。但另一方面，如果需要插入许多不同大小的项，我们总可以期望，有些时候这一检索可以在接近表开始处的某一点停止，也有些时候需要检查表的一大部分，平均而言，我们可以期望需要检查表中的一半元素，这样平均所需要的步数就是大约n/2。
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))
(adjoin-set 10 '(1 3 4 5))

;ex2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1))
                (x2 (car set2)))
            (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                  ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                  ((> x1 x2) (cons x2 (union-set set1 (cdr set2)))))))))
