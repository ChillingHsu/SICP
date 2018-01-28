(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))
(define bst-1
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))
(define bst-2
  (make-tree 3
             (make-tree 1 '() '())
             (make-tree 7
                        (make-tree 5 '() '())
                        (make-tree 9
                                   '()
                                   (make-tree 11 '() '())))))
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))
;ex2.63
;对所有的树都产生同样的结果。
;两个过程中都会n次递归，对于每次递归
;tree->list-1会调用append，该过程增长速度为O(log n)，所以总的增长速度为O(nlog n)。
;tree->list-2会调用cons，该过程增长速度为O(1)，所以总的增长速度为O(n)。
;所以tree->list-2增长速度更慢。
(tree->list-1 bst-2)
(tree->list-2 bst-2)

;ex2.64
;由有序列表构造一个平衡树，需要选定位于列表中间的元素this-entry作为根结点，然后将原序列划分为两部分分别以同样的方法构造两个平衡树作为左右子树。
;这里的子过程partial-tree用到了一个特殊的序对作为返回值，避免了手动划分输入序列，直接用参数n划分了整个过程，降低了所需步数。
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))
(list->tree (list 1 3 5 7 9 11))
;a)
; 5
; | \
; 1  9
;  \  |\
;   3 7 11
;b) 对于一个长度为n的列表，partial-tree会将每个元素作为根结点构造一次子树，每次的步数为O(1)，所以总的步数以O(n)的量级增长。
;ex2.65
;由于list->tree和tree->list以及有序表的合并和交集都有O(n)的实现，所以可以将BST转化为list再进行操作。
(define tree->list tree->list-2)

(define (intersection-set-as-ordered-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set-as-ordered-list (cdr set1) (cdr set2))))
              ((< x1 x2) (intersection-set-as-ordered-list (cdr set1) set2))
              ((> x1 x2) (intersection-set-as-ordered-list set1 (cdr set2)))))))

(define (union-set-as-ordered-list set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1))
                (x2 (car set2)))
            (cond ((= x1 x2) (cons x1 (union-set-as-ordered-list (cdr set1) (cdr set2))))
                  ((< x1 x2) (cons x1 (union-set-as-ordered-list (cdr set1) set2)))
                  ((> x1 x2) (cons x2 (union-set-as-ordered-list set1 (cdr set2)))))))))
(define (intersection-set set1 set2)
  (list->tree
    (intersection-set-as-ordered-list
      (tree->list set1)
      (tree->list set2))))
(define (union-set set1 set2)
  (list->tree
    (union-set-as-ordered-list
      (tree->list set1)
      (tree->list set2))))

(union-set bst-1 bst-2)

;ex2.66
(define (key x) x)
(define (lookup given-key tree)
  (cond ((null? tree) false)
        ((equal? given-key (key (entry tree))) (entry tree))
        ((< given-key (key (entry tree)))
         (lookup given-key (left-branch tree)))
        ((> given-key (key (entry tree)))
         (lookup given-key (right-branch tree)))))
