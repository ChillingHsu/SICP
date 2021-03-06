(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? 'leaf (car object)))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))
(define init-leaves
  '((B 2) (C 1) (D 1) (A 4)))
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))
;ex2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)
;ex2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (encode-symbol sym tree)
  (define (in? elt set)
    (cond ((null? set) false)
          ((equal? elt (car set)) true)
          (else (in? elt (cdr set)))))
  (if (leaf? tree)
      '()
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (cond ((in? sym (symbols left))
               (cons 0
                     (encode-symbol sym left)))
              ((in? sym (symbols right))
               (cons 1
                     (encode-symbol sym right)))
              (else
                (error "no match symbol -- ENCODE-SYMBOL" sym))))))

(encode '(A D A B B C A) sample-tree)
;ex2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge leaves)
  (define (merge-leaf-list leaves count)
    (if (= 1 count)
        leaves
        (let ((first (car leaves))
              (second (cadr leaves)))
          (merge-leaf-list
           (adjoin-set (make-code-tree first second)
                       (cddr leaves))
           (- count 1)))))
  (car (merge-leaf-list leaves (length leaves))))
(generate-huffman-tree init-leaves)

;ex2.70
(define alphabet
  '((A 2) (BOOM 1) (NA 16) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))
(define lyrics-tree (generate-huffman-tree alphabet))
(+
  (length (encode '(Get a job) lyrics-tree))
  (length (encode '(Sha na na na na na na na na) lyrics-tree))
  (length (encode '(Get a job) lyrics-tree))
  (length (encode '(Sha na na na na na na na na) lyrics-tree))
  (length (encode '(Wah yip yip yip yip yip yip yip yip yip) lyrics-tree))
  (length (encode '(Sha boom) lyrics-tree)))
(* 3
  (+
    (length '(Get a job))
    (length '(Sha na na na na na na na na))
    (length '(Get a job))
    (length '(Sha na na na na na na na na))
    (length '(Wah yip yip yip yip yip yip yip yip yip))
    (length '(Sha boom))))
;使用Huffman编码需要84个二进制位，使用定长编码需要108个二进制位。
;ex2.71
;对于一个有n个符号的字母表，最频繁的符号需要用1个二进制位，最不频繁的符号需要n-1个二进制位。
;ex2.72
;对于最频繁的结点，需要O(1)步。
;对于最不频繁的结点，要遍历每一个结点，且每一个结点都会进行一次集合查询（判断符号在不在这个结点里），所以需要用O(n) * O(n) = O(n^2)步。
