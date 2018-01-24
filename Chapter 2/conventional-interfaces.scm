; #lang sicp
;2.2.3
;sequence as conventional interfaces
;序列作为约定的接口
(define (inc a)
    (+ a 1))
(define nil '())
(define (filter predicate sequence)
    (cond ((null? sequence) sequence)
          ((predicate (car sequence))
           (cons (car sequence) (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))
(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (inc low) high))))
(enumerate-interval 1 7)

(define (enumerate-tree tree); 规定enumerate-tree的所有返回值都是一个list
    (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;ex2.23
;使用累计accumulate过程定义一些基本的表操作
;comment for ex2.37
; (define (map p sequence)
;     (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (square a)
    (* a a))

    (define (append seq1 seq2)
    (accumulate cons seq2 seq1))
(append (list 1 2 3 4) (list 5 6 7 8))

(define (length sequence)
    (accumulate (lambda (x y) (inc y)) 0 sequence))
(length (list 1 2 3 4 5 1 2 3))
;ex2.34
(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms)
                        (+ this-coeff (* x higher-terms)))
                0
                coefficient-sequence))
(horner-eval 2 (list 1 3 0 5 0 1))
;ex2.35
(define (count-leaves tree)
    (accumulate +
                0
                (map (lambda (subtree)
                      (cond ((null? subtree) 0)
                            ((not (pair? subtree)) 1)
                            (else (count-leaves subtree))))
                     tree)))
(count-leaves (list 1 2 (list 3 4 (list 5 6) 7) 8 9))
;ex2.36
(define (accumulate-n op initial seqs)
    (if (null? (car seqs))
        nil
        (cons (accumulate op initial (map car seqs))
              (accumulate-n op initial (map cdr seqs)))))
(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
