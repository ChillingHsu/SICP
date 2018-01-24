#lang sicp
;ex2.30
;直接定义square-tree
(define (square a)
    (* a a))
(define nil '())
; (define (square-tree tree)
;     (cond ((null? tree) nil)
;         ((not (pair? tree)) (square tree))
;         (else (cons (square-tree (car tree))
;                     (square-tree (cdr tree))))))
;使用map定义
; (define (square-tree tree)
;     (map (lambda (subtree)
;                  (if (pair? subtree)
;                      (square-tree subtree)
;                      (square subtree)))
;          tree))
;ex2.31
;进一步抽象出过程tree-map
(define (tree-map proc tree)
    (map (lambda (subtree)
         (if (pair? subtree)
             (tree-map proc subtree)
             (proc subtree)))
         tree))
(define (square-tree tree)
    (tree-map square tree))
(define t (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree t)
