#lang sicp
;ex2.29a
;定义mobile和branch和相应的选择函数
(define (make-mobile left right)
  (list left right))
(define (left-branch mob)
  (car mob))
(define (right-branch mob)
  (cadr mob))
(define (make-branch length structure)
  (list length structure))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))
;ex2.29b
(define (branch-weight b)
    (let ((b-s (branch-structure b)))
    (if (pair? b-s)
      (mobile-weight b-s)
      b-s)))
(define (mobile-weight m)
  (if (pair? m)
    (+ (branch-weight (left-branch m))
    (branch-weight (right-branch m)))
    m))
(define (total-weight mob)
  (mobile-weight mob))
(define mobile
  (make-mobile
  (make-branch 2 4)
  (make-branch 2
               (make-mobile
                (make-branch 1 3)
                (make-branch 3 1)))))
(total-weight mobile)
;ex2.29c
(define (torque branch)
  (* (mobile-weight (branch-structure branch))
    (branch-length branch)))

(define (balanced? mob)
   (if (pair? mob)
     (let ((r-b (right-branch mob))
           (l-b (left-branch mob)))
      (and (= (torque r-b)
              (torque l-b))
           (balanced? (branch-structure l-b))
           (balanced? (branch-structure r-b))))
     #t))
(balanced? mobile)
