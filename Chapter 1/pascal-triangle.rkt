#lang sicp
;ex1.12
;帕斯卡三角
(define (pascal-triangle row col)
  (if (or (= row 0) (= col 0) (= col row))
      1
      (+ (pascal-triangle (- row 1) col)
         (pascal-triangle (- row 1) (- col 1)))))
