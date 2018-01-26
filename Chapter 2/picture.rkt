#lang sicp
(#%require sicp-pict)
(paint einstein)
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
(paint (right-split einstein 4))
;ex2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
(paint (corner-split einstein 4))

(define (square-limit corner-painter)
  (let ((up-left (flip-horiz corner-painter))
        (up-right corner-painter))
    (let ((up (beside up-left up-right)))
      (let ((down (flip-vert up)))
        (below down up)))))
;ex2.45
(define (split origin-placer split-placer)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split origin-placer split-placer) painter (- n 1))))
          (origin-placer painter (split-placer smaller smaller))))))
(paint ((split beside below) einstein 4))
(paint ((split below beside) einstein 4))
