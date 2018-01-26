;ex2.42
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
(define (flatmap proc sequence)
  (fold-right append
                '()
                (map proc sequence)))
(define (queens board-size)
  (define empty-board '())
  (define (adjoin-position position col sequence)
    (append sequence (list position)))
  (define (safe? col positions)
    (define (iter row current-col)
      (if (< current-col 1)
          #t
          (let ((upper-diagonal (+ row (- col current-col)))
                (lower-diagonal (- row (- col current-col)))
                (current-col-queen (list-ref positions (- current-col 1))))
               (and (not (= current-col-queen row))
                    (not (= current-col-queen upper-diagonal))
                    (not (= current-col-queen lower-diagonal))
                    (iter row (- current-col 1))))))
    (iter (list-ref positions (- col 1)) (- col 1)))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
(queens 8)
;ex2.43
;对于递归过程(queen-cols k)，原本的程序只会调用一次(queen-cols k-1)，而Louis的程序将(queen-cols (- k 1))放到了一个关于(enumerate-interval 1 board-size)的map里，着每次会调用board-size次(queen-cols k-1)。所以每层递归的时间都是原来的board-size倍，因为总共有board-size层递归，所以Louis的程序用时将会是原程序用时的(board-size^board-size)倍。
;所以这个程序需用时(board-size^board-size)*T。
