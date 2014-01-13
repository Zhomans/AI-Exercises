;;Zachary Homans
;;March 2013

;;Example Sudoku Puzzle
(define my-sudoku '(((0 0 4) (8 0 0) (0 1 7))
                    ((6 7 0) (9 0 0) (0 0 0))
                    ((5 0 8) (0 3 0) (0 0 4))
                    ((3 0 0) (7 4 0) (1 0 0))
                    ((0 6 9) (0 0 0) (7 8 0))
                    ((0 0 1) (0 6 9) (0 0 5))
                    ((1 0 0) (0 8 0) (3 0 6))
                    ((0 0 0) (0 0 6) (0 9 1))
                    ((2 4 0) (0 0 1) (5 0 0))))

;;Prints out puzzle nicely.
(define read-sudoku
  (lambda (sudoku)
    (cond
      ((null? sudoku) (display "Done\n"))
      (else
       (display (car sudoku))
       (newline)
       (read-sudoku (cdr sudoku))))))

;-------------------------------------------------------------------------------------

;;Base Function. Takes a sudoku puzzle.
(define sudoku-solver
  (lambda (sudoku)
    (point-solver sudoku 0 0)))

;;Solves an individual point in the puzzle.
(define point-solver
  (lambda (sudoku row col)
    (cond
      ((> col 8) (point-solver sudoku (+ row 1) 0)) ;;If the point is out-of-bounds, place it back within bounds.
      ((> row 8) (point-solver sudoku 0 col))
      ((complete-puzzle? sudoku) (read-sudoku sudoku)) ;;If the puzzle is complete, read it out.
      ((> (locate-point sudoku row col) 0) (point-solver sudoku row (+ col 1))) ;;Ignore the point if it's already solved.
      (else (point-solver (replace-point sudoku (solve-point sudoku row col) row col) row (+ col 1)))))) ;;Solve point if possible and continue to next point.

;;Checks to see if puzzle is completed.
(define complete-puzzle?
  (lambda (sudoku)
    (complete-puzzle?-help (flatten sudoku))))

(define complete-puzzle?-help
  (lambda (sudoku)
    (cond
      ((null? sudoku) #t)
      ((equal? 0 (car sudoku)) #f)
      (else (complete-puzzle?-help (cdr sudoku))))))

;;Identifies the value of a point given the row and column.
(define locate-point
  (lambda (sudoku row col)
    (nth (flatten (nth sudoku row)) col)))

;;Replaces a point with its solution.
(define replace-point
  (lambda (sudoku new-point row col)
    (replace-nth sudoku row (replace-point-help new-point (nth sudoku row) col))))

(define replace-point-help
  (lambda (new-point row col)
    (cond
      ((and (<= 0 col) (>= 2 col)) (replace-nth row 0 (replace-nth (car row) col new-point)))
      ((and (<= 3 col) (>= 5 col)) (replace-nth row 1 (replace-nth (cadr row) (- col 3) new-point)))
      (else (replace-nth row 2 (replace-nth (nth row 2) (- col 6) new-point))))))

;;Actually solve the point based on surrounding rows and columns and its box.
(define solve-point
  (lambda (sudoku row col)
    (calculate-point (modulo row 3) (modulo col 3) (my-box sudoku row col) (my-row sudoku row) (my-col sudoku col) (nearby-rows sudoku row) (nearby-cols sudoku col))))

;;Generate list of points within a point's box.
(define my-box
  (lambda (sudoku row col)
    (cond
      ((and (<= 0 row) (>= 2 row)) (cons (nth (nth sudoku 0) (quotient col 3)) (cons (nth (nth sudoku 1) (quotient col 3)) (cons (nth (nth sudoku 2) (quotient col 3)) '()))))
      ((and (<= 3 row) (>= 5 row)) (cons (nth (nth sudoku 3) (quotient col 3)) (cons (nth (nth sudoku 4) (quotient col 3)) (cons (nth (nth sudoku 5) (quotient col 3)) '()))))
      (else (cons (nth (nth sudoku 6) (quotient col 3)) (cons (nth (nth sudoku 7) (quotient col 3)) (cons (nth (nth sudoku 8) (quotient col 3)) '())))))))

;;Generate list of points within a point's row.
(define my-row
  (lambda (sudoku row)
    (flatten (nth sudoku row))))

;;Generate list of points within a point's column.
(define my-col
  (lambda (sudoku col)
    (cond
      ((null? sudoku) '())
      (else (cons (nth (flatten (car sudoku)) col) (my-col (cdr sudoku) col))))))

;;Generate list of points within a point's nearby rows.
(define nearby-rows
  (lambda (sudoku row)
    (cond
      ((= 0 row) (cons (my-row sudoku 1) (cons (my-row sudoku 2) '())))
      ((= 1 row) (cons (my-row sudoku 0) (cons (my-row sudoku 2) '())))
      ((= 2 row) (cons (my-row sudoku 0) (cons (my-row sudoku 1) '())))
      ((= 3 row) (cons (my-row sudoku 4) (cons (my-row sudoku 5) '())))
      ((= 4 row) (cons (my-row sudoku 3) (cons (my-row sudoku 5) '())))
      ((= 5 row) (cons (my-row sudoku 3) (cons (my-row sudoku 4) '())))
      ((= 6 row) (cons (my-row sudoku 7) (cons (my-row sudoku 8) '())))
      ((= 7 row) (cons (my-row sudoku 6) (cons (my-row sudoku 8) '())))
      ((= 8 row) (cons (my-row sudoku 6) (cons (my-row sudoku 7) '()))))))

;;Generate list of points within a point's nearby columns.
(define nearby-cols
  (lambda (sudoku row)
    (cond
      ((= 0 row) (cons (my-col sudoku 1) (cons (my-col sudoku 2) '())))
      ((= 1 row) (cons (my-col sudoku 0) (cons (my-col sudoku 2) '())))
      ((= 2 row) (cons (my-col sudoku 0) (cons (my-col sudoku 1) '())))
      ((= 3 row) (cons (my-col sudoku 4) (cons (my-col sudoku 5) '())))
      ((= 4 row) (cons (my-col sudoku 3) (cons (my-col sudoku 5) '())))
      ((= 5 row) (cons (my-col sudoku 3) (cons (my-col sudoku 4) '())))
      ((= 6 row) (cons (my-col sudoku 7) (cons (my-col sudoku 8) '())))
      ((= 7 row) (cons (my-col sudoku 6) (cons (my-col sudoku 8) '())))
      ((= 8 row) (cons (my-col sudoku 6) (cons (my-col sudoku 7) '()))))))

;;Calculate the solution to the point.
(define calculate-point
  (lambda (row col box myrow mycol nrows ncols)
    (test-poss (poss-list box myrow mycol) row col box nrows ncols '() '())))

;;Determine the possible solutions to a point.
(define poss-list
  (lambda (box row col)
    (subtract '(1 2 3 4 5 6 7 8 9) (union (union row col) (flatten box)))))
    
;;Test a possibility to a point.
(define test-poss
  (lambda (lst-poss row col box nrows ncols def-work may-work)
    (cond
      ((not (null? def-work)) (car def-work)) ;;If there is only one possibility, it must be the solution.
      ((null? lst-poss) 0) ;;If nothing is certain, don't provide solution.
      ((and (null? (cdr lst-poss)) (null? def-work) (null? may-work)) (car lst-poss)) ;;If there's only one possibility left, it must be the solution.
      ((poss-work? (car lst-poss) row col box nrows ncols) (test-poss (cdr lst-poss) row col box nrows ncols (cons (car lst-poss) def-work) may-work)) ;;If a possibility works, remember that.
      (else (test-poss (cdr lst-poss) row col box nrows ncols def-work (cons (car lst-poss) may-work)))))) ;;Else, try the next possibility.

;;Test if a possibility works.
(define poss-work?
  (lambda (poss row col box nrows ncols)
    (only-space? (flatten (consider-ncols (consider-nrows box row poss nrows 1) col poss ncols 1)) 0)))

;;Consider nearby rows when testing possibility.
(define consider-nrows
  (lambda (box row poss nrows n)
    (cond
      ((= 3 n) box)
      ((element? poss (car nrows)) (cond
                                     ((and (= n 1) (= row 0)) (consider-nrows (replace-nth box 1 '(1 1 1)) row poss (cdr nrows) 2))
                                     ((and (= n 2) (= row 0)) (consider-nrows (replace-nth box 2 '(1 1 1)) row poss (cdr nrows) 3))
                                     ((and (= n 1) (= row 1)) (consider-nrows (replace-nth box 0 '(1 1 1)) row poss (cdr nrows) 2))
                                     ((and (= n 2) (= row 1)) (consider-nrows (replace-nth box 2 '(1 1 1)) row poss (cdr nrows) 3))
                                     ((and (= n 1) (= row 2)) (consider-nrows (replace-nth box 0 '(1 1 1)) row poss (cdr nrows) 2))
                                     ((and (= n 2) (= row 2)) (consider-nrows (replace-nth box 1 '(1 1 1)) row poss (cdr nrows) 3))))
      (else (consider-nrows box row poss (cdr nrows) (+ n 1))))))

;;Consider nearby columns when testing possibility.
(define consider-ncols
  (lambda (box col poss ncols n)
    (cond
      ((= 3 n) box)
      ((element? poss (car ncols)) (cond
                                     ((and (= n 1) (= col 0)) (consider-ncols (replace-nth-col box 1 1) col poss (cdr ncols) 2))
                                     ((and (= n 2) (= col 0)) (consider-ncols (replace-nth-col box 2 1) col poss (cdr ncols) 3))
                                     ((and (= n 1) (= col 1)) (consider-ncols (replace-nth-col box 0 1) col poss (cdr ncols) 2))
                                     ((and (= n 2) (= col 1)) (consider-ncols (replace-nth-col box 2 1) col poss (cdr ncols) 3))
                                     ((and (= n 1) (= col 2)) (consider-ncols (replace-nth-col box 0 1) col poss (cdr ncols) 2))
                                     ((and (= n 2) (= col 2)) (consider-ncols (replace-nth-col box 1 1) col poss (cdr ncols) 3))))
      (else (consider-ncols box col poss (cdr ncols) (+ n 1))))))

;;Replace a column with a new colmun.
(define replace-nth-col
  (lambda (box col replace)
    (cond
      ((null? box) '())
      (else (cons (replace-nth (car box) col replace) (replace-nth-col (cdr box) col replace))))))

;;If only one point could hold the possibility, then the possibility works.
(define only-space?
  (lambda (box count)
    (cond
      ((null? box) (if (= count 1) #t #f))
      ((equal? (car box) 0) (only-space? (cdr box) (+ count 1)))
      (else (only-space? (cdr box) count)))))






       
;Code I didn't write myself and/or Basic Code
;;--------------------------------------------------------------
(define flatten
  (lambda (lst)
    (if (null? lst)
        lst
        (append
          (rtn-lst (car lst))
          (flatten (cdr lst))))))

(define rtn-lst
  (lambda (lst)
    (cond 
      ((list? lst)
        (if (null? lst)
            empty
            (flatten lst)))
      (else 
        (list lst)))))       

(define nth
  (lambda (lst n)
    (cond
      ((null? lst) (display "Error: List Size"))
      ((equal? 0 n) (car lst))
      (else (nth (cdr lst) (- n 1))))))

(define replace-nth
  (lambda (lst n replace)
    (cond
      ((null? lst) '())
      ((equal? 0 n) (cons replace (replace-nth (cdr lst) (- n 1) replace)))
      (else (cons (car lst) (replace-nth (cdr lst) (- n 1) replace))))))
    
(define (element? x lst)
  (cond ((null? lst) #f)
        ((eq? x (car lst)) #t)
        (#t (element? x (cdr lst)))))

(define (union a b)
  (cond ((null? b) a)
        ((element? (car b) a)
         (union a (cdr b)))
        (#t (union (cons (car b) a) (cdr b)))))
    
(define (subtract a b)
  (cond ((null? a) '())
        ((element? (car a) b)
         (subtract (cdr a) b))
        (#t (cons (car a) (subtract (cdr a) b)))))