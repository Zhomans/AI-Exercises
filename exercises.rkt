;;Lists;;

;;Question 1
(define compress
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((null? (cdr lst)) (cons (car lst) (compress (cdr lst))))
      ((equal? (car lst) (cadr lst)) (compress (cdr lst)))
      (else (cons (car lst) (compress (cdr lst)))))))

;;Question 2
(define pack
  (lambda (lst)
    (cond
      ((null? lst) lst)
      ((null? (cdr lst)) lst)
      (else (pack-help lst '())))))

(define pack-help
  (lambda (lst temp-lst)
    (cond
      ((null? lst) '())
      ((null? (cdr lst)) (cons (cons (car lst) temp-lst) (pack-help (cdr lst) '())))
      ((equal? (car lst) (cadr lst)) (pack-help (cdr lst) (cons (car lst) temp-lst)))
      (else (cons (cons (car lst) temp-lst) (pack-help (cdr lst) '()))))))

;;Question 3
(define encode-direct
  (lambda (lst)
    (cond
      ((null? lst) lst)
      ((null? (cdr lst)) lst)
      (else (simplfy (encode-direct-help lst 0))))))

(define simplfy
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((equal? (car (car lst)) 1) (cons (cadr (car lst)) (simplfy (cdr lst))))
      (else (cons (car lst) (simplfy (cdr lst)))))))

(define encode-direct-help
  (lambda (lst n)
    (cond
      ((null? lst) '())
      ((null? (cdr lst)) (cons (cons (+ n 1) (list (car lst))) (encode-direct-help (cdr lst) 0)))
      ((equal? (car lst) (cadr lst)) (encode-direct-help (cdr lst) (+ n 1)))
      (else (cons (cons (+ n 1) (list (car lst))) (encode-direct-help (cdr lst) 0))))))

;;Question 4
(define repli
  (lambda (lst n)
    (repli-help lst n 0)))

(define repli-help
  (lambda (lst n m)
    (cond
      ((null? lst) '())
      ((= m n) (repli-help (cdr lst) n 0))
      (else (cons (car lst) (repli-help lst n (+ m 1)))))))

;;Question 5
(define drop
  (lambda (lst n)
    (drop-help lst n 1)))

(define drop-help
  (lambda (lst n m)
    (cond
      ((null? lst) '())
      ((= m n) (drop-help (cdr lst) n 1))
      (else (cons (car lst) (drop-help (cdr lst) n (+ m 1)))))))

;;Arithmetic;;

;;Question 1

(define is-prime
  (lambda (n)
    (cond
      ((= n 1) #f)
      ((or (= n 2) (= n 3)) #t)
      ((or (integer? (/ n 2)) (integer? (/ n 3))) #f)
      (else (is-prime-help n (sqrt n) 1)))))

(define is-prime-help
  (lambda (n sqrtn k)
    (cond
      ((> (- (* 6 k) 1) sqrtn) #t)
      ((or (integer? (/ n (- (* 6 k) 1))) (integer? (/ n (+ (* 6 k) 1)))) #f)
      (else (is-prime-help n sqrtn (+ k 1))))))

;;Question 2

(define my-gcd
  (lambda (a b)
    (cond
      ((= (- a b) 0) b)
      ((negative? (- a b)) (my-gcd b a))
      (else (my-gcd (- a b) b)))))

;;Question 3

(define prime-factors
  (lambda (n)
    (prime-factors-help n 2)))

(define prime-factors-help
  (lambda (n m)
    (cond
      ((is-prime n) (list n))
      ((is-prime m) (if (integer? (/ n m)) 
                        (cons m (prime-factors-help (/ n m) m)) 
                        (prime-factors-help n (+ m 1))))
      (else (prime-factors-help n (+ m 1))))))

;;Question 4

(define prime-factors-mult
  (lambda (n)
    (prime-factors-mult-help n 2 0)))

(define prime-factors-mult-help
  (lambda (n m c)
    (cond
      ((= n 1) (list (list m c)))
      ((is-prime m) (if (integer? (/ n m)) 
                        (prime-factors-mult-help (/ n m) m (+ c 1))
                        (if (> c 0)
                            (cons (list m c) (prime-factors-mult-help n (+ m 1) 0))
                            (prime-factors-mult-help n (+ m 1) 0))))
      (else (prime-factors-mult-help n (+ m 1) 0)))))


;;Trees;;

;;Question 1

(define cbal-tree
  (lambda (n)
    (cbal-tree-help n 1)))

(define cbal-tree-help
  (lambda (n m)
    (cond
      ((= n 0) '(nil))
      ((= n m) '(x nil nil))
      (else (append '(x) (list (cbal-tree-help n (+ m 1))) (list (cbal-tree-help n (+ m 1))))))))

;;Question 2

(define symm-tree?
  (lambda (tree)
    (cond
      ((and (equal? (cadr tree) 'nil) (equal? (caddr tree) 'nil)) #t)
      ((or (equal? (cadr tree) 'nil) (equal? (caddr tree) 'nil)) #f)
      (else (and (symm-tree? (cadr tree)) (symm-tree? (caddr tree)))))))

;;Question 3

(define count-leaves
  (lambda (tree)
    (cond
      ((equal? tree 'nil) 0)
      ((and (equal? (cadr tree) 'nil) (equal? (caddr tree) 'nil)) 1)
      (else (+ (count-leaves (cadr tree)) (count-leaves (caddr tree)))))))

;;Question 4

(define leaves
  (lambda (tree)
    (cond
      ((equal? tree 'nil) '())
      ((and (equal? (cadr tree) 'nil) (equal? (caddr tree) 'nil)) (car tree))
      (else (flatten (append  (list (leaves (cadr tree))) (list (leaves (caddr tree)))))))))

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

;;SICP;;

;;Question 2.4
;;(define (cons x y)
;;  (lambda (m) (m x y)))

;;(define (car z)
;;  (z (lambda (p q) p)))

;;(define (cdr z)
;;  (z (lambda (p q) q)))

;;This is true for any objects x and y. (car (cons x y)) -> ((lambda (m) (m x y)) (lambda (p q) p)) -> ((lambda (p q) p) x y) -> x. The same process can be done to show cdr gives the same result but with y.



;;Question 2.5
(define (consx x y)
  (* (expt 2 x) (expt 3 y)))

(define (carx z)
  (let ((g (/ z 2)))
    (cond
      ((integer? g) (+ 1 (carx g)))
      (else 0))))

(define (cdrx z)
  (let ((g (/ z 3)))
    (cond
      ((integer? g) (+ 1 (cdrx g)))
      (else 0))))