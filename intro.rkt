(define fibonacci
  (lambda (n)
    (if (= n 1)
        1
        (if (= n 2)
            1
            (+ (factorial (- n 1)) (factorial (- n 2)))))))

(define double
  (lambda (n)
    (* 2 n)))

(define listTotal
  (lambda (list)
    (if (null? list)
        0
        (+ (car list) (listTotal (cdr list))))))

(define findNth
  (lambda (list n)
    (if (= n 1)
        (car list)
        (findNth (cdr list) (- n 1)))))

(define doubleList
  (lambda (list)
    (if (null? list)
        '()
        (cons (double (car list)) (doubleList (cdr list))))))

(define member?
  (lambda (elt lst)
    (if (null? lst)
        #f
        (if (equal? elt (car lst))
            #t
            (member? elt (cdr lst))))))

;(define reverseList
  ;(lambda (lst)
    ;(if (null? lst)
        