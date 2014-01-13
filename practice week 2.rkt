(define remdups
  (lambda (lst)
    (if (null? lst)
        "Null list"
        (remdups-help '() lst))))

(define remdups-help
  (lambda (seen-lst lst)
    (if (null? lst)
        seen-lst
        (if (member (car lst) seen-lst)
            (remdups-help seen-lst (cdr lst))
            (remdups-help (append seen-lst (list (car lst))) (cdr lst))))))

(define substitute
  (lambda (wtr rpl lst)
    (if (null? lst)
        lst
        (if (equal? wtr (car lst))
            (append (list rpl) (substitute wtr rpl (cdr lst)))
            (append (list (car lst)) (substitute wtr rpl (cdr lst)))))))

(define false-n-list
  (lambda (n)
    (if (equal? n 0)
        '()
        (cons #f (false-n-list (- n 1))))))

(define false-n-n-list
  (lambda (n)
    (false-n-n-list-help n n)))

(define false-n-n-list-help
  (lambda (n-const n-count)
    (if (equal? n-count 0)
        '()
        (cons (false-n-list n-const) (false-n-n-list-help n-const (- n-count 1))))))

(define n-queens
  (lambda (n)
    (n-queens-help (false-n-n-list n))))

(define n-queens-help
  (lambda (board)
    