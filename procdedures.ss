(eval-one-exp
  '(define (caar ls)
    (car (car ls))))
(eval-one-exp
  '(define (cdar ls)
    (cdr (car ls))))
(eval-one-exp
  '(define (cadr ls)
    (car (cdr ls))))
(eval-one-exp
  '(define (cddr ls)
    (cdr (cdr ls))))

(eval-one-exp
  '(define (caaar ls)
    (car (caar ls))))
(eval-one-exp
  '(define (cadar ls)
    (car (cdar ls))))
(eval-one-exp
  '(define (caadr ls)
    (car (cadr ls))))
(eval-one-exp
  '(define (caddr ls)
    (car (cddr ls))))

(eval-one-exp
  '(define (cdaar ls)
    (cdr (caar ls))))
(eval-one-exp
  '(define (cddar ls)
    (cdr (cdar ls))))
(eval-one-exp
  '(define (cdadr ls)
    (cdr (cadr ls))))
(eval-one-exp
  '(define (cdddr ls)
    (cdr (cddr ls))))



; (eval-one-exp
;   '(define map
;     (lambda (f ls . more)
;       (if (null? more)
;           (let map1 ([ls ls])
;             (if (null? ls)
;                 '()
;                 (cons (f (car ls))
;                       (map1 (cdr ls)))))
;           (let map-more ([ls ls] [more more])
;             (if (null? ls)
;                 '()
;                 (cons
;                   (apply f (car ls) (map car more))
;                   (map-more (cdr ls) (map cdr more)))))))))

(eval-one-exp
  '(define map
    (lambda (f ls)
      (let map1 ([ls ls])
        (if (null? ls)
          '()
          (cons (f (car ls))
            (map1 (cdr ls))))))))

(eval-one-exp
  '(define member
    (lambda (x ls)
      (cond
        [(null? ls) #f]
        [(equal? (car ls) x) ls]
        [else (member x (cdr ls))]))))