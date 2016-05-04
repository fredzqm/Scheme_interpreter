(eval-many-exps '(

(define (caar ls)
  (car (car ls)))

(define (cdar ls)
  (cdr (car ls)))

(define (cadr ls)
  (car (cdr ls)))

(define (cddr ls)
  (cdr (cdr ls)))

(define (caaar ls)
  (car (caar ls)))

(define (cadar ls)
  (car (cdar ls)))

(define (caadr ls)
  (car (cadr ls)))

(define (caddr ls)
  (car (cddr ls)))

(define (cdaar ls)
  (cdr (caar ls)))

(define (cddar ls)
  (cdr (cdar ls)))

(define (cdadr ls)
  (cdr (cadr ls)))

(define (cdddr ls)
  (cdr (cddr ls)))

(define map
  (lambda (f ls . more)
    (if (null? more)
        (let map1 ([ls ls])
          (if (null? ls)
              '()
              (cons (f (car ls))
                    (map1 (cdr ls)))))
        (let map-more ([ls ls] [more more])
          (if (null? ls)
              '()
              (cons
                (apply f (car ls) (map car more))
                (map-more (cdr ls) (map cdr more))))))))

(define member
  (lambda (x ls)
    (cond
      [(null? ls) #f]
      [(equal? (car ls) x) ls]
      [else (member x (cdr ls))])))


))
