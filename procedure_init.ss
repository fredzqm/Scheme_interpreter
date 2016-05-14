(define (addPredefinedProcedures)
(eval-many-exps '(

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

(define-class (stack)
  ([s '()])
  [(pop)
      (let ([x (car s)])
        (set! s (cdr s))
        x)]
  [(push e)
      (set! s (cons e s))])

(with-values
  (call/cc 
    (lambda (k)
      (set! exit-list k)))
  list)

)))
