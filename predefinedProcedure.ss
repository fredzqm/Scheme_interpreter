(eval-one-exp
  '(define member
    (lambda (var ls)
      (cond
        [(null? ls) #f]
        [(eq? var (car ls)) ls]
        [else (member var (cdr ls))]))))


