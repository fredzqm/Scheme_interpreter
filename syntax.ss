(define-datatype syntax-pattern syntax-pattern?
  [listpt (carpt syntax-pattern?) (cdrpt syntax-pattern?)]
  [sympt (id symbol?)]
  [exprpt (id symbol?)]
  [multpt (eachpt syntax-pattern?)]
  [contpt (symbol? sym)]
  [emptpt]
  )

(define-datatype result-pattern result-pattern?
  [listpt-r (pts (list-of result-pattern?))]
  [multpt-r (i number?) (eachrpt result-pattern?)]
  [exprpt-r (id symbol?)]
  [contpt-r (sym symbol?)]
  )

(define matchRule
  (lambda (pattern result body)
    (let ([matches (matchpattern pattern body)])
      (and matches (assembleResult result matches '())))))

(define matchpattern
  (lambda (pattern body)
    (cases syntax-pattern pattern
      [listpt (carpt cdrpt)
        (let ([carMatch (matchpattern carpt (car body))]
          [cdrMatch (matchpattern cdrpt (cdr body))])
        (and carMatch cdrMatch
          (cons (append (car carMatch)(car cdrMatch))
            (append (cdr carMatch)(cdr cdrMatch)))))]
      [sympt (id)
        (and (symbol? body)
          (list (list (cons id body))))]
      [exprpt (id)
        (list (list (cons id body)))]
      [multpt (eachpt)
        (let ([matches (map (lambda(b) (matchpattern eachpt b)) body)])
          (and (andmap (lambda(x) x) matches)
            (list '() matches)))]
      [contpt (sym) (and (eq? sym body) (list '()))]
      [emptpt ()(and (null? body) (list '()))])))


(define assembleResult
  (lambda (result matches end)
    (cases result-pattern result
      [listpt-r (pts)
          (cons (fold-right (lambda (pt end)
                        (assembleResult pt matches end))
            '() pts) end)]
      [multpt-r (i eachrpt)
          (fold-right (lambda (match end)
                        (assembleResult eachrpt match end))
            end (list-ref matches i))]
      [exprpt-r (id)
          (cons (cdr (assoc id (car matches))) end)]
      [contpt-r (sym)
          (cons sym end)])))
    


