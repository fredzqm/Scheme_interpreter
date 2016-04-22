(define-datatype syntax-pattern syntax-pattern?
  [listpt (carpt syntax-pattern?) (cdrpt syntax-pattern?)]
  [singpt (id symbol?)]
  [multpt (eachpt syntax-pattern?)]
  [contpt (symbol? sym)]
  [emptpt])

(define-datatype syntaxType syntaxType?
  [patternSyntax (syntaxList (list-of (lambda(x)(and (syntax-pattern? (car x))(syntax-pattern? (cdr x))))))]
  [coreSyntax (sym symbol?)]
  [primitiveSyntax (sym symbol?)]
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
          (append carMatch cdrMatch)))]
      [singpt (id)
        (and (symbol? body)
          (list (cons id body)))]
      [multpt (eachpt)
        (if (null? body)
          '()
          (and (pair? body)
            (let ([carMatch (matchpattern eachpt (car body))]
              [cdrMatch (matchpattern pattern (cdr body))])
              (if (null? cdrMatch)
                (map (lambda (x) (cons (car x)(list (cdr x)))) carMatch)
                (map (lambda (x y) (cons (car x)(cons (cdr x)(cdr y)))) carMatch cdrMatch)))))]
      [contpt (sym) (and (eq? sym body) '())]
      [emptpt ()(and (null? body) '())])))

(define assembleResult
  (lambda (result matches count)
    (cases syntax-pattern result
      [listpt (carpt cdrpt)
        (cons (assembleResult carpt matches) (assembleResult cdrpt matches))]
      [singpt (id)
        (reference matches id count)]
      [multpt (eachpt)
        (let loop ([index (length (reference matches id count))])
          (if (= index 0)
            '()
            (cons (assembleResult eachpt matches (append count (list index)))
              (loop (- index 1)))))]
      [contpt (sym) sym]
      [emptpt () '()])))

(define consisteLen
  (lambda (eachpt matches count)
    (cases syntax-pattern result
      [listpt (carpt cdrpt)
        (let ([carLen (consisteLen carpt matches count)]
              [cdrLen (consisteLen cdrpt matches count)])
          (and carLen cdrLen 
            
            ))]
      [singpt (id)
        (length (reference matches id count))]
      [multpt (eachpt)
        (consisteLen eachpt matches count)]
      [contpt (sym) #t]
      [emptpt () #t])))


(define reference
  (lambda (matches id count)
    (let loop ([matchLine (cdr (assoc matches id))][count count])
      (if (null? count)
        matchLine
        (loop (list-ref matchLine (car count)) (cdr count))))))