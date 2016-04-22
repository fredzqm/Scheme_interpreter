(define-datatype syntax-pattern syntax-pattern?
  [listpt (carpt syntax-pattern?) (cdrpt syntax-pattern?)]
  [singpt (id symbol?)]
  [multpt (eachpt syntax-pattern?)]
  [contpt (symbol? sym)]
  [emptpt])

(define-datatype syntaxType syntaxType?
  [patternSyntax (syntaxList (list-of (lambda(x)(and (syntax-pattern? (car x))(syntax-pattern? (cdr x))))))]
  [primitiveSyntax (sym symbol?)])


(define apply-syntax
  (lambda (syntax body env)
    (let ([curlev-parse (lambda (exp) (parse-exp exp env))])
      (cases syntaxType syntax
        [patternSyntax (syntaxList)
            (or (ormap (lambda(x) (matchRule (car x) (cdr x) body)) syntax)
              (eopl:error 'apply-syntax "Attempt to apply bad syntax: ~s" syntax))]
        [primitiveSyntax (sym)
          (case sym
            [(lambda)
              (lambda-cexp (car body) (map curlev-parse (cdr body)))
              ]
            [(if)
              (if-cexp
                (curlev-parse (car body))
                (curlev-parse (cadr body))
                (if (null? (cddr body))
                    (app-cexp (var-cexp 'void) '())
                    (curlev-parse (cadr body))))]
            [(let) 
              (app-cexp (lambda-cexp (map car (car body)) (map curlev-parse (cdr body)))
                (map (lambda (p) (parse-exp (cadr p) env)) (car body)))]
            [(let*)
              (if (or (null? (cdar body)) (null? (car body)))
                  (cons 'let body)
                  (list 'let (list (caar body))
                        (cons* 'let* (cdar body) (cdr body))))]
              ; (let ([vars-ls (map (lambda (p) (cons (car p) (parse-exp (cadr p) env))) (car body))])
              ;   (if (null? vars-ls) ; This is will create an extra let with null vars-ls
              ;     (let-exp 'let vars-ls (map curlev-parse (cdr body)))
              ;     (let-exp 'let (list (car vars-ls))
              ;       (list (let-exp 'let* (cdr vars-ls) (map curlev-parse (cdr body)))))))]
            [(letrec) (eopl:error 'eval-exp "Not implemented")]
            [(letrec*) (eopl:error 'eval-exp "Not implemented")]
            [(begin)
              (app-exp (lambda-exp '() body) '())]
            [(and)
              (cond
                [(null? body) (lit-exp #t)]
                [(null? (cdr body)) (car body)]
                [else (if-exp #t (car body)
                        (and-exp (cdr body))
                        (lit-exp #f))])]
            [(or)
              (cond
                [(null? body) (lit-exp #f)]
                [(null? (cdr body)) (car body)]
                [else (let-exp 'let
                  (list (cons 'val (car body)))
                  (list (if-exp #t (var-exp 'val) (var-exp 'val) 
                  (or-exp (cdr body)))))])])]))))

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