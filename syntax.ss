(define-datatype syntax-pattern syntax-pattern?
  [listpt (carpt syntax-pattern?) (cdrpt syntax-pattern?)]
  [singpt (id symbol?)]
  [multpt (eachpt syntax-pattern?)]
  [contpt (symbol? sym)]
  [emptpt])

(define-datatype syntaxType syntaxType?
  [patternSyntax (syntaxList (list-of (lambda(x)(and (syntax-pattern? (car x))(syntax-pattern? (cdr x))))))]
  [primitiveSyntax (sym symbol?)])

; Zero error-checking for now
(define apply-syntax
  (lambda (syntax body env)
    (let ([curlev-parse (lambda (exp) (parse-exp exp env))])
      (cases syntaxType syntax
        [patternSyntax (syntaxList)
            (or (ormap (lambda(x) (matchRule (car x) (cdr x) body)) syntax)
              (eopl:error 'apply-syntax "Attempt to apply bad syntax: ~s" syntax))]
        [primitiveSyntax (sym)
          (case sym
            [(quote) (apply lit-cexp body)]
            [(lambda)
              (lambda-cexp (car body) (map curlev-parse (cdr body)))
              ]
            [(if)
              (if-cexp
                (curlev-parse (car body))
                (curlev-parse (cadr body))
                (if (null? (cddr body))
                    (app-cexp (var-cexp 'void) '())
                    (curlev-parse (caddr body))))]
            [(let)
              (if (symbol? (car body))
                ; (let loop ([a v1] [b v2]) body) -> (let ([loop (lambda (a b) body)]) (loop a b))
                ; Named Let
                ; Warning; Letrec not implemented
                (letrec ([name (car body)]
                      [vars (map car (cadr body))]
                      [vals (map cadr (cadr body))]
                      [bodies (cddr body)])
                  (list 'let
                    (list (list name (cons* 'lambda vars bodies)))
                    (cons name vals)))
                ; Reguler Let
                (app-cexp (lambda-cexp (map car (car body)) (map curlev-parse (cdr body)))
                  (map (lambda (p) (parse-exp (cadr p) env)) (car body))))]
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
              (list (cons* 'lambda '() body))]
            [(and)
              (cond
                [(null? body) #t]
                [(null? (cdr body)) (car body)]
                [else (list 'if (car body)
                        (cons 'and (cdr body))
                        #f)])]
            [(or)
              (cond
                [(null? body) #f]
                [(null? (cdr body)) (car body)]
                [else (list 'let
                          (list (list 'val (car body)))
                          (list 'if 'val 'val (cons 'or (cdr body))))])]
            [(cond)
              (if (null? (cdr body))
                (if (eqv? 'else (caar body))
                  (cadar body)
                  (app-cexp (var-cexp 'void) '()))
                (list 'if (caar body) (cadar body) (cons 'cond (cdr body))))]
            [(case)
              (let ([var (car body)]
                    [tests (cdr body)])
                (list 'let (list (list 'var var))
                  (cons 'cond
                    (map (lambda (p)
                            (if (eqv? 'else (car p))
                              p
                              (list (cons 'or (map (lambda (t) (list 'eqv? var t)) (car p))) (cadr p)))) tests))))]
            [(while)
              ; (while t e1 e2 ...) -> (let temp ([test t]) e1 e2 ... (temp t))
              (let ([t (car body)]
                    [bodies (append (cdr body) (list (list 'temp t)))])
                (cons* 'let 'temp (list (list 'test t)) bodies))])]))))

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