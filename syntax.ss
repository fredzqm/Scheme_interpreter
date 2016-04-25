(define-datatype syntax-pattern syntax-pattern?
  [listpt (carpt syntax-pattern?) (cdrpt syntax-pattern?)]
  [multpt (eachpt syntax-pattern?) (endpt syntax-pattern?)]
  [sympt (id symbol?)]
  [exprpt (id symbol?)]
  [contpt (sym symbol?)]
  [wildpt]
  [emptpt]
  )

(define-datatype result-pattern result-pattern?
  [listpt-r (pts (implist-of result-pattern?))]
  [multpt-r (i number?) (eachrpt result-pattern?)]
  [exprpt-r (id symbol?)]
  [contpt-r (sym (lambda(x) #t))]
  )


(define slist-contain
  (lambda (slist x)
    (if (pair? slist)
      (or (slist-contain (car slist) x)
        (slist-contain (cdr slist) x))
      (eq? x slist))))

;-------------------+
;                   |
;  defined-syntax   |
;                   |
;-------------------+

(define parse-syntax-result-pair
  (lambda (syntax result constantls body)
    (let* ([parsed-syntax (parse-syntax-pattern syntax constantls)]
          [occurs (occurs-syntax-pattern parsed-syntax)]
          [parsed-result (parse-result-pattern result occurs)]
          [indexed-result (findMultIndex parsed-result occurs)])
      (matchRule parsed-syntax indexed-result body))))


(define parse-syntax-pattern
  (lambda (pat constantls)
    (let ([symls '()])
      (let parseLoop ([pat pat])
        (cond
          [(null? pat)
            (emptpt)]
          [(symbol? pat)
            (cond
             [(eq? pat '...) (eopl:error 'parse-syntax-pattern "improper use of ...")] ; to signal ...
             [(eq? pat '_) (wildpt)]
             [(member pat constantls) (contpt pat)]
             [else 
                (if (member pat symls)
                  (eopl:error 'parse-syntax-pattern "repeated pattern name: ~s" pat)
                  (set! symls (cons pat symls)))
                (exprpt pat)])]
          [(pair? pat)
            (if (and (pair? (cdr pat))(eq? '... (cadr pat)))
              (multpt (parseLoop (car pat)) 
                (parseLoop (cddr pat)))
              (listpt (parseLoop (car pat)) 
                (parseLoop (cdr pat))))]
          [else (eopl:error 'parseLoop "Invalid sytax pattern ~s" pat)])))))

(define occurs-syntax-pattern
  (lambda (pattern)
    (cases syntax-pattern pattern
      [listpt (carpt cdrpt)
        (let ([carOccur (occurs-syntax-pattern carpt)]
              [cdrOccur (occurs-syntax-pattern cdrpt)])
          (cons (append (car carOccur)(car cdrOccur))
            (append (cdr carOccur)(cdr cdrOccur))))]
      [multpt (eachpt endpt)
        (let ([eachOccur (occurs-syntax-pattern eachpt)]
              [endOcuur (occurs-syntax-pattern endpt)])
            (cons (car endOcuur)
              (cons eachOccur (cdr endOcuur))))]
      [sympt (id)
        (list (list id))]
      [exprpt (id)
        (list (list id))]
      [contpt (sym) (list '())]
      [wildpt () (list '())]
      [emptpt () (list '())])))


(define parse-result-pattern
  (lambda (pat occurs)
    (let ([const? (lambda(x) 
                    (not (slist-contain occurs x)))])
      (let parseLoop ([pat pat])
        (cond
          [(symbol? pat)
            (case pat
             [(...) (eopl:error 'parse-result-pattern "improper use of ...")] ; to signal ...
             [(_) (eopl:error 'parse-result-pattern "improper use of _")]
             [else (if (const? pat)
                      (contpt-r pat)
                      (exprpt-r pat))])]
          [(null? pat)
            (listpt-r '())]
          [(pair? pat)
            (let ([parsed
              (let loop ([pat pat])
                (cond 
                  [(null? pat) '(0)]
                  [(not (pair? pat)) (cons 0 (parseLoop pat))] ; improper list
                  [(eq? (car pat) '...)
                    (let ([rest (loop (cdr pat))])
                      (cons (+ 1 (car rest)) (cdr rest)))]
                  [else (let ([pat (parseLoop (car pat))]
                              [rest (loop (cdr pat))])
                    (let loop2 ([pat pat]
                                [multOrder (car rest)]
                                [end (cdr rest)])
                      (if (= 0 multOrder) 
                        (cons* 0 pat end)
                        (loop2 (multpt-r 0 pat) (- multOrder 1) end))))]))])
              (if (not (= 0 (car parsed)))
                (eopl:error 'parse-result-pattern "... is not matched to symbol ~s" pat))
              (listpt-r (cdr parsed)))]
          [else (eopl:error 'parse-result-pattern "Invalid pattern ~s" pat)])))))

(define findMultIndex
  (lambda (result occurs)
    (cases result-pattern result
      [listpt-r (pts)
        (let ([try (map (lambda(x) (findMultIndex x occurs)) pts)])
          (and (andmap (lambda(x)x) try) (listpt-r try)))]
      [multpt-r (i eachrpt)
        (let ([try (let loop ([i 1][envs (cdr occurs)])
              (if (null? envs) #f ; no match found
                (let ([match (findMultIndex eachrpt (car envs))])
                  (if match  i
                    (loop (+ i 1) (cdr envs))))))])
          (and try (multpt-r try eachrpt)))]
      [exprpt-r (id)
          (and (member id (car occurs)) result)]
      [contpt-r (sym)
          result])))

  ; [listpt (carpt syntax-pattern?) (cdrpt syntax-pattern?)]
  ; [multpt (eachpt syntax-pattern?) (endpt syntax-pattern?)]
  ; [sympt (id symbol?)]
  ; [exprpt (id symbol?)]
  ; [contpt (sym symbol?)]
  ; [wildpt]
  ; [emptpt]

; return #f is never going to match
; return #t is sometime going to match
; return an alist of matches if always going to match
; (define resultMatchPattern
;   (lambda (result pattern)
;       (cases syntax-pattern pattern
;         [listpt (carpt cdrpt)
;           (case result-pattern result
;             [listpt-r (pts)
;               ]
;             [multpt-r (i eachrpt)
;               ]
;             [exprpt-r (id)
;               ]
;             [contpt-r (sym)
;               ]
;             [else #f])
;           ]
;       ; [listpt (carpt cdrpt)
;       ;   (and (pair? body)
;       ;     (let ([carMatch (matchpattern carpt (car body))]
;       ;           [cdrMatch (matchpattern cdrpt (cdr body))])
;       ;       (and carMatch cdrMatch
;       ;         (cons (append (car carMatch)(car cdrMatch))
;       ;           (append (cdr carMatch)(cdr cdrMatch))))))]
;         [multpt (eachpt endpt)
;           (case result-pattern result
;             [listpt-r (pts)
;               ]
;             [multpt-r (i eachrpt)
;               ]
;             [exprpt-r (id)
;               ]
;             [contpt-r (sym)
;               ]
;             [else #f])
;           ]
;       ; [multpt (eachpt endpt)
;       ;   (let loop ([body body][matchls '()])
;       ;     (or 
;       ;       (and (pair? body)
;       ;         (let ([eachMatch (matchpattern eachpt (car body))])
;       ;           (and eachMatch
;       ;             (loop (cdr body) (append matchls (list eachMatch))))))
;       ;       (let ([endMatch (matchpattern endpt body)])
;       ;         (and endMatch
;       ;           (cons (car endMatch)
;       ;             (cons matchls (cdr endMatch)))))))]
;         [sympt (id)
;           (case result-pattern result
;             [listpt-r (pts)
;               (and (result-pattern? pts) ; awere of improper list
;                 (resultMatchPattern pts pattern) #t)]
;             [multpt-r (i eachrpt)
;               (and (resultMatchPattern eachrpt pattern) #t)]
;             [exprpt-r (id)
;               #t]
;             [contpt-r (sym)
;               #t]
;             [else #f])
;           ]
;       ; [sympt (id)
;       ;   (and (symbol? body)
;       ;     (list (list (cons id body))))]
;         [exprpt (id)
;           (list (cons id result))]
;       ; [exprpt (id)
;       ;   (list (list (cons id body)))]
;         [contpt (sym)
;           (case result-pattern result
;             [listpt-r (pts)
;               (and (result-pattern? pts) ; awere of improper list
;                 (resultMatchPattern pts pattern) #t)]
;             [multpt-r (i eachrpt)
;               (and (resultMatchPattern eachrpt pattern) #t)]
;             [exprpt-r (id)
;               #t]
;             [contpt-r (sym-r)
;               (and (eq? sym sym-r) (list '()))]
;             [else #f])]
;       ; [contpt (sym) (and (eq? sym body) (list '()))]
;         [wildpt ()
;           (list '())]
;       ; [wildpt () (list '())]
;         [emptpt ()
;           (case result-pattern result
;             [listpt-r (pts)
;               (and (null? pts) (list '()))]
;             [multpt-r (i eachrpt)
;               #t]
;             [else #f])]
;       ; [emptpt ()(and (null? body) (list '()))])))
;         )
;     ))









;-------------------+
;                   |
; expansion Runtime |
;                   |
;-------------------+


(define matchRule
  (lambda (pattern result body)
    (let ([matches (matchpattern pattern body)])
      (and matches (car (assembleResult result matches '()))))))

(define matchpattern
  (lambda (pattern body)
    (cases syntax-pattern pattern
      [listpt (carpt cdrpt)
        (and (pair? body)
          (let ([carMatch (matchpattern carpt (car body))]
                [cdrMatch (matchpattern cdrpt (cdr body))])
            (and carMatch cdrMatch
              (cons (append (car carMatch)(car cdrMatch))
                (append (cdr carMatch)(cdr cdrMatch))))))]
      [multpt (eachpt endpt)
        (let loop ([body body][matchls '()])
          (or 
            (and (pair? body)
              (let ([eachMatch (matchpattern eachpt (car body))])
                (and eachMatch
                  (loop (cdr body) (append matchls (list eachMatch))))))
            (let ([endMatch (matchpattern endpt body)])
              (and endMatch
                (cons (car endMatch)
                  (cons matchls (cdr endMatch)))))))]
      [sympt (id)
        (and (symbol? body)
          (list (list (cons id body))))]
      [exprpt (id)
        (list (list (cons id body)))]
      [contpt (sym) (and (eq? sym body) (list '()))]
      [wildpt () (list '())]
      [emptpt ()(and (null? body) (list '()))])))


(define assembleResult
  (lambda (result matches end)
    (cases result-pattern result
      [listpt-r (pts)
          (cons 
            (let loop ([pts pts])
              (cond
                [(null? pts)  '()]
                [(result-pattern? pts)
                  (car (assembleResult pts matches '()))]
                [else (assembleResult (car pts) matches (loop (cdr pts)))]))
            end)]
      [multpt-r (i eachrpt)
          (fold-right (lambda (match end)
                        (assembleResult eachrpt match end))
            end (list-ref matches i))]
      [exprpt-r (id)
          (cons (cdr (assoc id (car matches))) end)]
      [contpt-r (sym)
          (cons sym end)])))
    
