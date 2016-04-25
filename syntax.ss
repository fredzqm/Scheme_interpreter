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
  [listpt-r (pts (list-of result-pattern?))]
  [multpt-r (i number?) (eachrpt result-pattern?)]
  [endpt-r (pt result-pattern?)]
  [exprpt-r (id symbol?)]
  [contpt-r (sym (lambda(x) #t))]
  )




;-------------------+
;                   |
;  defined-syntax   |
;                   |
;-------------------+

(define parse-syntax-pattern
  (lambda (pat)
    (cond
      [(null? pat)
        (emptpt)]
      [(symbol? pat)
        (case pat
         [(...) (eopl:error 'parse-syntax-pattern "improper use of ...")] ; to signal ...
         [(_) (wildpt)]
         [else (exprpt pat)])]
      [(pair? pat)
        (if (and (pair? (cdr pat))(eq? '... (cadr pat)))
          (multpt (parse-syntax-pattern (car pat)) 
            (parse-syntax-pattern (cddr pat)))
          (listpt (parse-syntax-pattern (car pat)) 
            (parse-syntax-pattern (cdr pat))))]
      [else (eopl:error 'parse-syntax-pattern "Invalid sytax pattern ~s" pat)])))


(define parse-result-pattern
  (lambda (pat)
    (cond
      [(symbol? pat)
        (case pat
         [(...) (eopl:error 'parse-result-pattern "improper use of ...")] ; to signal ...
         [(_) (eopl:error 'parse-result-pattern "improper use of _")]
         [else (exprpt-r pat)])]
      [(null? pat)
        (listpt-r '())]
      [(pair? pat)
        (let ([parsed
          (let loop ([pat pat])
            (cond 
              [(null? pat) '(0)]
              [(not (pair? pat)) (list 0 (endpt-r (parse-result-pattern pat)))] ; improper list
              [(eq? (car pat) '...)
                (let ([rest (loop (cdr pat))])
                  (cons (+ 1 (car rest)) (cdr rest)))]
              [else (let ([pat (parse-result-pattern (car pat))]
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
      [else (eopl:error 'parse-result-pattern "Invalid pattern ~s" pat)])))


  ; [listpt (carpt syntax-pattern?) (cdrpt syntax-pattern?)]
  ; [multpt (eachpt syntax-pattern?) (endpt syntax-pattern?)]
  ; [sympt (id symbol?)]
  ; [exprpt (id symbol?)]
  ; [contpt (sym symbol?)]
  ; [wildpt]
  ; [emptpt]

(case result-pattern result
  [listpt-r (pts)
    ]
  [multpt-r (i) (eachrpt)
    ]
  [endpt-r (pt)
    ]
  [exprpt-r (id)
    ]
  [contpt-r (sym)
    ])

; return #f is never going to match
; return #t is sometime going to match
; return an alist of matches if always going to match
(define resultMatchPattern
  (lambda (result pattern)
    (let )
      (cases syntax-pattern pattern
        [listpt (carpt cdrpt)
          (case result-pattern result
            [listpt-r (pts)
              ]
            [multpt-r (i) (eachrpt)
              ]
            [endpt-r (pt)
              ]
            [exprpt-r (id)
              ]
            [contpt-r (sym)
              ])
          ]
      ; [listpt (carpt cdrpt)
      ;   (and (pair? body)
      ;     (let ([carMatch (matchpattern carpt (car body))]
      ;           [cdrMatch (matchpattern cdrpt (cdr body))])
      ;       (and carMatch cdrMatch
      ;         (cons (append (car carMatch)(car cdrMatch))
      ;           (append (cdr carMatch)(cdr cdrMatch))))))]
        [multpt (eachpt endpt)
          (case result-pattern result
            [listpt-r (pts)
              ]
            [multpt-r (i) (eachrpt)
              ]
            [endpt-r (pt)
              ]
            [exprpt-r (id)
              ]
            [contpt-r (sym)
              ])
          ]
      ; [multpt (eachpt endpt)
      ;   (let loop ([body body][matchls '()])
      ;     (or 
      ;       (and (pair? body)
      ;         (let ([eachMatch (matchpattern eachpt (car body))])
      ;           (and eachMatch
      ;             (loop (cdr body) (append matchls (list eachMatch))))))
      ;       (let ([endMatch (matchpattern endpt body)])
      ;         (and endMatch
      ;           (cons (car endMatch)
      ;             (cons matchls (cdr endMatch)))))))]
        [sympt (id)
          (case result-pattern result
            [listpt-r (pts)
              ]
            [multpt-r (i) (eachrpt)
              ]
            [endpt-r (pt)
              ]
            [exprpt-r (id)
              ]
            [contpt-r (sym)
              ])
          ]
      ; [sympt (id)
      ;   (and (symbol? body)
      ;     (list (list (cons id body))))]
        [exprpt (id)
          (case result-pattern result
            [listpt-r (pts)
              ]
            [multpt-r (i) (eachrpt)
              ]
            [endpt-r (pt)
              ]
            [exprpt-r (id)
              ]
            [contpt-r (sym)
              ])
          ]
      ; [exprpt (id)
      ;   (list (list (cons id body)))]
        [contpt (sym)
          (case result-pattern result
            [listpt-r (pts)
              ]
            [multpt-r (i) (eachrpt)
              ]
            [endpt-r (pt)
              ]
            [exprpt-r (id)
              ]
            [contpt-r (sym)
              ])
          ]
      ; [contpt (sym) (and (eq? sym body) (list '()))]
        [wildpt ()
          (case result-pattern result
            [listpt-r (pts)
              ]
            [multpt-r (i) (eachrpt)
              ]
            [endpt-r (pt)
              ]
            [exprpt-r (id)
              ]
            [contpt-r (sym)
              ])
          ]
      ; [wildpt () (list '())]
        [emptpt ()
          (case result-pattern result
            [listpt-r (pts)
              ]
            [multpt-r (i) (eachrpt)
              ]
            [endpt-r (pt)
              ]
            [exprpt-r (id)
              ]
            [contpt-r (sym)
              ])
          ]
      ; [emptpt ()(and (null? body) (list '()))])))
        )
    ))









;-------------------+
;                   |
; expansion Runtime |
;                   |
;-------------------+


(define matchRule
  (lambda (pattern result body)
    (let ([matches (matchpattern pattern body)])
      (and matches (assembleResult (endpt-r result) matches '())))))

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
          (cons (fold-right (lambda (pt end)
                        (assembleResult pt matches end))
            '() pts) end)]
      [multpt-r (i eachrpt)
          (fold-right (lambda (match end)
                        (assembleResult eachrpt match end))
            end (list-ref matches i))]
      [endpt-r (pt)
          (car (assembleResult pt matches '()))]
      [exprpt-r (id)
          (cons (cdr (assoc id (car matches))) end)]
      [contpt-r (sym)
          (cons sym end)])))
    
