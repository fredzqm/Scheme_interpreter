; Fred Zhang and Zhou zhou

;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process



(define (implist-of pred?)
  (lambda(implst)
    (let helper ([ls implst])
      (or (null? ls) (pred? ls)
        (and (pred? (car ls)) (helper (cdr ls)))))))

(define slist-contain?
  (lambda (slist x)
    (if (pair? slist)
      (or (slist-contain? (car slist) x)
        (slist-contain? (cdr slist) x))
      (eq? x slist))))

(load "chez-init.ss")
(load "syntax.ss")
(load "procedure_init.ss")
(load "syntax_expansion_init.ss")

;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+

; parsed expression
; the core expression of scheme. Syntax expansion should convert all code to core scheme.
(define-datatype cexpression cexpression?
  [var-cexp (varinfo pair?)]
  [lit-cexp
       (datum (lambda (x)
          (ormap (lambda (pred) (pred x))
           (list number? vector? boolean? symbol? string? pair? null?))))]
  [app-cexp (rator cexpression?)
      (rands (list-of cexpression?))]
  [lambda-cexp
      (vars (list-of symbol?))
      (ref-map (implist-of boolean?))
      (body (list-of cexpression?))]
  [if-cexp
      (test cexpression?)
      (then-op cexpression?)
      (else-op cexpression?)]
  [set!-cexp (varinfo pair?)
      (val cexpression?)]
  [define-cexp (var symbol?)
      (val cexpression?)])


; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.
(define-datatype proc-val proc-val?
  [prim-proc
    (name symbol?)
    (proc procedure?)]
  [special-proc
    (name symbol?)]
  [closure 
    (variableLength boolean?)
    (ref-map (implist-of boolean?))
    (body (list-of cexpression?))
    (env list?)]
  [cont-proc
    (k continuation?)])   


;-------------------+
;                   |
; ENVIRON TEMPLETE  |
;                   |
;-------------------+

(define (empty-templete)
  '())

; create another level of environment
(define (extend-templete syms env)
  (if (not ((list-of symbol?) syms))
    (eopl:error 'extend-templete "syms should be a list of symbols ~s" syms))
  (cons (list syms) env))

; add a posible symbol to this level
(define (add-sym-templete env sym)
  (if (or (null? env) (member sym (caar env)) (member sym (cdar env)))
    env
    (cons
      (cons
        (caar env)
        (cons sym (cdar env)))
      (cdr env))))

(define (merge-templets a b)
  (if (null? a) a
    (cons 
      (cons
        (caar a)
        (let loop ([x (cdar a)][y (cdar b)])
          (if (null? x) y
            (if (member (car x) y)
              (loop (cdr x) y)
              (cons (car x) (loop (cdr x) y))))))
      (cdr a))))

; return a var-cexp represented in lexical order.
; variable representation
; free var: '(name)
; bounded var: '(name depth . index)
; uncertain free var: '(name d1 d2 d3 ...)
; uncertain bounded var: '(name d1 d2 d3 ... depth . index)
(define (search-in-templete env sym bounded free)
  (let helper ([env env]
              [k (lambda (num ls)
                (if num
                  (bounded (cons* sym num ls))
                  (free (cons sym ls))))])
    (if (null? env)
        (k #f '())
        (index-in-ls sym (caar env)
          (lambda(lexiIndex)
            (if lexiIndex
              (k 0 lexiIndex)
              (helper (cdr env)
                (lambda (num ls)
                  (if num
                    (index-in-ls sym (cdar env)
                      (lambda (posible)
                        (if posible
                          (k 0 (cons num ls))
                          (k (+ num 1) ls))))
                    (index-in-ls sym (cdar env)
                      (lambda (posible)
                        (if posible
                          (k 0 '())
                          (k #f '())))))))))))))

; a helper method for templete
(define (index-in-ls sym ls k)
  (if (null? ls)
    (k #f)
    (if (eq? sym (car ls))
      (k 0)
      (index-in-ls sym (cdr ls)
        (lambda (x)
          (k (and x (+ 1 x))))))))


;-------------------+
;                   |
; LOCAL ENVIRONMENT |
;                   |
;-------------------+

(define (empty-local-env)
  '())

(define (apply-local-env env info succeed fail)
  (let ([sym (car info)])
    (if (null? (cdr info))
      (search-table global-env sym succeed fail)
      (let helper ([carls (cadr info)][cdrls (cddr info)][env env])
        (if (= carls 0)
          (if (integer? cdrls)
            (succeed (vector-ref (caar env) cdrls))
            (search-table (cdar env) sym
              succeed
              (lambda ()
                (if (null? cdrls)
                  (search-table global-env sym succeed fail)
                  (helper (car cdrls)(cdr cdrls) (cdr env))))))
          (helper (- carls 1) cdrls (cdr env)))))))


(define (extend-local-env vary? ref-map args env succeed fail)
  (let helper ([ref-map ref-map][args args]
              [k (lambda (curLevel)
                    (succeed 
                      (cons 
                        (cons 
                          (list->vector curLevel)
                          (empty-table))
                        env)))])
    (if (null? ref-map)
      (if vary?
        (k (list (refer (map de-refer args))))
        (if (null? args)
          (k '())
          (fail)))
      (if (null? args)
        (fail)
        (helper (cdr ref-map) (cdr args)
            (lambda (cdrVal)
              (k
                (cons 
                  (if (car ref-map) 
                    (car args) 
                    (refer (de-refer (car args))))
                  cdrVal))))))))


(define (define-in-local-env! env sym val)
  (if (null? env)
    (update-table! global-env sym val)
    (update-table! (cdar env) sym val)))

;-------------------+
;                   |
; GLOBAL ENVIRONMENT|
;                   |
;-------------------+

(define (empty-table)
  (make-hashtable 
    (lambda(s)(string-length (symbol->string s)))
    symbol=?))

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3
(define create-table
  (lambda (syms vals)
    (if (not (= (length syms)(length vals)))
      (eopl:error 'create-table "syms and vals has different length syms ~s vals" syms vals))
    (let ([env (empty-table)])
      (let loop ([syms syms][vals vals])
        (if (null? syms)
          env
          (begin
            (update-table! env (car syms) (car vals))
            (loop (cdr syms)(cdr vals))))))))

(define search-table
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (if (hashtable-contains? env sym)
      (succeed (hashtable-ref env sym #f))
      (fail))))


(define update-table!
  (lambda (env sym val)
    (hashtable-set! env sym val)))



;-------------------+
;                   |
;    PARSER         |
;                   |
;-------------------+


; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.
; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp
  (lambda (datum templete k)
    (let ([curlev-parse (lambda (exp) (parse-exp exp templete (lambda (temp result) result)))])
      (if (pair? datum)
        (let ([rator (car datum)][rands (cdr datum)])
          (if (symbol? rator) (search-in-templete templete rator
              (lambda (bounded)
                (k templete (app-cexp (var-cexp bounded) (map curlev-parse rands)))) ; occur bounded
              (lambda (free) (search-table global-syntax-env rator ; occur free
                (lambda (expanRules) (apply-syntax expanRules datum
                  (lambda(x) (parse-exp x templete k))
                  (lambda() (search-table core-syntax-env rator
                    (lambda(coreRules) (apply-core-syntax coreRules datum templete k)) ; does proper syntax exapnsion
                    (lambda() (eopl:error 'syntax-expansion "Invalid sytanx ~s" datum)))))) ; try syntax exapnsion but failed
                (lambda() (search-table core-syntax-env rator
                  (lambda(coreRules) (apply-core-syntax coreRules datum templete k))
                  (lambda() (k templete (app-cexp (var-cexp free) (map curlev-parse rands)))))))))
            (k templete (app-cexp (curlev-parse rator) (map curlev-parse rands)))))
        (k templete (cond
          [(symbol? datum) (search-in-templete templete datum
                              (lambda (bounded) (var-cexp bounded))
                              (lambda (free) (var-cexp free)))]
          [(number? datum) (lit-cexp datum)]
          [(vector? datum) (lit-cexp datum)]
          [(boolean? datum) (lit-cexp datum)]
          [(string? datum) (lit-cexp datum)]
          [(null? datum) (lit-cexp datum)]
          [else (eopl:error 'parse-exp "bad expression: ~s" datum)]))))))


(define apply-syntax
  (lambda (syntaxList exp succeed fail)
    (let ([try (ormap (lambda(x) (matchRule (car x) (cdr x) (cdr exp))) syntaxList)])
      (if try 
        (succeed try)
        (fail)))))
      

(define apply-core-syntax
  (lambda (coreSyntax exp templete k)
    (let ([sym (car exp)][body (cdr exp)])
      (or (ormap (lambda(x) (matchpattern x body)) coreSyntax)
        (eopl:error 'apply-core-syntax "Invalid core expression format ~s" exp))
      (case sym
        [(quote)
          (k templete (lit-cexp (car body)))]
        [(lambda)
          (let helper ([varls (car body)]
                      [k (lambda (vars ref-map)
                          (k 
                            templete
                            (lambda-cexp 
                              vars 
                              ref-map
                              (let loop ([code (cdr body)][templete (extend-templete vars templete)])
                                (if (null? code) '()
                                  (parse-exp (car code) templete
                                    (lambda (temp result)
                                      (cons result (loop (cdr code) temp)))))))))])
            (cond
              [(null? varls) (k '() '())]
              [(symbol? varls) (k (list varls) '())]
              [(pair? varls)
                (helper (cdr varls)
                  (lambda (cdrVars cdrRef-map)
                    (cond
                      [(symbol? (car varls))
                        (k
                          (cons (car varls) cdrVars)
                          (cons #f cdrRef-map))]
                      [(and (pair? (car varls)) (eq? 'ref (caar varls)) 
                        (symbol? (cadar varls)) (null? (cddar varls)))
                        (k
                          (cons (cadar varls) cdrVars)
                          (cons #t cdrRef-map))]
                      [else (eopl:error 'lambda-cexp "In correct format of lambda expression ~s" (cons 'lambda body))])))]))]
        [(if)
          (parse-exp (car body) templete
            (lambda (temp-test result-test)
              (parse-exp (cadr body) temp-test
                (lambda (temp-then result-then)
                  (parse-exp (caddr body) temp-test
                    (lambda (temp-else result-else)
                      (k
                        (merge-templets temp-then temp-else)
                        (if-cexp result-test result-then result-else))))))))]
        [(set!)
          (k 
            templete 
            (set!-cexp
              (search-in-templete templete (car body)
                (lambda (x) x) (lambda (x) x))
              (parse-exp (cadr body) templete
                (lambda (temp result) result))))]
        [(define)
          (k
            (add-sym-templete templete (car body))
            (define-cexp
              (car body)
              (parse-exp (cadr body) templete
                (lambda (temp result) result))))]
        [else (eopl:error 'apply-core-syntax "not implemented core expression ~s" exp)]))))


;-----------------------+
;                       |
;       REFERENCE       |
;                       |
;-----------------------+

(define reference?
  (lambda (x)
    (or (box? x) (list? x))))

; these three functions define ADT reference, the return value of eval-exp
(define (refer . a) 
  (cond
    [(null? a) '()]
    [(null? (cdr a)) (box (car a))]
    [else a]))

(define (de-refer ref) 
  (cond
    [(box? ref) (unbox ref)]
    [(null? ref) (void)]
    [(and (list? ref) (< 1 (length ref)))
      (eopl:error 'de-refer "Return multiple values ~s to single value environment" ref)]
    [else (eopl:error 'de-refer "Try to de-reference Invalid reference ~s" ref)]))

(define (de-refer-aslist ref)
  (cond
    [(null? ref) '()]
    [(box? ref) (list (unbox ref))]
    [(and (list? ref) (< 1 (length ref)))
      ref]
    [else (eopl:error 'de-refer-aslist "Try to de-reference Invalid reference ~s" ref)]))

(define (modify! ref val) 
  (if (not (box? ref))
    (eopl:error 'modify! "Can only modify a reference with one value: ~s," ref)
    (set-box! ref val)))


;-------------------+
;                   |
;   INTERPRETER     |
;                   |
;-------------------+

; the shell entry
(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    (let displayLoop ([answers (de-refer-aslist (top-level-eval (read)))])
      (if (null? answers)
        (rep)
        (begin
          (eopl:pretty-print (car answers))
          (displayLoop (cdr answers)))))))

; (define (toString v)
;   (if (proc-val? v)
;     (cases proc-val v
;       [prim-proc (name)
;         (list 'prim-proc name)]
;       [special-proc (name)
;         (list 'prim-proc name)]
;       [closure (variableLength vars ref-map body env)
;         (list 'lambda vars (unparse-exp body))])))

; the separate interpreter entry
(define eval-one-exp
  (lambda (x) 
    (apply values (de-refer-aslist (top-level-eval x)))))

(define eval-many-exps
  (lambda (ls)
    (for-each top-level-eval ls)))

; top-level-eval evaluates a form in the global environment
(define top-level-eval
  (lambda (form)
    (cond
      [(and (pair? form) (eq? (car form) 'define-syntax)
        (eval-define-syntax (cdr form)))]
      [else 
        (eval-exp 
          (parse-exp form 
            (empty-templete) (lambda (temp result) result)) 
          (empty-local-env)
          (final-k))])))


;-----------------------+
;                       |
;      EVALUATION       |
;                       |
;-----------------------+

(define-datatype continuation continuation?
  [if-k
    (then-op cexpression?)
    (else-op cexpression?)
    (env list?)
    (next-k continuation?)]
  [eval-rands-k
    (next-k continuation?)]
  [eval-rands-car-k
    (cdr-rands (list-of cexpression?))
    (env list?)
    (next-k continuation?)]
  [eval-rands-cdr-k
    (car-ref reference?)
    (env list?)
    (next-k continuation?)]
  [set!-k
    (varinfo pair?)
    (env list?)
    (next-k continuation?)]
  [define-k
    (var symbol?)
    (env list?)
    (next-k continuation?)]
  [eval-body-k
    (cdr-code (list-of cexpression?))
    (env list?)
    (next-k continuation?)]
  [call-with-values-k
    (consumer reference?)
    (next-k continuation?)]
  [final-k])



(define apcont (lambda (k x)
    (if (procedure? k)
      (begin
        (display x) (newline)
        (display k) (newline)
        (k x))
      (cases continuation k
        [if-k (then-op else-op env next-k)
          (if (de-refer x)
            (eval-exp then-op env next-k)
            (eval-exp else-op env next-k))]
        [eval-rands-k (next-k)
          (apply-proc (car x) (cdr x) next-k)]
        [eval-rands-car-k (cdr-rands env next-k)
          (eval-rands cdr-rands env
            (eval-rands-cdr-k x env next-k))]
        [eval-rands-cdr-k (car-ref env next-k)
          (apcont next-k (cons car-ref x))]
        [set!-k (varinfo env next-k)
          (apply-local-env env varinfo
            (lambda (ref) (modify! ref (de-refer x)))
            (lambda () (update-table! global-env (car varinfo) x)))
          (apcont next-k (refer))]
        [define-k (var env next-k)
          (define-in-local-env! env var x)
          (apcont next-k (refer))]
        [eval-body-k (cdr-code env next-k)
          (eval-body cdr-code env next-k)]
        [call-with-values-k (consumer next-k)
          (apply-proc consumer (map refer (de-refer-aslist x)) next-k)]
        [final-k () x]
        [else (eopl:error 'apcont "Undefined continuation: ~s" k)]))))

; eval-exp is the main component of the interpreter
; eval-exp should return a list of result.
; this well help the implementation of multiple return value
; It also makes reference easier
(define (eval-exp exp env k)
    (cases cexpression exp
      [lit-cexp (datum) (apcont k (refer datum))]
      [var-cexp (varinfo)
				(apply-local-env env varinfo ; look up its value.
    	   (lambda (x) (apcont k x)) ; procedure to call if id is in the environment 
         (lambda () (eopl:error 'apply-local-env "variable not found in environment: ~s" varinfo)))]
      [if-cexp (test then-op else-op)
        (eval-exp test env
          (if-k then-op else-op env k))]
      [lambda-cexp (vars ref-map body)
        (apcont k (refer 
          (closure (not (= (length vars)(length ref-map)))
            ref-map body env)))]
      [set!-cexp (varinfo val)
        (eval-exp val env
          (set!-k varinfo env k))]
      [define-cexp (var val)
        (eval-exp val env
          (define-k var env k))]
      [app-cexp (rator rands)
        (eval-rands (cons rator rands) env
          (eval-rands-k k))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))


(define (eval-rands rands env k)
    (if (null? rands)
      (apcont k '())
      (eval-exp (car rands) env
        (eval-rands-car-k (cdr rands) env k))))

; evaluate the list of operands, putting results into a list
;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.
; arguments:
;   proc-r: reference of a procedure, not de-referred
;   args: list of arguments, the list is not referred, but each arg is referred
(define (apply-proc proc-r args k) ; args should not have been de-referred
    (let ([proc-v (de-refer proc-r)])
      (cases proc-val proc-v
        [prim-proc (op proc) (apcont k (refer (apply proc (map de-refer args))))]
        [special-proc (op)
          (case op
            [(apply)
              (if (null? (cdr args))
                (eopl:error 'apply "with no argument ~s" proc-v))
              (apply-proc (car args)
                (let loop ([nextarg (cadr args)] [leftarg (cddr args)]) ; Caution: No error-checking for 0 args
                  (if (null? leftarg)
                    (if (list? (de-refer nextarg))
                      (map refer (de-refer nextarg))
                      (eopl:error 'apply "The last argument of apply should be a proper list of arguments ~s" nextarg))
                    (cons nextarg (loop (car leftarg) (cdr leftarg)))))
                k)]
            [(call-with-values)
              (if (not (= 2 (length args)))
                (eopl:error 'call-with-values "call-with-values takes two parameters: a producer and a consumer: ~s" args))
              (apply-proc (car args) '()
                (call-with-values-k (cadr args) k))]
            [(values) 
              (apcont k (apply refer (map de-refer args)))]
            [(call/cc)
              (if (not (null? (cdr args)))
                (eopl:error 'call/cc "call/cc only take one argument: ~s" args))
              (apply-proc (car args) (list (refer (cont-proc k))) k)])]
        [closure (vary? ref-map body env)
          (extend-local-env vary? ref-map args env
            (lambda (env) (eval-body body env k))
            (lambda () (eopl:error 'apply-proc "not enough arguments: closure ~a ~a" proc-v args)))]
        [cont-proc (k)
          (apcont k (apply refer (map de-refer args)))]
        [else (eopl:error 'apply-proc "Attempt to apply bad procedure: ~s" proc-v)])))

(define (eval-body code env k)
    (eval-exp (car code) env
      (if (null? (cdr code)) k
        (eval-body-k (cdr code) env k))))

;-----------------------+
;                       |
;  GLOBAL ENVIRONMENT   |
;                       |
;-----------------------+

(define global-syntax-env
  (empty-table))

; To be added with define-syntax
(define core-syntax-env 
  (create-table
    '(quote lambda if set! define)
    (list 
      (list ; quote
        (listpt (exprpt 'e) (emptpt)))
      (list ; lambda
        (listpt (multpt (exprpt 'v) (sympt 'l))
          (multpt (exprpt 'e) (emptpt)))
        (listpt (multpt (exprpt 'v) (emptpt))
          (multpt (exprpt 'e) (emptpt))))
      (list ; if 
        (listpt (exprpt 'p)
          (listpt (exprpt 't)
            (listpt (exprpt 'e) (emptpt)))))
      (list ; set!
        (listpt (sympt 'v)
          (listpt (exprpt 'e) (emptpt))))
      (list ; define
        (listpt (sympt 'v)
          (listpt (exprpt 'e) (emptpt)))))))



(define *spec-proc-names* '(apply values call-with-values call/cc))
(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = < > <= >= cons list null? assq eq?
                            eqv? equal? atom? car caar caaar caadr cadar cdaar caddr cdadr cddar cdddr cadr cdar
                            cddr cdr length list->vector list? pair? append list-tail
                            vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr!
                            vector-set! display newline void quotient))


(define (reset-global-env)
  (set! global-env
    (create-table
       (append  '(procedure?) *spec-proc-names* *prim-proc-names*)
       (append 
          (list (refer (prim-proc 'procedure? proc-val?)))
          (map (lambda(x) (refer (special-proc x))) *spec-proc-names*)
          (map (lambda(x) (refer (prim-proc x (eval x)))) *prim-proc-names*))))
  (addPredefinedProcedures))






;-----------------------+
;                       |
;       INTIALIZE       |
;                       |
;-----------------------+


(addSyntaxExpansion)
(reset-global-env)


; to easy typing eval-one-exp
(define-syntax i
  (syntax-rules ()
    [(_ x)
      (eval-one-exp (quote x))]
    [(_ x1 x2 ...)
      (eval-many-exps 
        (list (quote x1) (quote x2) ...))]))

; to ease tracing
(define t
  (lambda ()
    (trace eval-exp eval-body apply-proc eval-rands)))