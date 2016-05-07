; Fred Zhang and Zhou zhou

;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process

(define apcont
  (lambda (k . x)
    (apply k x)))

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

; environment type definitions
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of (lambda(x) (or (not x)(symbol? x)))))
   (vals list?)
   (env environment?)))

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.
(define-datatype proc-val proc-val?
  [prim-proc
    (name symbol?)]
  [special-proc
    (name symbol?)]
  [closure 
    (variableLength boolean?)
    (ref-map (implist-of boolean?))
    (body (list-of cexpression?))
    (env list?)])   


;-------------------+
;                   |
;   ENVIRONMENTS    |
;                   |
;-------------------+
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
(define (add-sym-templete sym env k)
  (if (not (symbol? sym))
    (eopl:error 'add-sym-templete "sym should be a symbol ~s" sym))
  (apcont k 
    (if (or (member sym (caar env)) (member sym (cdar env)))
      env
      (cons
        (cons (caar env) (cons sym (cdar env)))
        (cdr env)))))

; return a var-cexp represented in lexical order.
; variable representation
; free var: '(name 0)
; bounded var: '(name depth . index)
; search free var: '(name d1 d2 d3 ...)
; search bounded var: '(name d1 d2 d3 ... depth . index)
(define (search-in-templete env sym)
  (let helper ([env env]
      [k (lambda (num ls)
        (if num
          (cons* sym num ls)
          (cons sym ls)))])
    (if (null? env)
        (apcont k #f '())
        (index-in-ls sym (caar env)
          (lambda(lexiIndex)
            (if lexiIndex
              (apcont k 0 lexiIndex)
              (helper (cdr env)
                (lambda (num ls)
                  (if num
                    (index-in-ls sym (cdar env)
                      (lambda (posible)
                        (if posible
                          (apcont k 0 (cons num ls))
                          (apcont k (+ num 1) ls))))
                    (index-in-ls sym (cdar env)
                      (lambda (posible)
                        (if posible
                          (apcont k 0 '())
                          (apcont k #f '())))))))))))))

(define (index-in-ls sym ls k)
  (if (null? ls)
    (apcont k #f)
    (if (eq? sym (car ls))
      (apcont k 0)
      (index-in-ls sym (cdr ls)
        (lambda (x)
          (apcont k (and x (+ 1 x))))))))

;-------------------+
;                   |
; LOCAL ENVIRONMENT |
;                   |
;-------------------+

(define (apply-local-env env info succeed fail)
  (let ([sym (car info)])
    (if (null? (cdr info))
      (apply-global-env sym succeed fail)
      (let helper ([carls (cadr info)][cdrls (cddr info)][env env])
        (if (= carls 0)
          (if (integer? cdrls)
            (apcont succeed (vector-ref (caar env) cdrls))
            (apply-env (cdar env) sym
              succeed
              (lambda ()
                (if (null? cdrls)
                  (apply-global-env sym succeed fail)
                  (helper (car cdrls)(cdr cdrls) (cdr env))))))
          (helper (- carls 1) cdrls (cdr env)))))))


(define (apply-global-env sym succeed fail)
  (apply-env global-env sym succeed fail))

(define (extend-local-env vary? ref-map args env succeed fail)
  (let helper ([ref-map ref-map][args args]
              [k (lambda (curLevel)
                    (apcont succeed 
                      (cons 
                        (cons 
                          (list->vector curLevel)
                          (empty-env))
                        env)))])
    ; (if (null? args)
    ;   (if (null? ref-map)
    ;     (if vary?
    ;       (apcont k (list (refer '())))
    ;       (apcont k '()))
    ;     (apcont fail))
    ;   (if (null? ref-map)
    ;     (if vary?
    ;       (apcont k (list (refer (map de-refer args))))
    ;       (apcont fail))
    ;     (helper (cdr ref-map) (cdr args)
    ;         (lambda (cdrVal)
    ;           (apcont k
    ;             (cons 
    ;               (if (car ref-map) 
    ;                 (car args) 
    ;                 (refer (de-refer (car args))))
    ;               cdrVal))))))
    
    (if (null? ref-map)
      (if vary?
        (apcont k (list (refer (map de-refer args))))
        (if (null? args)
          (apcont k '())
          (apcont fail)))
      (if (null? args)
        (apcont fail)
        (helper (cdr ref-map) (cdr args)
            (lambda (cdrVal)
              (apcont k
                (cons 
                  (if (car ref-map) 
                    (car args) 
                    (refer (de-refer (car args))))
                  cdrVal))))))
    ))

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3
(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (if (not (= (length syms)(length vals)))
      (eopl:error 'extend-env "syms and vals has different length syms ~s vals" syms vals))
    (if (null? syms)
      (extended-env-record (list #f) (list #f) env)
      (extended-env-record syms vals env))))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
       (if (number? list-index-r)
     (+ 1 list-index-r)
     #f))))))

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      (empty-env-record ()
        (fail))
      (extended-env-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
            (succeed (list-ref vals pos))
            (apply-env env sym succeed fail)))))))

(define add-to-env!
  (letrec ([helper! (lambda (ls val)
                      (if (null? (cdr ls))
                        (if (car ls) ; if the list is initially empty, which has a #f in it
                          (set-cdr! ls (list val))
                          (set-car! ls val))
                        (helper! (cdr ls) val)))])
    (lambda (env sym val)
      (cases environment env
        (empty-env-record () 
          (eopl:error 'add-to-env! "Cannot add to the end of an empty environment"))
        (extended-env-record (syms vals env)
          (helper! syms sym)
          (helper! vals val))))))

(define change-env!
  (lambda (env sym val)
    (cases environment env
      (empty-env-record () (change-env! global-env sym val)) ; At top level
      (extended-env-record (syms vals encl_env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
            (list-set-at-index! vals pos val) ; Value is found
            (if (eq? env global-env)
              (add-to-env! env sym val)
              (change-env! encl_env sym val))))))))

(define define-in-env!
  (lambda (env sym val)
    (cases environment env
      (empty-env-record () (define-in-env! global-env sym val)) ; At top level
      (extended-env-record (syms vals encl_env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
            (list-set-at-index! vals pos val) ; Value is found
            (add-to-env! env sym val) ; We don't care if it is in global; if it's not found in the current scope, it just needs to be added.
            ))))))


(define list-set-at-index!
  (lambda (ls ind val)
    (if (= 0 ind) (set-car! ls val)
      (list-set-at-index! (cdr ls) (- ind 1) val))))

(define implst->list
  (letrec ([loop (lambda (vars)
    (if (pair? vars)
        (cons (car vars) (loop (cdr vars)))
        (list vars)))])
  loop))
;-------------------+
;                   |
;    PARSER         |
;                   |
;-------------------+


; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp
  (lambda (datum templete)
     (let ([curlev-parse (lambda (exp) (parse-exp exp templete))])
        (cond
          [(symbol? datum) (var-cexp (search-in-templete templete datum))]
          [(number? datum) (lit-cexp datum)]
          [(vector? datum) (lit-cexp datum)]
          [(boolean? datum) (lit-cexp datum)]
          [(string? datum) (lit-cexp datum)]
          [(null? datum) (lit-cexp datum)]
          [(pair? datum)
            (let ([rator (car datum)][rands (cdr datum)])
              (if (symbol? rator)
                (if (slist-contain? rator templete)
                  (app-cexp (curlev-parse rator) (map curlev-parse rands)) ; occur bound
                  (apply-env global-syntax-env rator ; occur free
                    (lambda (expanRules) (apply-syntax expanRules datum
                      (lambda(x)(curlev-parse x))
                      (lambda() (apply-env core-syntax-env rator
                        (lambda(coreRules) (apply-core-syntax coreRules datum templete)) ; does proper syntax exapnsion
                        (lambda() (eopl:error 'syntax-expansion "Invalid sytanx ~s" datum)))))) ; try syntax exapnsion but failed
                    (lambda() (apply-env core-syntax-env rator
                      (lambda(coreRules) (apply-core-syntax coreRules datum templete))
                      (lambda() (app-cexp (curlev-parse rator) (map curlev-parse rands)))))))
                (app-cexp (curlev-parse rator) (map curlev-parse rands))))]
          [else (eopl:error 'parse-exp "bad expression: ~s" datum)]))))


(define apply-syntax
  (lambda (syntaxList exp succeed fail)
    (let ([try (ormap (lambda(x) (matchRule (car x) (cdr x) (cdr exp))) syntaxList)])
      (if try 
        (succeed try)
        (fail)))))
      
(define apply-core-syntax
  (lambda (coreSyntax exp templete)
    (let ([sym (car exp)][body (cdr exp)]
          [curlev-parse (lambda (exp) (parse-exp exp templete))])
      (or (ormap (lambda(x) (matchpattern x body)) coreSyntax)
        (eopl:error 'apply-core-syntax "Invalid core expression format ~s" exp))
      (case sym
        [(quote)
          (lit-cexp
            (car body))]
        ; [(lambda)
        ;   (let* ([vars-list (car body)]
        ;         [ref? (lambda (obj)
        ;                 (cond
        ;                   [(symbol? obj) #f]
        ;                   [(and (list? obj) (eqv? 'ref (car obj)) (symbol? (cadr obj))) #t]
        ;                   [else (eopl:error 'apply-core-syntax "Invalid argument declaration: ~s" vars-list)]))]
        ;         [extr-var (lambda (obj)
        ;                     (if (symbol? obj) obj (cadr obj)))])
        ;     (let ([ref-map (implst-map ref? vars-list)]
        ;           [templete (implst-map extr-var vars-list)])
        ;       (let ([innertemplete (cons (car body) templete)])
        ;         (lambda-cexp
        ;           templete
        ;           ref-map
        ;           (map (lambda(exp) (parse-exp exp innertemplete)) (cdr body))))))]
        [(lambda)
          (let helper ([varls (car body)]
                      [k (lambda (vars ref-map)
                          (lambda-cexp 
                            vars 
                            ref-map
                            (let ([innertemplete (extend-templete vars templete)])
                              (map (lambda(exp) (parse-exp exp innertemplete)) (cdr body)))))])
              (cond
                [(null? varls) (apcont k '() '())]
                [(symbol? varls) (apcont k (list varls) '())]
                [(pair? varls)
                  (helper (cdr varls)
                    (lambda (cdrVars cdrref-map)
                      (cond
                        [(symbol? (car varls))
                          (apcont k
                            (cons (car varls) cdrVars)
                            (cons #f cdrRef-map))]
                        [(and (pair? (car varls)) (eq? 'ref (caar varls)) 
                          (symbol? (cadar varls)) (null? (cddar varls)))
                          (apcont k
                            (cons (cadar varls) cdrVars)
                            (cons #t cdrRef-map))]
                        [else (eopl:error 'lambda-cexp "In correct format of lambda expression ~s" (cons 'lambda body))])
                      ))])
            )]
        [(if)
          (if-cexp
            (curlev-parse (car body))
            (curlev-parse (cadr body))
            (curlev-parse (caddr body)))]
        [(set!)
          (set!-cexp
            (search-in-templete templete (car body))
            (curlev-parse (cadr body)))]
        [(define)
          (define-cexp
            (car body)
            (curlev-parse (cadr body)))]
        [else (eopl:error 'apply-core-syntax "not implemented core expression ~s" exp)]))))


;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+

(define global-syntax-env
  (extend-env '() '() (empty-env)))

; To be added with define-syntax
(define core-syntax-env 
  (extend-env
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
          (listpt (exprpt 'e) (emptpt)))))
    (empty-env)))



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
      [else (eval-exp (parse-exp form (empty-templete)) (empty-env))])))


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

; eval-exp is the main component of the interpreter
; eval-exp should return a list of result.
; this well help the implementation of multiple return value
; It also makes reference easier
(define eval-exp
  (lambda (exp env)
    (cases cexpression exp
      [lit-cexp (datum) (refer datum)]
      [var-cexp (varinfo)
				(apply-local-env env varinfo ; look up its value.
    	   (lambda (x) x) ; procedure to call if id is in the environment 
         (lambda () (eopl:error 'apply-local-env "variable not found in environment: ~s" varinfo)))]
      [if-cexp (test then-op else-op)
        (if (de-refer (eval-exp test env))
          (eval-exp then-op env)
          (eval-exp else-op env))]
      [lambda-cexp (vars ref-map body)
        (refer (closure (not (= (length vars)(length ref-map)))
                        ref-map body env))]
      [set!-cexp (varinfo val)
        (apply-local-env env varinfo
          (lambda(ref) (modify! ref (de-refer (eval-exp val env))))
          (lambda() (define-in-env! env (eval-exp val env))))
        (refer (void))]
      [define-cexp (var val)
        (define-in-env! global-env var (eval-exp val env))
        (refer (void))]
      [app-cexp (rator rands)
        (let ([procref (eval-exp rator env)]
              [argsref (map (lambda(x) (eval-exp x env)) rands)])
          (apply-proc procref argsref))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list
;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.
; arguments:
;   proc-r: reference of a procedure, not de-referred
;   args: list of arguments, the list is not referred, but each arg is referred
(define apply-proc
  (lambda (proc-r args) ; args should not have been de-referred
    (let ([proc-v (de-refer proc-r)])
      (cases proc-val proc-v
        [prim-proc (op) (refer (apply-prim-proc op (map de-refer args)))]
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
                    (cons nextarg (loop (car leftarg) (cdr leftarg))))))]
            [(call-with-values)
              (if (not (= 2 (length args)))
                (eopl:error 'call-with-values "call-with-values takes two parameters: a producer and a consumer: ~s" args))
              (let ([ret (apply-proc (car args) '())])
                  (apply-proc (cadr args) (map refer (de-refer-aslist ret))))]
            [(values) (apply refer (map de-refer args))])]
        [closure (vary? ref-map body env)
          (let lambdaEval ([code body]
            [env (extend-local-env vary? ref-map args env
                    (lambda (x) x)
                    (lambda () (eopl:error 'apply-proc "not enough arguments: closure ~a ~a" proc-v args)))])
              ; (if (list? vars)
              ;   (if (= (length vars)(length args))
              ;     (extend-env vars (map (lambda(x)(refer (de-refer x))) args)  env)
              ;     (eopl:error 'apply-proc "incorrect number of argument: closure ~a ~a" proc-v args))
              ;   (extend-env (implst->list vars)
              ;     (map (lambda(x) (refer (de-refer x)))
              ;       (let loop ([vars vars][args args]) ; match all parts of args to vars
              ;         (if (pair? vars)
              ;           (if (pair? args)
              ;             (cons (car args) (loop (cdr vars) (cdr args)))
              ;             (eopl:error 'apply-proc "not enough arguments: closure ~a ~a" proc-v args))
              ;           (list (refer (map de-refer args))))))
              ;     env))
            (if (null? (cdr code))
              (eval-exp (car code) env)
              (begin (eval-exp (car code) env)
                (lambdaEval (cdr code) env))))]
        ; You will add other cases
        [else (eopl:error 'apply-proc "Attempt to apply bad procedure: ~s" proc-v)]))))


(define *spec-proc-names* '(apply values call-with-values))
(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = < > <= >= cons list null? assq eq?
                            eqv? equal? atom? car caar caaar caadr cadar cdaar caddr cdadr cddar cdddr cadr cdar
                            cddr cdr length list->vector list? pair? append list-tail procedure?
                            vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr!
                            vector-set! display newline void quotient))

(define (reset-global-env)
  (set! global-env
    (extend-env
       (append *spec-proc-names* *prim-proc-names*)
       (append 
          (map (lambda(x) (refer (special-proc x))) *spec-proc-names*)
          (map (lambda(x) (refer (prim-proc x))) *prim-proc-names*))
       (empty-env)))
  (addPredefinedProcedures))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.
; arguments:
;   prim-proc: the primitive procedure, de-referred
;   args: the list of arguments, de-referred
; returns:
;   single value, not referred
(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(add1) (+ (1st args) 1)] ; Error-handling for more than 1 args?
      [(sub1) (- (1st args) 1)]
      [(zero?) (zero? (1st args))]
      [(not) (not (1st args))]
      [(=) (= (1st args) (2nd args))]
      [(<) (apply < args)]
      [(>) (apply > args)]
      [(<=) (apply <= args)]
      [(>=) (apply >= args)]
      [(cons) (cons (1st args) (2nd args))]
      [(list) args]
      [(null?) (apply null? args)]
      [(assq) (apply assq args)]
      [(eq?) (apply eq? args)]
      [(eqv?) (apply eqv? args)]
      [(equal?) (apply equal? args)]
      [(atom?) (apply atom? args)]
      [(car) (apply car args)]
      [(caar) (apply caar args)]
      [(caaar) (apply caaar args)]
      [(caadr) (apply caadr args)]
      [(cadar) (apply cadar args)]
      [(cdaar) (apply cdaar args)]
      [(caddr) (apply caddr args)]
      [(cdadr) (apply cdadr args)]
      [(cddar) (apply cddar args)]
      [(cdddr) (apply cdddr args)]
      [(cadr) (apply cadr args)]
      [(cdar) (apply cdar args)]
      [(cddr) (apply cddr args)]
      [(cdr) (apply cdr args)]
      [(length) (apply length args)]
      [(list->vector) (apply list->vector args)]
      [(list?) (apply list? args)]
      [(pair?) (apply pair? args)]
      [(append) (apply append args)]
      [(list-tail) (apply list-tail args)]
      [(procedure?) (apply proc-val? args)]
      [(vector->list) (apply vector->list args)]
      [(vector) (apply vector args)]
      [(make-vector) (apply make-vector args)]
      [(vector-ref) (apply vector-ref args)]
      [(vector?) (apply vector? args)]
      [(number?) (apply number? args)]
      [(symbol?) (apply symbol? args)]
      [(set-car!) (apply set-car! args)]
      [(set-cdr!) (apply set-cdr! args)]
      [(vector-set!) (apply vector-set! args)]
      [(display) (apply display args)]
      [(newline) (apply newline args)]
      [(void) (apply void args)]
      [(quotient) (apply quotient args)]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))


; ; Other Utility Methods





(define implst-map ; No error checking for now
  (lambda (f ls . more)
    (letrec ([map-one-implist
            (lambda (ls)
              (cond
                [(and (not (pair? ls)) (not (null? ls))) (f ls)]
                [(null? ls) '()]
                [else (cons (f (car ls)) (map-one-implist (cdr ls)))]))])
      (if (null? more)
        (map-one-implist ls)
        (apply map map-one-implist ls more)))))



(load "procdedures.ss")
(load "syntaxExpansion.ss")
(addSyntaxExpansion)
(reset-global-env)
