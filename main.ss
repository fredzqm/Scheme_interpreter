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

;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+

; parsed expression


; the core expression of scheme. Syntax expansion should convert all code to core scheme.
(define-datatype cexpression cexpression?
  [var-cexp (id symbol?)]
  [lit-cexp
       (datum (lambda (x)
          (ormap (lambda (pred) (pred x))
           (list number? vector? boolean? symbol? string? pair? null?))))]
  [app-cexp (rator cexpression?)
      (rands (list-of cexpression?))]
  [lambda-cexp (vars (implist-of symbol?))
      (body (list-of cexpression?))]
  [if-cexp
      (test cexpression?)
      (then-op cexpression?)
      (else-op cexpression?)]
  [set!-cexp (var symbol?)
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
    (vars (implist-of symbol?))
    (body (list-of cexpression?))
    (env environment?)])   


;-------------------+
;                   |
;   ENVIRONMENTS    |
;                   |
;-------------------+

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
  (lambda (datum bondedVars)
     (let ([curlev-parse (lambda (exp) (parse-exp exp bondedVars))])
        (cond
          [(symbol? datum) (var-cexp datum)]
          [(number? datum) (lit-cexp datum)]
          [(vector? datum) (lit-cexp datum)]
          [(boolean? datum) (lit-cexp datum)]
          [(string? datum) (lit-cexp datum)]
          [(null? datum) (lit-cexp datum)]
          [(pair? datum)
            (let ([rator (car datum)][rands (cdr datum)])
              (if (symbol? rator)
                (if (slist-contain? rator bondedVars)
                  (app-cexp (var-cexp rator) (map curlev-parse rands)) ; occur bounded
                  (apply-env global-syntax-env rator ; occur free
                    (lambda (expanRules) (apply-syntax expanRules datum
                      (lambda(x)(curlev-parse x))
                      (lambda() (apply-env core-syntax-env rator
                        (lambda(coreRules) (apply-core-syntax coreRules datum bondedVars)) ; does proper syntax exapnsion
                        (lambda() (eopl:error 'syntax-expansion "Invalid Sytanx ~s" datum)))))) ; try syntax exapnsion but failed
                    (lambda() (apply-env core-syntax-env rator
                      (lambda(coreRules) (apply-core-syntax coreRules datum bondedVars))
                      (lambda() (app-cexp (var-cexp rator) (map curlev-parse rands)))))))
                (app-cexp (curlev-parse rator) (map curlev-parse rands))))]
          [else (eopl:error 'parse-exp "bad expression: ~s" datum)]))))


(define apply-syntax
  (lambda (syntaxList exp succeed fail)
    (let ([try (ormap (lambda(x) (matchRule (car x) (cdr x) (cdr exp))) syntaxList)])
      (if try 
        (succeed try)
        (fail)))))
      
(define apply-core-syntax
  (lambda (coreSyntax exp bondedVars)
    (let ([sym (car exp)][body (cdr exp)]
          [curlev-parse (lambda (exp) (parse-exp exp bondedVars))])
      (or (ormap (lambda(x) (matchpattern x body)) coreSyntax)
        (eopl:error 'apply-core-syntax "invalid core expression format ~s" exp))
      (case sym
        [(quote)
          (lit-cexp
            (car body))]
        [(lambda)
          (let ([innerbondedVars (cons (car body) bondedVars)])
            (lambda-cexp
              (car body)
              (map (lambda(exp) (parse-exp exp innerbondedVars)) (cdr body))))]
        [(if)
          (if-cexp
            (curlev-parse (car body))
            (curlev-parse (cadr body))
            (curlev-parse (caddr body)))]
        [(set!)
          (set!-cexp
            (car body)
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
    (let displayLoop ([answers (top-level-eval (read))])
      (if (null? answers)
        (rep)
        (begin
          (eopl:pretty-print (car answers))
          (displayLoop (cdr answers)))))))

; the separate interpreter entry
(define eval-one-exp
  (lambda (x) 
    (apply values (top-level-eval x))))

(define eval-many-exps
  (lambda (ls)
    (for-each top-level-eval ls)))

; top-level-eval evaluates a form in the global environment
(define top-level-eval
  (lambda (form)
    (values-aslist (cond
      [(and (pair? form)(eq? (car form) 'define-syntax)
        (eval-define-syntax (cdr form)))]
      [else (eval-exp (parse-exp form '()) (empty-env))]))))


; those three function define ADT reference, the return value of eval-exp
(define value
  (lambda (x)
    (cond 
      [(null? x) (void)]
      [(null? (cdr x)) (car x)]
      [else (eopl:error 'value "Cannot pass a mult-values to a non-multivalues environment")])))

(define values-aslist
  (lambda (x) x))

(define modify!
  (lambda (ref val-r)
    (cond 
      [(null? ref) 
        (list (value val-r))]
      [(null? (cdr ref))
        (set-car! ref (value val-r))
        (refer)]
      [else (eopl:error 'modify! "Cannot pass a mult-values to a non-multivalues environment")])))

(define refer list)

; eval-exp is the main component of the interpreter
; eval-exp should return a list of result.
; this well help the implementation of multiple return value
; It also makes reference easier
(define eval-exp
  (lambda (exp env)
    (cases cexpression exp
      [lit-cexp (datum) (refer datum)]
      [var-cexp (id)
				(apply-env env id ; look up its value.
      	   (lambda (x) x) ; procedure to call if id is in the environment 
           (lambda ()
             (apply-env global-env id
                (lambda (x) x)
                (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
                  "variable not found in environment: ~s" id)))))]
      [if-cexp (test then-op else-op)
        (if (value (eval-exp test env))
          (eval-exp then-op env)
          (eval-exp else-op env))]
      [lambda-cexp (vars body)
        (refer (closure vars body env))]
      [set!-cexp (var val)
        (let ([ref (eval-exp (var-cexp var) env)])
          (modify! ref (eval-exp val env)))]
      [define-cexp (var val)
        ; (let ([ref (eval-exp val env)])
          (define-in-env! env var (eval-exp val env))
          (refer)
          ; (modify! ref val))
        ]
      [app-cexp (rator rands)
        (let ([procref (eval-exp rator env)]
              [argsref (map (lambda(x) (eval-exp x env)) rands)])
          (apply-proc procref argsref))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list
;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.
(define apply-proc
  (lambda (proc-r args)
    (let ([proc-v (value proc-r)])
      (cases proc-val proc-v
        [prim-proc (op) (refer (apply-prim-proc op (map value args)))]
        [special-proc (op)
          (case op
            [(apply)
              (if (null? (cdr args))
                (eopl:error 'apply "with no argument ~s" proc-v))
              (apply-proc (car args)
                (let loop ([nextarg (cadr args)][leftarg (cddr args)]) ; Caution: No error-checking for 0 args
                  (if (null? leftarg)
                    (if (list? nextarg)
                      (map refer (value nextarg))
                      (eopl:error 'apply "The last argument of apply should be a list of arguments ~s" nextarg))
                    (cons nextarg (loop (car leftarg) (cdr leftarg))))))]
            [(call-with-values)
              (if (not (= 2 (length args)))
                (eopl:error 'call-with-values "call-with-values takes two parameter producer and consumer: ~s" args))
              (let ([ret (apply-proc (car args) '())])
                (apply-proc (cadr args) (map refer (values-aslist ret))))]
            [(values)
              (apply refer (map value args))])]
  	 ;  [closure (len vars body env)
     ;      (if (if lastVar
     ;            (> len (length args))
     ;            (not (= len (length args))))
     ;        (eopl:error 'apply-proc "incorrect number of argument: closure ~a ~a" proc-v args))
     ;      (let lambdaEval ([code body][env (extend-env vars args env)])
     ;        (if (null? (cdr code))
     ;          (eval-exp (car code) env)
     ;          (begin (eval-exp (car code) env)
     ;            (lambdaEval (cdr code) env))))]
        [closure (vars body env)
          (let lambdaEval ([code body]
            [env 
              (if (list? vars)
                (if (= (length vars)(length args))
                  (extend-env vars (map (lambda(x)(refer (value x))) args)  env)
                  (eopl:error 'apply-proc "incorrect number of argument: closure ~a ~a" proc-v args))
                (extend-env (implst->list vars)
                  (map (lambda(x) (refer (value x)))
                    (let loop ([vars vars][args args]) ; match all parts of args to vars
                      (if (pair? vars)
                        (if (pair? args)
                          (cons (car args) (loop (cdr vars)(cdr args)))
                          (eopl:error 'apply-proc "not enough arguments: closure ~a ~a" proc-v args))
                        (list (refer (map value args))))))
                  env))])
            (if (null? (cdr code))
              (eval-exp (car code) env)
              (begin (eval-exp (car code) env)
                (lambdaEval (cdr code) env))))]
        ; You will add other cases
        [else (eopl:error 'apply-proc "Attempt to apply bad procedure: ~s" proc-v)]))))


(define *spec-proc-names* '(apply values call-with-values))
(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = < > <= >= cons list null? assq eq?
                            eqv? equal? atom? car cdr length list->vector list? pair? append list-tail procedure?
                            vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr!
                            vector-set! display newline void quotient))

(define (reset-global-env)
  (set! global-env
    (extend-env
       (append *spec-proc-names* *prim-proc-names*)
       (append 
          (map (lambda(x) (refer (special-proc x))) *spec-proc-names*)
          (map (lambda(x) (refer (prim-proc x))) *prim-proc-names*))
       (empty-env))))

(reset-global-env)

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.
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
      [(list) (apply list args)]
      [(null?) (apply null? args)]
      [(assq) (apply assq args)]
      [(eq?) (apply eq? args)]
      [(eqv?) (apply eqv? args)]
      [(equal?) (apply equal? args)]
      [(atom?) (apply atom? args)]
      [(car) (apply car args)]
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


; Other Utility Methods


(define implst->list
  (letrec ([loop (lambda (vars)
    (if (pair? vars)
        (cons (car vars) (loop (cdr vars)))
        (list vars)))])
  loop))



(define list-set-at-index!
  (lambda (ls ind val)
    (if (= 0 ind) (set-car! ls val)
      (list-set-at-index! (cdr ls) (- ind 1) val))))




(load "syntaxExpansion.ss")
(load "procdedures.ss")