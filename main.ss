; Fred Zhang and Zhou zhou

;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process
(define map
  (lambda (proc ls)
    (if (null? ls)
        '()
        (cons (proc (car ls)) (map proc (cdr ls))))))

(define (implist-of pred?)
  (lambda(implst)
    (let helper ([ls implst])
      (or (null? ls) (pred? ls)
        (and (pred? (car ls)) (helper (cdr ls)))))))

(load "chez-init.ss") 
(load "syntax.ss")

;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+

; parsed expression

(define-datatype syntaxType syntaxType?
  [patternSyntax 
    (syntaxList (list-of (lambda(x) 
      (and (syntax-pattern? (car x))(result-pattern? (cdr x))))))]
  [coreSyntax
    (sym symbol?)
    (validSyntax (list-of syntax-pattern?))])

; the core expression of scheme. Syntax expansion should convert all code to core scheme.
(define-datatype cexpression cexpression?
  [var-cexp (id symbol?)]
  [lit-cexp
       (datum (lambda (x)
          (ormap (lambda (pred) (pred x))
           (list number? vector? boolean? symbol? string? pair? null? void?))))]
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
  [closure (vars (implist-of symbol?))
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
            ; (cases environment encl_env ; Value is NOT found
            ;   (extended-env-record (lsyms lvals lenv)
            ;     (change-env! encl_env sym val))
            ;   (empty-env-record () ; Doesn't gurantee in global environment
            ;     (if (eq? env global-env)
            ;         (add-to-env! env sym val)
            ;         (change-env! encl_env sym val))))

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
  (lambda (datum env)
     (let* ([curlev-parse (lambda (exp) (parse-exp exp env))])
        (cond
          [(symbol? datum) (var-cexp datum)]
          [(number? datum) (lit-cexp datum)]
          [(vector? datum) (lit-cexp datum)]
          [(boolean? datum) (lit-cexp datum)]
          [(string? datum) (lit-cexp datum)]
          [(null? datum) (lit-cexp datum)]
          [(pair? datum)
            (if (symbol? (car datum))
              (let ([ratorSym (car datum)])
                (apply-env env (car datum)
                  (lambda (x) (app-cexp (var-cexp ratorSym) (map curlev-parse (cdr datum)))) ; occur bounded
                  (lambda () (apply-env global-syntax-env (car datum) ; occur free
                              (lambda(x) (apply-syntax x datum env)) ; does proper syntax exapnsion
                              (lambda() (app-cexp (var-cexp ratorSym) (map (lambda (d) (parse-exp d env)) (cdr datum))))))))
              (app-cexp (parse-exp (car datum) env) (map (lambda (d) (parse-exp d env)) (cdr datum))))]
          [else (eopl:error 'parse-exp "bad expression: ~s" datum)]))))

; Zero error-checking for now
(define apply-syntax
  (lambda (syntax exp env)
    (let ([body (cdr exp)][curlev-parse (lambda (exp) (parse-exp exp env))])
      (cases syntaxType syntax
        [patternSyntax (syntaxList)
          (curlev-parse
            (or (ormap (lambda(x) (matchRule (car x) (cdr x) body)) syntaxList)
              (eopl:error 'syntax-expansion "Invalid Sytanx ~s" exp)))]
        [coreSyntax (sym validSyntax)
          (or (ormap (lambda(x) (matchpattern x body)) validSyntax)
            (eopl:error 'parse-exp "Invalid Core Syntax ~s" exp))
          (case sym
            [(quote) 
              (lit-cexp
                (car body))]
            [(lambda)
              (lambda-cexp 
                (car body)
                (map curlev-parse (cdr body)))]
            [(if)
              (if-cexp
                (curlev-parse (car body))
                (curlev-parse (cadr body))
                (if (null? (cddr body))
                    (lit-cexp (void))
                    (curlev-parse (caddr body))))]
            [(set!)
              (set!-cexp 
                (car body)
                (curlev-parse (cadr body)))]
            [(define)
              (define-cexp
                (car body)
                (curlev-parse (cadr body)))]
            [else (eopl:error 'apply-syntax "not implemented core expression ~s" exp)])]))))


;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+
(define *prim-syntax-names* '(afeojavoeaija))



; To be added with define-syntax
(define global-syntax-env 
  (extend-env
    '()
    '()
    (extend-env
      '(quote lambda if set! define)
      (list 
        (coreSyntax 'quote (list
          (listpt (exprpt 'e) (emptpt))))
        (coreSyntax 'lambda (list
          (listpt (multpt (sympt 'v) (sympt 'l))
            (multpt (exprpt 'e) (emptpt)))
          (listpt (multpt (sympt 'v) (emptpt))
            (multpt (exprpt 'e) (emptpt)))))
        (coreSyntax 'if (list
          (listpt (exprpt 'p)
            (listpt (exprpt 't)
              (listpt (exprpt 'e) (emptpt))))
          (listpt (exprpt 'p)
            (listpt (exprpt 't) (emptpt)))))
        (coreSyntax 'set! (list 
          (listpt (sympt 'v)
            (listpt (exprpt 'e) (emptpt)))))
        (coreSyntax 'define (list 
          (listpt (sympt 'v)
            (listpt (exprpt 'e) (emptpt))))))
      (empty-env))))



;-------------------+
;                   |
;   INTERPRETER     |
;                   |
;-------------------+

; the shell entry
(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (read))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

; the separate interpreter entry
(define eval-one-exp
  (lambda (x) (top-level-eval x)))


; top-level-eval evaluates a form in the global environment
(define top-level-eval
  (lambda (form)
    (cond
      [(and (pair? form)(eq? (car form) 'define-syntax)
        (eval-define-syntax (cdr form)))]
      [else (eval-exp (parse-exp form (empty-env)) (empty-env))])))


; eval-exp is the main component of the interpreter
(define eval-exp
  (lambda (exp env)
    (cases cexpression exp
      [lit-cexp (datum) datum]
      [var-cexp (id)
				(apply-env env id ; look up its value.
      	   (lambda (x) x) ; procedure to call if id is in the environment 
           (lambda ()
             (apply-env global-env id
                (lambda (x) x)
                (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
                  "variable not found in environment: ~s" id)))))]
      [if-cexp (test then-op else-op)
        (if (eval-exp test env)
          (eval-exp then-op env)
          (eval-exp else-op env))]
      [lambda-cexp (vars body)
        (closure vars body env)]
      [set!-cexp (var val)
        (let ([val (eval-exp val env)])
          (change-env! env var val))]
      [define-cexp (var val)
        (let ([val (eval-exp val env)])
          (define-in-env! env var val))]
      [app-cexp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (map (lambda(x) (eval-exp x env)) rands)])
          (apply-proc proc-value args))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
			[closure (vars body env)
        (let lambdaEval ([code body]
          [env 
            (if (list? vars)
              (if (= (length vars)(length args))
                (extend-env vars args env)
                (eopl:error 'apply-proc "incorrect number of argument: closure ~a ~a" proc-value args))
              (extend-env (implst->list vars)
                (let loop ([vars vars][args args]) ; match all parts of args to vars
                  (if (pair? vars)
                    (if (pair? args)
                      (cons (car args) (loop (cdr vars)(cdr args)))
                      (eopl:error 'apply-proc "not enough arguments: closure ~a ~a" proc-value args))
                    (list args)))
              env))])
          (if (null? (cdr code))
            (eval-exp (car code) env)
            (begin (eval-exp (car code) env)
              (lambdaEval (cdr code) env))))]
      ; You will add other cases
      [else (eopl:error 'apply-proc "Attempt to apply bad procedure: ~s" proc-value)])))


(define *prim-proc-names* '(apply map + - * / add1 sub1 zero? not = < > <= >= cons list null? assq eq?
                            eqv? equal? atom? car caar caaar caadr cadar cdaar caddr cdadr cddar cdddr
                            cadr cdar cddr cdr length list->vector list? pair? procedure? vector->list
                            vector make-vector vector-ref vector? number? symbol? set-car! set-cdr!
                            vector-set! display newline void quotient member))

(define global-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))


; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.
(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(apply) (apply-proc (car args)
                  (let loop ([arg-ls (cdr args)]) ; Caution: No error-checking for 0 args
                    (if (null? (cdr arg-ls))
                      (car arg-ls)
                      (cons (car arg-ls) (loop (cdr arg-ls))))))]
      [(map) (let ([proc (car args)]
                    [arg-ls (cadr args)])
                  (if (null? arg-ls) ; Caution: No error-checking for 0 args
                      '()
                      (cons (apply-proc proc (list (car arg-ls))) (apply-prim-proc 'map (list proc (cdr arg-ls))))))]
      ; (if (null? (cdr args))
      ;             '()
      ;             (cons (apply-proc (car args) (cadr args)) (apply-prim-proc prim-proc (cons (car args) (cddr args)))))]
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
(define void?
  (lambda (obj)
    (eq? (void) obj)))

(define not-pred
  (lambda (pred?)
    (lambda (arg)
      (not (pred? arg)))))

(define equal-to-n
  (lambda (n)
    (lambda (arg)
      (= n arg))))

(define or-pred
  (lambda (preda predb)
    (lambda (obj)
      (or (preda obj) (predb obj)))))

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



(define create-symbol-counter
  (lambda ()
    (let ([count 0])
      (lambda ()
        (let ([current count])
          (set! count (+ 1 count))
          (string->symbol (number->string current)))))))


(load "syntaxExpansion.ss")
