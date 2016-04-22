; Fred Zhang and Zhou zhou

;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process

(load "chez-init.ss") 
(load "syntax.ss")

;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+

; parsed expression
(define (implist-of pred?)
  (lambda(implst)
    (let helper ([ls implst])
      (or (null? ls) (pred? ls)
        (and (pred? (car ls)) (helper (cdr ls)))))))


(define-datatype expression expression?
  [var-exp (id symbol?)]
  [lit-exp
       (datum (lambda (x)
          (ormap 
           (lambda (pred) (pred x))
           (list number? vector? boolean? symbol? string? pair? null?))))]
  [app-exp (rator expression?)
      (rands (list-of expression?))]
  [app-sym-exp (ratorSym symbol?)
      (rands (list-of expression?))]
  [lambda-exp (vars (implist-of symbol?))
      (body (list-of expression?))]
  [set!-exp (var symbol?)
      (val expression?)]
  [if-exp
      (two-armed? boolean?)
      (test expression?)
      (then-op expression?)
      (else-op (or-pred null? expression?))]
  [let-exp (lettype (lambda(x)(or (eq? x 'let)(eq? x 'letrec)(eq? x 'let*)(eq? x 'letrec*))))
      (vars-ls (list-of (lambda(y)(and (symbol? (car y))(expression? (cdr y))))))
      (body (list-of expression?))]
  [let-named-exp (name symbol?)
      (vars-ls (list-of (lambda(y)(and (symbol? (car y))(expression? (cdr y))))))
      (body (list-of expression?))]
  [quote-exp 
        (datum (lambda (x)
          (ormap 
           (lambda (pred) (pred x))
           (list number? vector? boolean? symbol? string? pair? null?))))]
  [begin-exp (body (list-of expression?))]
  [and-exp (body (list-of expression?))]
  [or-exp (body (list-of expression?))]
  )

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
  [if-cexp (test cexpression?)
      (then-op cexpression?)
      (else-op cexpression?)]
  [set!-cexp (var symbol?)
      (val cexpression?)])

;; environment type definitions
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
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
;    PARSER         |
;                   |
;-------------------+


; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)


; (define parse-exp
;   (lambda (datum)
;     (cond
;      [(symbol? datum) (var-exp datum)]
;      [(number? datum) (lit-exp datum)]
;      [(vector? datum) (lit-exp datum)]
;      [(boolean? datum) (lit-exp datum)]
;      [(string? datum) (lit-exp datum)]
;      [(null? datum) (lit-exp datum)]
;      [(pair? datum)
;       (cond
;        [(eqv? 'quote (1st datum))
; 	      (if (not (null? (cddr datum)))
; 	        (eopl:error 'parse-exp "quote expression: incorrect length: ~s" datum))
; 	      (quote-exp (2nd datum))]
;        [(eq? (car datum) 'lambda)
;           (if (null? (cddr datum))
;             (eopl:error 'parse-exp "lambda expression: incorrect length: ~s" datum))
;           (if (not ((implist-of symbol?) (cadr datum)))
;             (eopl:error 'parse-exp "lambda declaration list: formers must be symbols ~s" (cadr datum)))
;           (lambda-exp (cadr datum) (map parse-exp (cddr datum)))]
;        [(or (eq? (car datum) 'letrec) (eq? (car datum) 'let) (eq? (car datum) 'let*))
;           (if (null? (cdr datum))
;         (eopl:error 'parse-exp "~s expression: incorrect length: ~s" (car datum) datum))
;           (let ([letarg (cadr datum)] [letop (cddr datum)])
;             (if (not (list? (cadr datum)))
;               (begin (set! letarg (caddr datum))
;                 (set! letop (cdddr datum))))
;           (if (null? letop)
;               (eopl:error 'parse-exp "~s expression: incorrect length: ~s" (car datum) letop))
;             (if (not (list? letarg))
;               (eopl:error 'parse-exp "~s declaration list: not a proper list: ~s" (car datum) letarg))
;             (set! letarg (map (lambda(x) 
;                     (if (not (list? x))
;                       (eopl:error 'parse-exp "~s declaration pairs: not all proper list: ~s" (car datum) letarg))
;                 (if (not (symbol? (car x)))
;                       (eopl:error 'parse-exp "~s declaration pairs: first members must be symbols: ~s" (car datum) letarg))
;                 (if (not (= (length x) 2))
;                       (eopl:error 'parse-exp "~s declaration pairs: not all length 2: ~s" (car datum) letarg))
;                   (cons (car x)(parse-exp (cadr x)))) (cadr datum)))
;             (set! letop (map parse-exp (cddr datum)))
;             (if (list? (cadr datum))
;           (let-exp (car datum) letarg letop)
;               (let-named-exp (cadr datum) letarg letop)))]
;        [(eq? (car datum) 'if)
;           (cond
;             [(= (length datum) 4) (if-exp #t (parse-exp (cadr datum)) (parse-exp (caddr datum)) (parse-exp (cadddr datum)))]
;             [(= (length datum) 3) (if-exp #f (parse-exp (cadr datum)) (parse-exp (caddr datum)) '())]
;             [(= (length datum) 2) (eopl:error 'parse-exp "if expression: should have at least test and then clauses: ~s" datum)]
;             [else (eopl:error 'parse-exp "if expression: should have only test, then, and (optional) else clauses: ~s" datum)])]
;        [(eq? (car datum) 'set!)
;           (if (null? (cddr datum))
;             (eopl:error 'parse-exp "set! expression: missing expression: ~s" datum))
;           (if (> (length datum) 3)
;             (eopl:error 'parse-exp "set! expression: too many parts: ~s" datum))
;           (set!-exp (cadr datum) (parse-exp (caddr datum)))]
;        [(eq? (car datum) 'begin)
;           (begin-exp (map parse-exp (cdr datum)))]
;        [(eq? (car datum) 'and)
;           (and-exp (map parse-exp (cdr datum)))]
;        [(eq? (car datum) 'or)
;           (or-exp (map parse-exp (cdr datum)))]
;        [else
;           (if (not (list? datum))
;             (eopl:error 'parse-exp "Error in parse-exp: application ~s is not a proper list" datum))
;           (if (symbol? (car datum))
;             (app-sym-exp (car datum) (map parse-exp (cdr datum)))
;             (app-exp (parse-exp (car datum)) (map parse-exp (cdr datum))))])]
;      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))
(define parse-exp
  (lambda (datum env)
     (let* ([curlev-parse (lambda (exp) (parse-exp exp env))]
            [parsed 
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
                  (lambda (x) (app-cexp (var-cexp ratorSym) (cdr datum))) ; occur bounded
                  (lambda () (apply-env global-syntax-env (car datum) ; occur free
                              (lambda(x) (apply-syntax x (cdr datum) env)) ; does proper syntax exapnsion
                              (lambda() (app-cexp (var-cexp ratorSym) (map (lambda (d) (parse-exp d env)) (cdr datum))))))))
              (app-cexp (parse-exp (car datum) env) (map (lambda (d) (parse-exp d env)) (cdr datum))))]
          [else (eopl:error 'parse-exp "bad expression: ~s" datum)])])
      (if (cexpression? parsed)
        parsed
        (curlev-parse parsed))
    )))



(define unparse-exp
  (lambda (expr)
    (cases expression expr
      [var-exp (id)
          id]
      [lit-exp (var)
          var]
      [app-exp (rator rands)
          (cons (unparse-exp rator) (map unparse-exp rands))]
      [app-sym-exp (ratorSym rands)
          (cons ratorSym (map unparse-exp rands))]
      [lambda-exp (vars body)
          (cons* 'lambda vars (map unparse-exp body))]
      [if-exp (two-armed? test then-op else-op)
          (if two-armed?
            (list 'if (unparse-exp test) (unparse-exp then-op) (unparse-exp else-op))
            (list 'if (unparse-exp test) (unparse-exp then-op)))]
      [set!-exp (var val)
          (list 'set! var (unparse-exp val))]
      [let-exp (lettype vars-ls body)     
          (cons* lettype (map (lambda(x)(list (car x)(unparse-exp (cdr x)))) vars-ls)
            (map unparse-exp body))]
      [let-named-exp (name vars-ls body)      
          (cons* 'let name (map (lambda(x)(list (car x)(unparse-exp (cdr x)))) vars-ls)
            (map unparse-exp body))]
      [quote-exp (datum)
          (list 'quote datum)]
      [begin-exp (body)
          (cons 'begin (map unparse-exp body))]
      [and-exp (body)
          (cons 'and (map unparse-exp body))]
      [or-exp (body)
          (cons 'or (map unparse-exp body))]
          )))


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
    (extended-env-record syms vals env)))

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





;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+
(define *prim-syntax-names* '(lambda let let* letrec letrec* begin and or))

; To be added with define-syntax
(define global-syntax-env (extend-env 
  *prim-syntax-names*
  (map primitiveSyntax *prim-syntax-names*)
  (empty-env)))

; To be added later
; (define syntax-expand
;   (lambda (exp env)
;     (let* ([curlev-expand (lambda (exp) (syntax-expand exp env))]
;       [expand (cases expression exp
;           [lit-exp (datum) (lit-cexp datum)]
;           [quote-exp (datum) (lit-cexp datum)]
;           [var-exp (id) (var-cexp id)]
;           [if-exp (two-armed? test then-op else-op)
;             (if-cexp (curlev-expand test)
;               (curlev-expand then-op) 
;               (if two-armed? (curlev-expand else-op) (lit-cexp (void))))]
;           [lambda-exp (vars body)
;             (let* ([bonded (if (list? vars) vars (implst->list vars))]
;               [nextlev-expand (lambda (exp) (syntax-expand exp (extend-env bonded bonded env)))])
;                 (lambda-cexp vars (map nextlev-expand body)))]
;           [app-exp (rator rands)
;             (app-cexp (curlev-expand rator) (map curlev-expand rands))]
;           [app-sym-exp (ratorSym rands)
;             (apply-env env ratorSym
;               (lambda (x) (app-cexp (var-cexp ratorSym) (map curlev-expand rands))) ; occur bounded
;               (lambda () (apply-env global-syntax-env ratorSym ; occur free
;                           (lambda(x) (apply-syntax x (map unparse-exp rands))) ; does proper syntax exapnsion
;                           (lambda() (app-cexp (var-cexp ratorSym) (map curlev-expand rands))))))] ; occur free, should bined globally
;           ; cases above are base cases where expression is part of core expresion
          
;           [let-exp (lettype vars-ls body)
;             (app-sym-exp lettype (cons (parse-exp (map (lambda (p) (list (car p) (unparse-exp (cdr p)))) vars-ls)) body))]
;             ; (case lettype
;             ;   [(let) 
;             ;     (app-exp (lambda-exp (map car vars-ls) body)
;             ;       (map cdr vars-ls))]
;             ;   [(let*)  
;             ;     (if (null? vars-ls) ; This is will create an extra let with null vars-ls
;             ;       (let-exp 'let vars-ls body)
;             ;       (let-exp 'let (list (car vars-ls))
;             ;         (list (let-exp 'let* (cdr vars-ls) body))))]
;             ;   [(letrec) (eopl:error 'eval-exp "Not implemented")]
;             ;   [(letrec*) (eopl:error 'eval-exp "Not implemented")])]
;           [begin-exp (body)
;             (app-sym-exp 'begin body)]
;             ; (apply-env env 'begin
;             ;   (lambda(x) (app-exp (var-exp 'begin) body))
;             ;   (lambda() (app-exp (lambda-exp '() body) '())))]
;           [and-exp (body)
;             (app-sym-exp 'and body)]
;             ; (apply-env env 'and
;             ;   (lambda(x) (app-exp (var-exp 'and) body))
;             ;   (lambda() (cond
;             ;       [(null? body) (lit-exp #t)]
;             ;       [(null? (cdr body)) (car body)]
;             ;       [else (if-exp #t (car body)
;             ;         (and-exp (cdr body))
;             ;         (lit-exp #f))])))]
;           [or-exp (body)
;             (app-sym-exp 'or body)]
;             ; (apply-env env 'or
;             ;   (lambda(x) (app-exp (var-exp 'or) body))
;             ;   (lambda() (cond
;             ;       [(null? body) (lit-exp #f)]
;             ;       [(null? (cdr body)) (car body)]
;             ;       [else (let-exp 'let
;             ;           (list (cons 'val (car body)))
;             ;           (list (if-exp #t (var-exp 'val) (var-exp 'val) 
;             ;               (or-exp (cdr body)))))])))]
;           [else (eopl:error 'syntax-expand "Bad abstract syntax: ~a" exp)])])
;       (if (cexpression? expand)
;         expand
;         (curlev-expand expand))
;     )))






;-------------------+
;                   |
;   INTERPRETER     |
;                   |
;-------------------+



; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env))))

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
      [app-cexp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (map (lambda(x) (eval-exp x env)) rands)])
          (apply-proc proc-value args))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

; (define map (lambda(x) (eval-exp x env))
;   (lambda (rands)
;     (map eval-exp rands)))

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
      [else (eopl:error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))
  
(define *prim-proc-names* '(apply + - * / add1 sub1 zero? not = < > <= >= cons list null? assq eq? equal?
                            atom? car caar caaar caadr cadar cdaar caddr cdadr cddar cdddr cadr
                            cdar cddr cdr length list->vector list? pair? procedure? vector->list
                            vector make-vector vector-ref vector? number? symbol? set-car! set-cdr!
                            vector-set! display newline))

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
      [(apply) (apply-proc (car args) (cdr args))]
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
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read) (empty-env)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x (empty-env)))))

; Other Utility Methods
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