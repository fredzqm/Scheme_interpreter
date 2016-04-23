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


; (define-datatype expression expression?
;   [var-exp (id symbol?)]
;   [lit-exp
;        (datum (lambda (x)
;           (ormap 
;            (lambda (pred) (pred x))
;            (list number? vector? boolean? symbol? string? pair? null?))))]
;   [app-exp (rator expression?)
;       (rands (list-of expression?))]
;   [app-sym-exp (ratorSym symbol?)
;       (rands (list-of expression?))]
;   [lambda-exp (vars (implist-of symbol?))
;       (body (list-of expression?))]
;   [set!-exp (var symbol?)
;       (val expression?)]
;   [if-exp
;       (two-armed? boolean?)
;       (test expression?)
;       (then-op expression?)
;       (else-op (or-pred null? expression?))]
;   [let-exp (lettype (lambda(x)(or (eq? x 'let)(eq? x 'letrec)(eq? x 'let*)(eq? x 'letrec*))))
;       (vars-ls (list-of (lambda(y)(and (symbol? (car y))(expression? (cdr y))))))
;       (body (list-of expression?))]
;   [let-named-exp (name symbol?)
;       (vars-ls (list-of (lambda(y)(and (symbol? (car y))(expression? (cdr y))))))
;       (body (list-of expression?))]
;   [quote-exp 
;         (datum (lambda (x)
;           (ormap 
;            (lambda (pred) (pred x))
;            (list number? vector? boolean? symbol? string? pair? null?))))]
;   [begin-exp (body (list-of expression?))]
;   [and-exp (body (list-of expression?))]
;   [or-exp (body (list-of expression?))]
;   )

(define-datatype syntaxType syntaxType?
  [patternSyntax 
    (syntaxList (list-of (lambda(x) 
      (and (syntax-pattern? (car x))(syntax-pattern? (cdr x))))))]
  [coreSyntax 
    (sym symbol?)]
  [primitiveSyntax 
    (sym symbol?)])

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
                              (lambda(x) (apply-syntax x (cdr datum) env)) ; does proper syntax exapnsion
                              (lambda() (app-cexp (var-cexp ratorSym) (map (lambda (d) (parse-exp d env)) (cdr datum))))))))
              (app-cexp (parse-exp (car datum) env) (map (lambda (d) (parse-exp d env)) (cdr datum))))]
          [else (eopl:error 'parse-exp "bad expression: ~s" datum)]))))

; Zero error-checking for now
(define apply-syntax
  (lambda (syntax body env)
    (let ([curlev-parse (lambda (exp) (parse-exp exp env))])
      (cases syntaxType syntax
        [patternSyntax (syntaxList)
            (or (ormap (lambda(x) (matchRule (car x) (cdr x) body)) syntax)
              (eopl:error 'apply-syntax "Attempt to apply bad syntax: ~s" syntax))]
        [coreSyntax (sym)
          (case sym
            [(quote) (apply lit-cexp body)]
            [(lambda)
              (lambda-cexp (car body) (map curlev-parse (cdr body)))]
            [(if)
              (if-cexp
                (curlev-parse (car body))
                (curlev-parse (cadr body))
                (if (null? (cddr body))
                    (lit-cexp (void))
                    (curlev-parse (caddr body))))])]
        [primitiveSyntax (sym)
          (curlev-parse
            (case sym
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
                  (cons (cons* 'lambda (map car (car body)) (cdr body))
                    (map cadr (car body))))]
              [(let*)
                (if (or (null? (cdar body)) (null? (car body)))
                    (cons 'let body)
                    (list 'let (list (caar body))
                          (cons* 'let* (cdar body) (cdr body))))]
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
                  (cons* 'let 'temp (list (list 'test t)) bodies))])
            )
          ]))))



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
(define *core-syntax-names* '(quote lambda if))
(define *prim-syntax-names* '(let let* letrec letrec* begin and or cond case while))

; To be added with define-syntax
(define global-syntax-env 
  (extend-env 
    *prim-syntax-names*
    (map primitiveSyntax *prim-syntax-names*)
    (extend-env
      *core-syntax-names*
      (map coreSyntax *core-syntax-names*) 
      (empty-env))))


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
  
(define *prim-proc-names* '(apply map + - * / add1 sub1 zero? not = < > <= >= cons list null? assq eq?
                            eqv? equal? atom? car caar caaar caadr cadar cdaar caddr cdadr cddar cdddr
                            cadr cdar cddr cdr length list->vector list? pair? procedure? vector->list
                            vector make-vector vector-ref vector? number? symbol? set-car! set-cdr!
                            vector-set! display newline void))

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