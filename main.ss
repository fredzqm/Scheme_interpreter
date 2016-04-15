; Fred Zhang and Zhou zhou

;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process

(load "chez-init.ss") 

;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+

; parsed expression
 
; (define-datatype expression expression?  
;   [var-exp        ; variable references
;    (id symbol?)]
;   [lit-exp        ; "Normal" data.  Did I leave out any types?
;    (datum
;     (lambda (x)
;       (ormap 
;        (lambda (pred) (pred x))
;        (list number? vector? boolean? symbol? string? pair? null?))))]
;   [app-exp        ; applications
;    (rator expression?)
;    (rands (list-of expression?))]  
;   )
(define (implist-of pred?)
  (lambda(implst)
    (let helper ([ls implst])
      (or (null? ls) (pred? ls)
        (and (pred? (car ls)) (helper (cdr ls)))))))


(define-datatype expression expression?
  [var-exp (id symbol?)]
  [lit-exp        ; "Normal" data.  Did I leave out any types?
       (datum (lambda (x)
          (ormap 
           (lambda (pred) (pred x))
           (list number? vector? boolean? symbol? string? pair? null?))))]
  [app-exp (rator expression?)
      (rands (list-of expression?))]
  [lambda-exp (vars (implist-of symbol?))
      (body (list-of expression?))]
  [let-exp (lettype (lambda(x)(or (eq? x 'let)(eq? x 'letrec)(eq? x 'let*)(eq? x 'letrec*))))
      (vars-ls (list-of (lambda(y)(and (symbol? (car y))(expression? (cdr y))))))
      (body (list-of expression?))]
  [let-named-exp (name symbol?)
      (vars-ls (list-of (lambda(y)(and (symbol? (car y))(expression? (cdr y))))))
      (body (list-of expression?))]
  [if-exp (test expression?)
      (then-op expression?)
      (else-op expression?)]
  [quote-exp 
        (datum (lambda (x)
          (ormap 
           (lambda (pred) (pred x))
           (list number? vector? boolean? symbol? string? pair? null?))))]
  [set!-exp (var symbol?)
      (val expression?)])

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure (vars (implist-of symbol?))
      (body (list-of expression?))
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
;      [(string? datum) (lit-exp datum)]
;      [(vector? datum) (lit-exp datum)]
;      [(boolean? datum) (lit-exp datum)]
;      [(pair? datum)
;       (cond
;        [else (app-exp (parse-exp (1st datum))
; 		      (map parse-exp (cdr datum)))])]
;      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))


(define parse-exp
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)]
     [(number? datum) (lit-exp datum)]
     [(vector? datum) (lit-exp datum)]
     [(boolean? datum) (lit-exp datum)]
     [(string? datum) (lit-exp datum)]
     [(null? datum) (lit-exp datum)]
     [(pair? datum)
      (cond
       [(eqv? 'quote (1st datum))
	      (if (not (null? (cddr datum)))
	        (eopl:error 'parse-exp "quote expression: incorrect length: ~s" datum))
	      (quote-exp (2nd datum))]
       [(eq? (car datum) 'lambda)
          (if (null? (cddr datum))
            (eopl:error 'parse-exp "lambda expression: incorrect length: ~s" datum))
          (if (not ((implist-of symbol?) (cadr datum)))
            (eopl:error 'parse-exp "lambda declaration list: formers must be symbols ~s" (cadr datum)))
          (lambda-exp (cadr datum) (map parse-exp (cddr datum)))]
       [(or (eq? (car datum) 'letrec) (eq? (car datum) 'let) (eq? (car datum) 'let*))
          (if (null? (cdr datum))
        (eopl:error 'parse-exp "~s expression: incorrect length: ~s" (car datum) datum))
          (let ([letarg (cadr datum)] [letop (cddr datum)])
            (if (not (list? (cadr datum)))
              (begin (set! letarg (caddr datum))
                (set! letop (cdddr datum))))
          (if (null? letop)
              (eopl:error 'parse-exp "~s expression: incorrect length: ~s" (car datum) letop))
            (if (not (list? letarg))
              (eopl:error 'parse-exp "~s declaration list: not a proper list: ~s" (car datum) letarg))
            (set! letarg (map (lambda(x) 
                    (if (not (list? x))
                      (eopl:error 'parse-exp "~s declaration pairs: not all proper list: ~s" (car datum) letarg))
                (if (not (symbol? (car x)))
                      (eopl:error 'parse-exp "~s declaration pairs: first members must be symbols: ~s" (car datum) letarg))
                (if (not (= (length x) 2))
                      (eopl:error 'parse-exp "~s declaration pairs: not all length 2: ~s" (car datum) letarg))
                  (cons (car x)(parse-exp (cadr x)))) (cadr datum)))
            (set! letop (map parse-exp (cddr datum)))
            (if (list? (cadr datum))
          (let-exp (car datum) letarg letop)
              (let-named-exp (cadr datum) letarg letop)))]
       [(eq? (car datum) 'if)
      		(if (null? (cddr datum))
            (eopl:error 'parse-exp "if expression: incorrect length: ~s" datum))
          (if (> (length datum) 4)
            (eopl:error 'parse-exp "if expression: should have (only) test, then, and else clauses: ~s" datum))
          (if-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)) (parse-exp (cadddr datum)))]
       [(eq? (car datum) 'set!)
          (if (null? (cddr datum))
            (eopl:error 'parse-exp "set! expression: missing expression: ~s" datum))
          (if (> (length datum) 3)
            (eopl:error 'parse-exp "set! expression: too many parts: ~s" datum))
          (set!-exp (cadr datum) (parse-exp (caddr datum)))]
       [else
          (if (not (list? datum))
            (eopl:error 'parse-exp "Error in parse-exp: application ~s is not a proper list" datum))
          (app-exp (parse-exp (car datum)) (map parse-exp (cdr datum)))])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))


(define unparse-exp
  (lambda (expr)
    (cases expression expr
      [app-exp (rator rands)
          (cons (unparse-exp rator) (map unparse-exp rands))]
      [lambda-exp (vars body)
          (cons* 'lambda vars (map unparse-exp body))]
      [if-exp (test then-op else-op)
          (list 'if (unparse-exp test) (unparse-exp then-op) (unparse-exp else-op))]
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
      [var-exp (id)
          id]
      [lit-exp (var)
          var])))


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



; To be added later









;-------------------+
;                   |
;   INTERPRETER    |
;                   |
;-------------------+



; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env)))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [quote-exp (datum) datum]
      [var-exp (id)
				(apply-env env id; look up its value.
      	   (lambda (x) x) ; procedure to call if id is in the environment 
           (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
		          "variable not found in environment: ~s" id)))]
      [if-exp (test then-op else-op)
        (if (eval-exp test env) (eval-exp then-op env) (eval-exp else-op env))]
      [lambda-exp (vars body)
        (closure vars body env)]
      [let-exp (lettype vars-ls body)
        (case lettype
          [(let) (eval-exp (app-exp
              (lambda-exp (map car vars-ls) body)
              (map cdr vars-ls)) env)]
        [else eopl:error 'eval-exp "Bad type of let expression"])]
      [app-exp (rator rands)
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
        (let lambdaEval ([code body][env (extend-env vars args env)])
          (if (null? (cdr body))
            (eval-exp (car body) env)
            (begin (eval-exp (car body) env)
              (lambdaEval (cdr body) env))))]
      ; You will add other cases
      [else (eopl:error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = < > <= >= cons list null? assq eq? equal?
                            atom? car caar caaar caadr cadar cdaar caddr cdadr cddar cdddr cadr
                            cdar cddr cdr length list->vector list? pair? procedure? vector->list
                            vector make-vector vector-ref vector? number? symbol? set-car! set-cdr!
                            vector-set! display newline))

(define init-env         ; for now, our initial global environment only contains 
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
      [(procedure?) (apply procedure? args)]
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
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))

; Other Utility Methods
(define not-pred
  (lambda (pred?)
    (lambda (arg)
      (not (pred? arg)))))

(define equal-to-n
  (lambda (n)
    (lambda (arg)
      (= n arg))))



