(define (addSyntaxExpansion)
(eval-many-exps '(

(define-syntax if
    (syntax-rules ()
      [(_ a b)  
          (if a b (values))]))

(define-syntax define
    (syntax-rules ()
      [(_ (n a ...) e1 e2 ...)
          (define n
            (lambda (a ...) e1 e2 ...))]
      [(_ (n a ... l) e1 e2 ...)
          (define n
            (lambda (a ... . l) e1 e2 ...))]))

(define-syntax let
    (syntax-rules ()
      [(_ ([x v] ...) e1 e2 ...)
          ((lambda (x ...) e1 e2 ...) v ...)]
      [(_ name ([x v] ...) e1 e2 ...)
          (letrec ([name (lambda (x ...) e1 e2 ...)])
            (name v ...))]))

(define-syntax let*
    (syntax-rules ()
      [(_ ([x v]) e1 e2 ...)
          (let ([x v]) e1 e2 ...)]
      [(_ ([x1 v1] [x2 v2] ...) e1 e2 ...)
          (let ([x1 v1])
            (let* ([x2 v2] ...) e1 e2 ...))]))


(define-syntax letrec
    (syntax-rules ()
      [(_ ([x v] ...) e1 e2 ...)
          (let ([x #f] ...)
            (set! x v) ...
            e1 e2 ...)]))

(define-syntax letrec*
    (syntax-rules ()
      [(_ ([x v]) e1 e2 ...)
          (letrec ([x v]) e1 e2 ...)]
      [(_ ([x1 v1] [x2 v2] ...) e1 e2 ...)
          (letrec ([x1 v1])
            (letrec* ([x2 v2] ...) e1 e2 ...))]))

(define-syntax begin
    (syntax-rules ()
      [(_ e1 e2 ...)
          ((lambda () e1 e2 ...))]))

(define-syntax and
    (syntax-rules ()
      [(_) #t]
      [(_ e) e]
      [(_ e1 e2 e3 ...)
       (if e1 (and e2 e3 ...) #f)]))

(define-syntax or
  (syntax-rules ()
    [(_) #f]
    [(_ e) e]
    [(_ e1 e2 e3 ...)
     (let ([t e1])
       (if t t (or e2 e3 ...)))]))

(define-syntax cond
  (syntax-rules (else)
    [(_ (else l)) l]
    [(_ (p1 e1) (p2 e2) ... (else l))
      (if p1 e1
        (cond (p2 e2) ... (else l)))]
    [(_ (p1 e1))
      (if p1 e1)]
    [(_ (p1 e1) (p2 e2) ...)
      (if p1 e1
        (cond (p2 e2) ...))]))

(define-syntax while
  (syntax-rules ()
    [(_ test e1 e2 ...)
      (let loop ()
        (if test (begin e1 e2 ... (loop))))]))


(define-syntax case
  (syntax-rules (else)
    [(_ sym ((p1 p2 ...) e1 e2 ... ) ... (else l1 l2 ...))
      (cond [(member sym '(p1 p2 ...)) e1 e2 ...] ... (else l1 l2 ...))]
    [(_ sym ((p1 p2 ...) e1 e2 ... ) ... )
      (cond [(member sym '(p1 p2 ...)) e1 e2 ...] ... )]))


(define-syntax define-class
  (syntax-rules ()
    [(_ (className ca ...)
      ([f fnit] ...)
      [(methodName a ...) e1 e2 ...] ...)
        (define (className ca ...)
          (let ([f fnit] ...)
            (lambda (method . args)
              (cond
                [(eq? 'methodName method)
                  (apply (lambda(a ...) e1 e2 ...) args)] ...
                [else (list 'className "Does not have method" method)]))))]))


(define-syntax with-values
  (syntax-rules ()
    [(_ p c)
      (call-with-values (lambda() p) c)]))


(define-syntax let-values
  (syntax-rules ()
    [(_ ([args v]) e1 e2 ...)
          (with-values v (lambda args e1 e2 ...))]
    [(_ ([args1 v1] [args2 v2] ...) e1 e2 ...)
          (with-values v1
            (lambda args1
              (let-values ([args2 v2] ...)
                e1 e2 ...)))]))


(define-syntax trace-lambda
  (syntax-rules ()
    [(_ name (a ...) e1 e2 ...)
      (lambda (a ...)
        (**displayIndent+**)
        (display (list 'name a ...)) (newline)
        (let ([ret ((lambda (a ...) e1 e2 ...) a ...)])
          (**displayIndent-**)
          (display ret) (newline)
          ret))]))

)))

