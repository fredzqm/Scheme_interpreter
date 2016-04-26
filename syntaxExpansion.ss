
(eval-one-exp
  '(define-syntax let
      (syntax-rules ()
        [(_ ([x v] ...) e1 e2 ...)
            ((lambda (x ...) e1 e2 ...) v ...)]
        [(_ name ([x v] ...) e1 e2 ...)
            (letrec ([name (lambda (x ...) e1 e2 ...)])
              (name v ...))])))

(eval-one-exp
  '(define-syntax let*
      (syntax-rules ()
        [(_ ([x v]) e1 e2 ...)
            (let ([x v]) e1 e2 ...)]
        [(_ ([x1 v1] [x2 v2] ...) e1 e2 ...)
            (let ([x1 v1])
              (let* ([x2 v2] ...) e1 e2 ...))])))


(eval-one-exp
  '(define-syntax letrec
      (syntax-rules ()
        [(_ ([x v] ...) e1 e2 ...)
            (let* ([x #f] ...)
              (set! x v) ...
              e1 e2 ...)])))

(eval-one-exp
  '(define-syntax letrec*
      (syntax-rules ()
        [(_ ([x v]) e1 e2 ...)
            (letrec ([x v]) e1 e2 ...)]
        [(_ ([x1 v1] [x2 v2] ...) e1 e2 ...)
            (letrec ([x1 v1])
              (letrec* ([x2 v2] ...) e1 e2 ...))])))

(eval-one-exp
  '(define-syntax begin
      (syntax-rules ()
        [(_ e1 e2 ...)
            ((lambda () e1 e2 ...))])))

(eval-one-exp
  '(define-syntax and
      (syntax-rules ()
        [(_) #t]
        [(_ e) e]
        [(_ e1 e2 e3 ...)
         (if e1 (and e2 e3 ...) #f)])))

(eval-one-exp
  '(define-syntax or
    (syntax-rules ()
      [(_) #f]
      [(_ e) e]
      [(_ e1 e2 e3 ...)
       (let ([t e1])
         (if t t (or e2 e3 ...)))])))

(eval-one-exp
  '(define-syntax cond
    (syntax-rules (else)
      [(_ (else l)) l]
      [(_ (p1 e1) (p2 e2) ... (else l))
        (if p1 e1
          (cond (p2 e2) ... (else l)))]
      [(_ (p1 e1))
        (if p1 e1)]
      [(_ (p1 e1) (p2 e2) ...)
        (if p1 e1
          (cond (p2 e2) ...))])))

(eval-one-exp
  '(define-syntax while
    (syntax-rules ()
      [(_ test e1 e2 ...)
        (let loop ()
          (if test (begin e1 e2 ... (loop))))])))

(eval-one-exp
  '(define member
    (lambda (var ls)
      (cond
        [(null? ls) #f]
        [(eq? var (car ls)) ls]
        [else (member var (cdr ls))]))))

(eval-one-exp
  '(define-syntax case
    (syntax-rules (else)
      [(_ sym ((p ...) e) ... (else l))
        (cond [(member sym (list p ...)) e] ... (else l))]
      [(_ sym ((p ...) e) ... )
        (cond [(member sym (list p ...)) e] ... )]
        )))

; [(cond)
;   (if (null? (cdr body))
;     (if (eqv? 'else (caar body))
;       (cadar body)
;       (app-cexp (var-cexp 'void) '()))
;     (list 'if (caar body) (cadar body) (cons 'cond (cdr body))))]
; [(case)
;   (let ([var (car body)]
;         [tests (cdr body)])
;     (list 'let (list (list 'var var))
;       (cons 'cond
;         (map (lambda (p)
;                 (if (eqv? 'else (car p))
;                   p
;                   (list (cons 'or (map (lambda (t) (list 'eqv? var t)) (car p))) (cadr p)))) tests))))]
