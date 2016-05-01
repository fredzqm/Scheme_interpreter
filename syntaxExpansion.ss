
(eval-one-exp
  '(define map
    (lambda (f ls . more)
      (if (null? more)
          (let map1 ([ls ls])
            (if (null? ls)
                '()
                (cons (f (car ls))
                      (map1 (cdr ls)))))
          (let map-more ([ls ls] [more more])
            (if (null? ls)
                '()
                (cons
                  (apply f (car ls) (map car more))
                  (map-more (cdr ls) (map cdr more)))))))))


(eval-one-exp
  '(define-syntax if
      (syntax-rules ()
        [(_ a b)
            (if a b (void))])))

(eval-one-exp
  '(define-syntax define
      (syntax-rules ()
        [(_ (n a ...) e1 e2 ...)
            (define n
              (lambda (a ...) e1 e2 ...))]
        [(_ (n a ... l) e1 e2 ...)
            (define n
              (lambda (a ... . l) e1 e2 ...))])))

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
            (let ([x #f] ...)
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
  '(define-syntax case
    (syntax-rules (else)
      [(_ sym ((p1 p2 ...) e1 e2 ... ) ... (else l1 l2 ...))
        (cond [(member sym '(p1 p2 ...)) e1 e2 ...] ... (else l1 l2 ...))]
      [(_ sym ((p1 p2 ...) e1 e2 ... ) ... )
        (cond [(member sym '(p1 p2 ...)) e1 e2 ...] ... )])))


(eval-one-exp
  '(define-syntax define-class
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
                  [else (list 'className "Does not have method" method)]))))])))


(eval-one-exp
  '(define-class (stack)
    ([s '()])
    [(pop)
        (let ([x (car s)])
          (set! s (cdr s))
          x)]
    [(push e)
        (set! s (cons e s))]))

