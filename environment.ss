(define apcont
  (lambda (k . x)
    (apply k x)))

;-------------------+
;                   |
;   ENVIRONMENTS    |
;                   |
;-------------------+

; environment type definitions
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (vals list?)
   (alist (list-of (lambda(x))))
   (env environment?)))

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3
(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (syms vals env k)
    (let loop ([sym syms][val vals]
              [k (lambda(x)
                      (apcont k (extended-env-record x '() env)))])
      (cond
        [(null? sym)
          (if (null? vals)
            (apcont k '())
            (eopl:error 'extend-env "Incorrect number of argument, syms: ~s, vals: ~s" syms vals))]
        [(symbol? sym)
          (apcont k (list val))]
        [else
          (if (null? val)
            (eopl:error 'extend-env "Incorrect number of argument, syms: ~s, vals: ~s" syms vals)
            (loop (cdr sym) (cdr val)
              (lambda(x)
                (apcont k (cons (list (car val)) x)))))])))))

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