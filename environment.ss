; environment type definitions
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of (lambda(x) (or (not x)(symbol? x)))))
   (vals list?)
   (env environment?)))


(define memoize
  (let ([hashtable #f])
    (lambda (proc hash equiv?)
      (set! hashtable (make-hashtable hash equiv?))
      (lambda args
        (if (hashtable-contains? hashtable args)
          (hashtable-ref hashtable args #f)
          (let ([result (apply proc args)])
            (hashtable-set! hashtable args result)
            result))))))

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3
(define empty-env
  (lambda ()
    (empty-env-record)))

(define create-env
  (lambda (syms vals)
    (if (not (= (length syms)(length vals)))
      (eopl:error 'create-env "syms and vals has different length syms ~s vals" syms vals))
    (if (null? syms)
      (extended-env-record (list #f) (list #f) (empty-env))
      (extended-env-record syms vals (empty-env)))))

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