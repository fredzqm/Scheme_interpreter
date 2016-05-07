; environment type definitions
; (define-datatype environment environment?
;   (empty-env-record)
;   (extended-env-record
;    (syms (list-of (lambda(x) (or (not x)(symbol? x)))))
;    (vals list?)
;    (env environment?)))


(define environment?
  hashtable?)

(define (empty-env)
  (make-hashtable 
    (lambda(s)(string-length (symbol->string s)))
    symbol=?))

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3
(define create-env
  (lambda (syms vals)
    (if (not (= (length syms)(length vals)))
      (eopl:error 'create-env "syms and vals has different length syms ~s vals" syms vals))
    (let ([env (empty-env)])
      (let loop ([syms syms][vals vals])
        (if (null? syms)
          env
          (begin
            (define-in-env! env (car syms) (car vals))
            (loop (cdr syms)(cdr vals))))))))

; (define memoize
;   (let ([hashtable #f])
;     (lambda (proc hash equiv?)
;       (set! hashtable (make-hashtable ))
;       (lambda args
;         (if (hashtable-contains? hashtable args)
;           (hashtable-ref hashtable args #f)
;           (let ([result (apply proc args)])
;             (hashtable-set! hashtable args result)
;             result))))))

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (if (hashtable-contains? env sym)
      (apcont succeed (hashtable-ref env sym #f))
      (apcont fail))))


(define define-in-env!
  (lambda (env sym val)
    (hashtable-set! env sym val)))
