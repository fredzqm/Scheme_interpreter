(define apcont
  (lambda (k . x)
    (apply k x)))

;-------------------+
;                   |
; ENVIRON TEMPLETE  |
;                   |
;-------------------+

(define (empty-templete)
  '())

; create another level of environment
(define (extend-templete syms env k)
  (if (not ((list-of symbol?) syms))
    (eopl:error 'extend-templete "syms should be a list of symbols ~s" syms))
  (apcont k (cons (list syms) env)))

; add a posible symbol to this level
(define (add-pos-sym sym env k)
  (if (not (symbol? sym))
    (eopl:error 'add-pos-sym "sym should be a symbol ~s" sym))
  (apcont k 
    (if (or (member sym (caar env)) (member sym (cdar env)))
      env
      (cons
        (cons (caar env) (cons sym (cdar env)))
        (cdr env)))))

; return a var-cexp represented in lexical order.
; variable representation
; free var: '(name 0)
; bounded var: '(name depth . index)
; search free var: '(name d1 d2 d3 ...)
; search bounded var: '(name d1 d2 d3 ... depth . index)
(define (search-in-templete sym env k)
  (let helper ([env env]
      [k (lambda (num ls)
        (apcont k (if num
          (cons* sym num ls)
          (cons sym ls))))])
    (if (null? env)
        (apcont k #f '())
        (index-in-ls sym (caar env)
          (lambda(lexiIndex)
            (if lexiIndex
              (apcont k 0 lexiIndex)
              (helper (cdr env)
                (lambda (num ls)
                  (if num
                    (index-in-ls sym (cdar env)
                      (lambda (posible)
                        (if posible
                          (apcont k 0 (cons (+ num 1) ls))
                          (apcont k (+ num 1) ls))))
                    (index-in-ls sym (cdar env)
                      (lambda (posible)
                        (if posible
                          (apcont k 0 '())
                          (apcont k #f '())))))))))))))


(define (index-in-ls sym ls k)
  (if (null? ls)
    (apcont k #f)
    (if (eq? sym (car ls))
      (apcont k 0)
      (index-in-ls sym (cdr ls)
        (lambda (x)
          (apcont k (and x (+ 1 x))))))))

; (define extend-env
;   (lambda (syms vals env k)
;     (let loop ([sym syms][val vals]
;               [k (lambda(x)
;                       (apcont k (extended-env-record x '() env)))])
;       (cond
;         [(null? sym)
;           (if (null? vals)
;             (apcont k '())
;             (eopl:error 'extend-env "Incorrect number of argument, syms: ~s, vals: ~s" syms vals))]
;         [(symbol? sym) ; in case syms is an improper list, this case handles that
;           (apcont k (list val))]
;         [else ; the regular case
;           (if (null? val)
;             (eopl:error 'extend-env "Incorrect number of argument, syms: ~s, vals: ~s" syms vals)
;             (loop (cdr sym) (cdr val)
;               (lambda(x)
;                 (apcont k (cons (list (car val)) x)))))])))))

; (define apply-env
;   (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
;     (cases environment env
;       (empty-env-record ()
;         (fail))
;       (extended-env-record (syms vals env)
;         (let ((pos (list-find-position sym syms)))
;           (if (number? pos)
;             (succeed (list-ref vals pos))
;             (apply-env env sym succeed fail)))))))


; (define add-to-env!
;   (letrec ([helper! (lambda (ls val)
;                       (if (null? (cdr ls))
;                         (if (car ls) ; if the list is initially empty, which has a #f in it
;                           (set-cdr! ls (list val))
;                           (set-car! ls val))
;                         (helper! (cdr ls) val)))])
;     (lambda (env sym val)
;       (cases environment env
;         (empty-env-record () 
;           (eopl:error 'add-to-env! "Cannot add to the end of an empty environment"))
;         (extended-env-record (syms vals env)
;           (helper! syms sym)
;           (helper! vals val))))))

; (define change-env!
;   (lambda (env sym val)
;     (cases environment env
;       (empty-env-record () (change-env! global-env sym val)) ; At top level
;       (extended-env-record (syms vals encl_env)
;         (let ((pos (list-find-position sym syms)))
;           (if (number? pos)
;             (list-set-at-index! vals pos val) ; Value is found
;             (if (eq? env global-env)
;               (add-to-env! env sym val)
;               (change-env! encl_env sym val))))))))

; (define define-in-env!
;   (lambda (env sym val)
;     (cases environment env
;       (empty-env-record () (define-in-env! global-env sym val)) ; At top level
;       (extended-env-record (syms vals encl_env)
;         (let ((pos (list-find-position sym syms)))
;           (if (number? pos)
;             (list-set-at-index! vals pos val) ; Value is found
;             (add-to-env! env sym val) ; We don't care if it is in global; if it's not found in the current scope, it just needs to be added.
;             ))))))

