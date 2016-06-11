(define (addPredefinedProcedures)
(eval-many-exps '(

(define map
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
                (map-more (cdr ls) (map cdr more))))))))

(define member
  (lambda (x ls)
    (cond
      [(null? ls) #f]
      [(equal? (car ls) x) ls]
      [else (member x (cdr ls))])))

(define-class (stack)
  ([s '()])
  [(pop)
      (let ([x (car s)])
        (set! s (cdr s))
        x)]
  [(push e)
      (set! s (cons e s))])

(define exit-list
  (lambda x
    (return x)))

(with-values
  (call/cc 
    (lambda (k)
      (set! return k)))
  values)

(define escaper
  (lambda (p)
    (lambda args
      (with-values
        (apply p args)
        return))))

(define resume 'resume-undefined)

(define make-coroutine
  (lambda (body)
    (let ([local-continuation 'local-continuation-undefined])
      (letrec
          ([newcoroutine
            (lambda  (value) (local-continuation value))]
           [localresume
            (lambda  (continuation value)
              (let ([value (call/cc (lambda (k)
                                      (set! local-continuation k)
                                      (continuation value)))])
                (set! resume localresume)
                value))])
        (call/cc
         (lambda (exit)
           (body (localresume exit newcoroutine))
           (error 'co-routine "fell off end of coroutine")))))))


(define **trace-level** 0)

(define (**displayIndent**)
  (let loop ([i **trace-level**])
    (display "| ")
    (if (< 0 i)
      (loop (- i 1)))))
        
(define (**displayIndent+**)
  (**displayIndent**)
  (set! **trace-level** (+ **trace-level** 1)))

(define (**displayIndent-**)
  (set! **trace-level** (- **trace-level** 1))
  (**displayIndent**))


(define display-traced-output
  (let ([multi-indent-string
   (lambda (level)
     (let loop ([level level] [result ""])
       (if (zero? level)
     result
     (loop (- level 1) (string-append result "| ")))))])
  (lambda args   ; (level proc-name args) or (level answer)
    (let ([indent-string (multi-indent-string (car args))])
      (display indent-string)
      (display (if (= 2(length args))
       (cadr args)
       (cons (cadr args) (caddr args)))))
      (newline))))

)))
