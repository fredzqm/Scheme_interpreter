; with continual passing style
; (define subst-leftmost-cps
; 	(lambda (new old slist equality-pred? yes no)
; 		(if (null? slist)
; 			(apply-continuation no)
; 			(if (pair? slist)
; 				(subst-leftmost-cps new old  (car slist) equality-pred?
; 					(lambda (x) (apply-continuation yes (cons x (cdr slist))))
; 					(lambda () (subst-leftmost-cps new old  (cdr slist) equality-pred?
; 						(lambda (x)(apply-continuation yes (cons (car slist) x)))
; 						(lambda ()(apply-continuation no)))))
; 				(if (equality-pred? old slist)
; 					(apply-continuation yes new)
; 					(apply-continuation no))))))

; (define (subst-leftmost new old slist equality-pred?)
; 	(subst-leftmost-cps new old slist equality-pred? (lambda(x) x) (lambda() slist)))

; (define (subst-left-cps-2args new old slist equality-pred? k)
; 	(letrec ([substitute-flag 
; 		(lambda (slist k)
; 			(cond
; 				[(null? slist) (apply-continuation k #f '())]
; 				[(list? slist)
; 					(substitute-flag (car slist)
; 						(lambda (flag x)
; 							(if flag
; 								(apply-continuation k flag (cons x (cdr slist)))
; 								(substitute-flag (cdr slist)
; 									(lambda (flag x)
; 										(apply-continuation k flag (cons (car slist) x)))))))]
; 				[else (if (equality-pred? old slist)
; 					(apply-continuation k #t new)
; 					(apply-continuation k #f slist))]))])
; 	(substitute-flag slist k)))

(define (subst-left-cps-2args new old slist equality-pred? k)
	(let helperMethod ([slist slist][k k])
		(cond
			[(null? slist) (apply-continuation k #f '())]
			[(list? slist)
				(helperMethod (car slist)
					(lambda (flag x)
						(if flag
							(apply-continuation k flag (cons x (cdr slist)))
							(helperMethod (cdr slist)
								(lambda (flag x)
									(apply-continuation k flag (cons (car slist) x)))))))]
			[else (if (equality-pred? old slist)
				(apply-continuation k #t new)
				(apply-continuation k #f slist))])))

(define apply-continuation
	(lambda (k . args)
		(apply k args)))