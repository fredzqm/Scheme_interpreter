
; (define pat
; 	(listpt (multpt (listpt (sympt 's) (listpt (exprpt 'v) (emptpt))) (emptpt))
; 		(listpt (exprpt 'b1)(multpt (exprpt 'b2) (emptpt)))))

(define pat
	(listpt (multpt (sympt 'v) (emptpt))
            (multpt (exprpt 'e) (emptpt)))
	)

(define pat-r
	(listpt-r (list
		(listpt-r (list 
			(contpt-r 'lambda)
			(listpt-r (list
				(multpt-r 1 (exprpt-r 's))))
			(exprpt-r 'b1)
			(multpt-r 2 (exprpt-r 'b2))
			))
		(multpt-r 1 (exprpt-r 'v))
		)))

; (define body
; 	'(([a (+ 1 2)][b (* 1 2)])
; 			(+ a b) (- a b)))

(define body
	'(() 2))

(define matches 
	(matchpattern pat body))

(define result
	(assembleResult pat-r matches '()))

(define p
	(parse-syntax-pattern 
		'(() b1 b2 ...)
		))

(define r
	(parse-result-pattern
		'(let () b1 b2 ...)
		))