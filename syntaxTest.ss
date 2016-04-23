
(define pat
	(listpt (multpt (listpt (sympt 's) (listpt (exprpt 'v) (emptpt))))
		(listpt (exprpt 'b1)(multpt (exprpt 'b2)))))

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

(define matches 
	(matchpattern pat
		'(([a (+ 1 2)][b (* 1 2)])
			(+ a b) (- a b))))

(define result
	(assembleResult pat-r matches '()))