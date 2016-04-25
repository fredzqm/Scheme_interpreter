
(define pat
	(listpt (multpt (listpt (sympt 's) (listpt (exprpt 'v) (emptpt))) (emptpt))
		(listpt (exprpt 'b1)(multpt (exprpt 'b2) (emptpt)))))

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

(define body
	'(([a (+ 1 2)][b (* 1 2)])
			(+ a b) (- a b)))


(define matches 
	(matchpattern pat body))

(define result
	(assembleResult (endpt-r pat-r) matches '()))

(define result2
	(matchRule pat pat-r body))

