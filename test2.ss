
(define pat
	(listpt (exprpt 'a) (listpt (exprpt 'b) (emptpt))))

(define pat-r
	(listpt-r (list
		(exprpt-r 'a)
		(exprpt-r 'b)
		)))

(define matches 
	(matchpattern pat
		'(ax bx)
		))

(define result
	(assembleResult pat-r matches '()))