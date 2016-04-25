
(define p
	(parse-syntax-pattern 
		'(() b1 b2 ...)
		))

(define r1
	(parse-result-pattern
		'(let () b1 b2 ...)
		))


(define r2Matches
    '(( (b1 . b1r)
    	(end . endr))
      ( (((b2 . b2r)) ) )))

(define r2
	(parse-result-pattern
		'(let () b1 b2 ... . end)
		))

(define r2R
	(assembleResult r2 r2Matches '()))