
(define p
	(parse-syntax-pattern 
		'(() x y z x1 x2 x3 (b2 b2x b2xx ...) ... b3 ...)
		'(x y)))

(define occurs
	(occurs-syntax-pattern p))

(define r0
	(parse-result-pattern
		'(let () b1 b2 ...)
		'(b1 b2)))

(define r1
	(parse-result-pattern
		'(let () ba ba b2 ...)
		'(ba b2)))


(define r2Matches
    '(( (b1 . b1r)
    	(end . endr))
      ( (((b2 . b2r)) ) )))

(define r2
	(parse-result-pattern
		'(ok () b1 b2 ... . end)
		'(b1 b2 end)))

; (define r2R
; 	(assembleResult r2 r2Matches '()))