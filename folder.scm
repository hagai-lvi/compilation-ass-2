(load "compiler.scm")


(define fold
	(let ((run
		(compose-patterns
			(pattern-rule
				`(+ . ,(? 'vars-list))
				(lambda (vars-list) vars-list))
		)))
	(lambda (e)
		(run e
			(lambda ()
				(error 'parse
				(format "I can't recognize this: ~s" e)))))))
