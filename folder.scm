(load "compiler.scm")


(define fold
	(let ((run
		(compose-patterns
			(pattern-rule
				`(+ . ,(? `args))
				(lambda (args)
					)
			)
		)))
	(lambda (e)
		(let ()
			(run e
			(lambda ()
				(error 'parse
				(format "I can't recognize this: ~s" e)))))
		)))

(define (flatten sym list)
	(letrec ((f (lambda (list succ)
					(cond ((null? list) (succ list))
					      ((and (list? (car list)) (eqv? (car (car list)) sym)) (f (cdr list) (lambda (rest) (succ `(,@(cdar list) ,@rest )))))
					      (else (f (cdr list) (lambda (rest) (succ `(,(car list) ,@rest)))))
					  ))))
		(f list (lambda(x) x))))

(define (quotify l)
	(if (and (list? l) (> (length l) 1))
		`(quote (,@l))
		l
		))

(define (part pred lst)
	(cons 	(cons #t (filter pred lst))
			(cons #f (filter (lambda (x) (not (pred x)) ) lst))
	))
