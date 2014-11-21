(load "pattern-matcher.scm")

;;;;;;;;;;;
;; const ;;
;;;;;;;;;;;

; TODO: add 'begin' to the parsing of lambdas bodies

(define (^const? x)
	(or 	(boolean? x)
			(char? x)
			(number? x)
			(string? x)
			))

(define (^var? x)
(and (symbol? x)(let ((p (member x *reserved-words*)))
	(if p #f #t))))

(define *reserved-words*
  '(and begin cond define do else if lambda
    let let* letrec or quasiquote unquote 
    unquote-splicing quote set!))

(define *void-object* (void))

(define (^opt-lambda-args-list? list)
	(if (not (list? list))
	    #f
	    (andmap ^var? list)))

(define (^reg-lambda-args-list? list)
	(if (not (list? list))
	    #f
	    (andmap ^var? list)))

;splits the improper list to a pair of proper list and single argument: (opt-lambda-args-list '(a b c . d)) returns '((a b c) . d)
(define (opt-lambda-args-list args-list succ)
	(if (not (pair? args-list))
	    (succ (cons '() args-list))
	    (opt-lambda-args-list (cdr args-list) (lambda (partial-args-list) 
	    (succ (cons (cons (car args-list) (car partial-args-list)) (cdr partial-args-list)))))))

(define (improper-list? x) ;TODO add tests
	(and 	(pair? x)
			(not (null? (cdr (last-pair x))))))

(define (get-opt-lambda-mandatory-args x) (car x))
(define (get-opt-lambda-optional-args x) (cdr x))

(define parse
	(let ((run
		(compose-patterns
		(pattern-rule
			(? 'c ^const?)
			(lambda (c) `(const ,c)))
		(pattern-rule
			`(quote ,(? 'c))
			(lambda (c) `(const ,c)))
		(pattern-rule
			`,(? 'v ^var?)
			(lambda (v) `(var ,v)))
		(pattern-rule 	;if3
			`(if ,(? 'test) ,(? 'dit))
			(lambda (test dit)
				`(if3 ,(parse test) ,(parse dit) (const ,*void-object*))))
		(pattern-rule 	;if2
			`(if ,(? 'test) ,(? 'dit) ,(? 'dif))
			(lambda (test dit dif)
				`(if3 ,(parse test) ,(parse dit) ,(parse dif))))
		(pattern-rule 	;opt-lambda
			`(lambda ,(? 'opt-arg-list improper-list?) ,(? 'body))
			(lambda (opt-arg-list body)
				(let* ( 	(args-list (opt-lambda-args-list opt-arg-list (lambda (x) x)))
							(mandatory-args (get-opt-lambda-mandatory-args args-list))
							(optional-arg (get-opt-lambda-optional-args args-list)))
					`(lambda-opt ,mandatory-args ,optional-arg ,(parse body)))))
		(pattern-rule 	;reg-lambda
			`(lambda ,(? 'arg-list ^reg-lambda-args-list?) ,(? 'body))
			(lambda (arg-list body) `(lambda-simple ,arg-list ,(parse body))))
		(pattern-rule
			`(define ,(? 'var) ,(? 'ex) )
			(lambda (var ex)
				`(define ,(parse var) ,(parse ex))))
		)))
	(lambda (e)
		(run e
			(lambda ()
				(error 'parse
				(format "I can't recognize this: ~s" e)))))))
