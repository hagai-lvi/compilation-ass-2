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


(define (let-vars-expressions-list? list) 	;TODO think what are the criterions for a let-vars-expressions-list
	(andmap (lambda (x)
				(and (list? x) (^var? (car x))))
			list))


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
	   `(define ,(? 'var ^var?) ,(? 'ex) )
	   (lambda (vari ex)
	     `(define (var ,vari) ,(parse ex))))
	  (pattern-rule
	   `(,(? 'va  ^var? ^var?) . ,(? 'varb list?))
	   (lambda(vari variables)
	     `(applic (var ,vari) ,(map (lambda(s)(parse s)) variables ))))
	  (pattern-rule
	   `(,(? 'va list?) . ,(? 'va2 list?))
	   (lambda(first rest)
	     `(applic ,(parse first) ,(map (lambda(exp)(parse exp)) rest))))
	  (pattern-rule 	;let*
			`(let* ,(? let-vars-expressions-list?) ,(? 'body))
			(lambda (exp-list body)
				(parse (letstar exp-list body))))
		)))
	(lambda (e)
		(run e
			(lambda ()
				(error 'parse
				(format "I can't recognize this: ~s" e)))))))

(define (letstar exp-list body)
	(if (= (length exp-list) 0)
	    body
	    (let*( 	(seperated-exp-list (seperate-last-element exp-list))
				(last (cdr seperated-exp-list))
				(rest (car seperated-exp-list)))
		(letstar rest `((lambda (,(car last)) ,body ) ,(cadr last)))
	)))

; return a pair that contain the head of the list and the last element of the list
; example: (seperate-last-element '(1 2 3 4) returns '((1 2 3) . 4)
(define (seperate-last-element list)
	(letrec ((f (lambda (list succ)
					(if (null? (cdr list))
					    (succ `() (car list))
					    (f (cdr list) (lambda (rest last)
					    					(succ (cons (car list) rest) last)))))))
	(f list (lambda (x y) (cons x y)))))
