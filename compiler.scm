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

(define (get-lambda-variables vars)
	(if (=(length vars)0)
		'()
		(cons (caar vars) (get-lambda-variables (cdr vars)))))


(define (get-lambda-arguments exp)
	(if (=(length exp)0)
		'()
		(cons (cadar exp) (get-lambda-arguments (cdr exp)))))

(define expand-qq
  (lambda (e)
    (cond ((unquote? e) (cadr e))
	  ((unquote-splicing? e)
	   (error 'expand-qq "unquote-splicing here makes no sense!"))
	  ((pair? e)
	   (let ((a (car e))
		 (b (cdr e)))
	     (cond ((unquote-splicing? a) `(append ,(cadr a) ,(expand-qq b)))
		   ((unquote-splicing? b) `(cons ,(expand-qq a) ,(cadr b)))
		   (else `(cons ,(expand-qq a) ,(expand-qq b))))))
	  ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
	  ((or (null? e) (symbol? e)) `',e)
	  (else e))))

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define quasiquote? 
	(trace-lambda qq(e)
 (eq? e 'quasiquote)))

(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define s 'unquote)
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
		(pattern-rule 	;lambda-variadic
			`(lambda ,(? `var ^var?) . ,(? `body))	;TODO need to check if the body is legal (also in opt and regular lambdas)
			(lambda (args body)
				`(lambda-variadic ,args ,(parse `(begin ,@body)) )))

				(pattern-rule 	;opt-lambda
			`(lambda ,(? 'opt-arg-list improper-list?) . ,(? 'body))
			(lambda (opt-arg-list body)
				(let* ( 	(args-list (opt-lambda-args-list opt-arg-list (lambda (x) x)))
							(mandatory-args (get-opt-lambda-mandatory-args args-list))
							(optional-arg (get-opt-lambda-optional-args args-list)))
					`(lambda-opt ,mandatory-args ,optional-arg ,(parse `(begin ,@body))))))
		
		

	(pattern-rule 	;reg-lambda
			`(lambda ,(? 'arg-list ^reg-lambda-args-list?) . ,(? 'body))
			(lambda (arg-list body) `(lambda-simple ,arg-list ,(parse `(begin ,@body)))))

	   (pattern-rule
	   `(define ,(? 'var ^var?) ,(? 'ex) )
	   (lambda (vari ex)
	     `(define (var ,vari) ,(parse ex))))
	  	(pattern-rule
	  	`(define (,(? 'name) . ,(? 'varb)) ,(? 'exp))
	  	(trace-lambda define(first rest exp)
	  		`(define (var ,first) ,(parse `(lambda ,rest ,exp)))))
	  (pattern-rule
	  	`(define (,(? 'name) . ,(? 'varb)) ,(? 'exp))
	  	(trace-lambda define(first rest exp)
	  		`(define (var ,first) ,(parse `(lambda ,rest ,exp)))))
	   (pattern-rule
	   `(begin)
	   (lambda()
	     `(const ,*void-object*)))
	   (pattern-rule
	   `(begin ,(? `rest))
	   (lambda(rest)
	     (parse rest)))
	  (pattern-rule
	   `(begin . ,(? `rest))
	   (lambda(rest)
	     `(seq ,(map (lambda(exp)(parse exp))  rest))))
	  (pattern-rule
	   `(,(? 'a quasiquote?) . ,(? `rest))
	   (lambda(first rest)
	     (expand-qq (car rest))))
	  (pattern-rule
	   `(let ,(? 'va ) . ,(? 'body))
	   (lambda(vars body)
	     (parse `((lambda ,(get-lambda-variables vars) ,@body) ,@(get-lambda-arguments vars)))))
	  
	  (pattern-rule 	;let*
			`(let* ,(? let-vars-expressions-list?) . ,(? 'body))
			(lambda (exp-list body)
				(parse (letstar exp-list `(,@body)))))
	  
	    (pattern-rule 	;let*
			`(and)
			(lambda ()
			`(const #t)))
	    (pattern-rule
	    	`(and ,(? 'first))
			(lambda (first)
				(parse first)))
	  (pattern-rule 	;let*
			`(and ,(? 'first) ,(? 'second))
			(lambda (first second)
			(parse `(if ,first ,second #f))))
	  (pattern-rule 	;let*
			`(and ,(? 'first) . ,(? 'rest))
			(lambda (first rest)
			(parse `(if ,first (and ,@rest) #f))))
 (pattern-rule
	   `(,(? 'va  ^var?) . ,(? 'varb list?))
	   (lambda(vari variables)
	     `(applic (var ,vari) ,(map (lambda(s)(parse s)) variables ))))
	  (pattern-rule
	   `(,(? 'va list?) . ,(? 'va2 list?))
	   (lambda(first rest)
	     `(applic ,(parse first) ,(map (lambda(exp)(parse exp)) rest))))
		  (pattern-rule
	   `(let ,(? 'va ) ,(? 'body))
	   (lambda(vars body)
	     (parse  `((lambda ,(get-lambda-variables vars) ,body) ,@(get-lambda-arguments vars)))))
	  (pattern-rule
	  	'(cond ,(? cond-list) ) ; TODO add identifier for cond list
		(lambda (cond-list) (parse (expand-cond cond-list) ))
		)
		)))
	(lambda (e)
		(run e
			(lambda ()
				(error 'parse
				(format "I can't recognize this: ~s" e)))))))

(define letstar (trace-lambda letstar (exp-list body)
	(if (= (length exp-list) 0)
	    body
	    (let*( 	(seperated-exp-list (seperate-last-element exp-list))
				(last (cdr seperated-exp-list))
				(rest (car seperated-exp-list)))
		(letstar-new rest `((lambda (,(car last)) ,@body ) ,(cadr last)))
	))))

(define letstar-new (trace-lambda letstar (exp-list body)
	(if (= (length exp-list) 0)
	    body
	    (let*( 	(seperated-exp-list (seperate-last-element exp-list))
				(last (cdr seperated-exp-list))
				(rest (car seperated-exp-list)))
		(letstar-new rest `((lambda (,(car last)) ,body ) ,(cadr last)))
	))))

(define (expand-cond cond-list)
	(letrec ((f (lambda (cond-list succ)
					(cond 	((null? cond-list) (succ cond-list))
							((and (eqv? `else (caar cond-list)) (null? (cdr cond-list)) ) (succ (cadar cond-list))) ; TODO handle else
							((and (eqv? `else (caar cond-list)) (not (null? (cdr cond-list))) ) (error `expand-cond (format "else clause must be the last in a cond expression."))) ; TODO ERROR
							(else 	(f 	(cdr cond-list)
										(lambda (rest)
											(if 	(null? rest)
							
													(succ `(if ,(caar cond-list) ,(cadar cond-list) ))
													(succ `(if ,(caar cond-list) ,(cadar cond-list) ,rest))))))))))
		(f cond-list (lambda (x) x))))

(define (expand-letrec let-list body)
	(let ( 	(firsts (map (lambda (list) (car list)) let-list))
			(lasts (map (lambda (list) (cadr list)) let-list)))
		`((lambda (,firsts) ,body ) ,@lasts )))
;;;;;;;;;;;;;;;;;;
;;; HAGAI-TODO ;;;
;;; lambda-variadic ;;;
;;; letrec
;;;;;;;;;;;;;;;;;;

; return a pair that contain the head of the list and the last element of the list
; example: (seperate-last-element '(1 2 3 4) returns '((1 2 3) . 4)
(define (seperate-last-element list)
	(letrec ((f (lambda (list succ)
					(if (null? (cdr list))
					    (succ `() (car list))
					    (f (cdr list) (lambda (rest last)
					    					(succ (cons (car list) rest) last)))))))
	(f list (lambda (x y) (cons x y)))))

