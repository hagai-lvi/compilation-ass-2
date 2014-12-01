(load "compiler.scm")

;
; **********   *******   *******     *******  
;/////**///   **/////** /**////**   **/////** 
;    /**     **     //**/**    /** **     //**
;    /**    /**      /**/**    /**/**      /**
;    /**    /**      /**/**    /**/**      /**
;    /**    //**     ** /**    ** //**     ** 
;    /**     //*******  /*******   //*******  
;    //       ///////   ///////     ///////   
;                     
; Ilya: 	+, *, add1, append, car, cdr, cons, list
; 
; Hagai:	
; 		Done: number?, string?, string-append, sub1, zero?, null?


(define (id x)x)
(define quote? 
	(lambda (e)
 (or (eq? e 'quote)(and (pair? e)(eq? (car e) 'quote)))))


(define get-ex-left (lambda (op e)(let*  ((exp (fold e))
   	(exp2 (if(quote? exp)`'(,@(op (cadr exp)))
   		(op exp))))
   	(if(equal? (car exp) 'cons)
   	(op (cdr exp))exp2))))


(define get-ex-right (lambda (op e)(let*  ((exp (fold e))
   	(exp2 (if(quote? exp)`'(,@(op (cadr exp)))
   		(op exp))))
   	(if(equal? (car exp) 'cons)
   	(car (op (cdr exp)))exp2))))


(define const? (lambda(x)(or (^const? x)(quote? x))))

(define n-const? (lambda(x)(not(or (^const? x)(quote? x)))))



(define (super-map-fold vars)
	(map (lambda(x)(let ((exp (fold x)))
						 (if (quote? exp )(cadr exp) exp))) vars))



(define fold
	(let ((run
		(compose-patterns
			(pattern-rule
				`(+ . ,(? `args))
				(lambda (args) 
					(let* (
							(exp (if 	(list? args)(map fold args)args))	 
							(flat-list (flatten `+ exp))
							(part (part number? flat-list))
							(numbers (car part))
							(non-numbers (cdr part))
							(sum (apply + numbers)))
					(if	(null? non-numbers)
						sum
						`(+ ,sum ,@non-numbers) ))))
			(pattern-rule
				`(* . ,(? `args))
				(lambda (args)
					(let* (	(exp (if 	(list? args)(map fold args)args))
							(flat-list (flatten `* exp))
							(part (part number? flat-list))
							(numbers (car part))
							(non-numbers (cdr part))
							(sum (apply * numbers)))
					(if	(null? non-numbers)
						sum
						`(* ,sum ,@non-numbers) ))))
			(pattern-rule
			`(,(? 'quote quote?))
			(lambda(vars)
				 (fold vars)))
			
			(pattern-rule
			`(,(? 'quote quote?) ,(? `vars list?))
			(lambda(q vars)
				 `(quote ,vars)))		
			(pattern-rule
			`(,(? 'quote quote?) ,(? `vars))
			(lambda(q var)
				 `(quote ,var)))
			(pattern-rule
			`(,(? 'quote quote?) . ,(? `vars))
			(lambda(q vars)
				 `(quote ,vars)))
			(pattern-rule

			`(cons . ,(? `vars (lambda(vars)(ormap (lambda (x)(not (or (^const? x)(quote? x))))vars))))
			(lambda(vars)
				`(cons ,@vars)))
			
			(pattern-rule
			`(cons  ,(? `first quote?) ,(? 'quote quote?) ) 
				(lambda (first  second)
				`'(,(cadr (fold first)),@(cadr (fold second)))))


			(pattern-rule
			`(cons  ,(? `first ) ,(? 'quote quote?) ) 
				(lambda (first  second)
				`'(,(fold first),@(cadr (fold second)))))

			
			(pattern-rule
			`(cons  ,(? `first quote?) ,(? 'quote )) 
				(lambda (first  second)
				`'(,(cadr (fold first)),@(fold second))))			

			(pattern-rule
			`(cons  ,(? `first ) ,(? 'quote ?) ) 
				(lambda (first  second)
				`'(,(fold first),@(fold second))))


			(pattern-rule
			`(list . ,(? `vars (lambda(vars)(andmap (lambda(x)(not (or (^const? x)(quote? x))))vars))))
			(lambda(vars)
				`(list ,@vars)))
			
			
			(pattern-rule
			`(list . ,(? `first (lambda(vars)(andmap (lambda(x)(const? x))vars)))) 
				(lambda(vars)
				`'(,@(super-map-fold vars))))
			; (pattern-rule
			; `(list  ,(? `first ) ,(? 'quote quote?) ) 
			; 	(trace-lambda a(first  second)
			; 	`'(,(fold first),@(cadr (fold second)))))

			
			; (pattern-rule
			; `(cons  ,(? `first quote?) ,(? 'quote )) 
			; 	(trace-lambda a(first  second)
			; 	`'(,(cadr (fold first)),@(fold second))))			

			; (pattern-rule
			; `(cons  ,(? `first ) ,(? 'quote ?) ) 
			; 	(trace-lambda a(first  second)
			; 	`'(,(fold first),@(fold second))))



 (pattern-rule  `(car ,(? 'arg list?)  )
             (lambda(arg)(let ((exp (fold arg) ))
                 (cond
                   ( (and (pair? exp) (equal? (car exp) 'car)) `(caar ,@(cdr exp)))
                   ( (and (pair? exp) (equal? (car exp) 'cdr)) `(cadr ,@(cdr exp)))
                   ( (and (pair? exp) (equal? (car exp) 'caar)) `(caaar ,@(cdr exp)))
                   ( (and (pair? exp) (equal? (car exp) 'cddr)) `(caddr ,@(cdr exp)))
                   ( (and (pair? exp) (equal? (car exp) 'cdar)) `(cadar ,@(cdr exp)))
                   ( (and (pair? exp) (equal? (car exp) 'cadr)) `(caadr ,@(cdr exp)))
            		
                   ((pair? exp) 
                   	(cond 
                   		((quote? (car exp))(let ((value (caadr exp)))
                   			(if (const? value) value `'(,@value))))
                   	 	((equal? (car exp) 'cons)(cadr exp))
                   	 	((equal? (car exp) 'list)(cadr exp))
                   	 	((equal?(car exp) 'append)(cadr exp))
                   	 	(else `(car ,exp)))
                   )
                   (else `(car ,exp )))
                 )
             )) 


 (pattern-rule
           `(cdr ,(? 'arg list?)  )
             (lambda(arg)
               (let ((exp (fold arg) ))
                 (cond
                   ( (and (pair? exp) (equal? (car exp) 'car)) `(cadr ,@(cdr exp)))
                   ( (and (pair? exp) (equal? (car exp) 'cdr)) `(cddr ,@(cdr exp)))
                   ( (and (pair? exp) (equal? (car exp) 'caar)) `(cdaar ,@(cdr exp)))
                   ( (and (pair? exp) (equal? (car exp) 'cddr)) `(cdddr ,@(cdr exp)))
                   ( (and (pair? exp) (equal? (car exp) 'cadr)) `(cdadr ,@(cdr exp)))
                   ( (and (pair? exp) (equal? (car exp) 'cdar)) `(cddar ,@(cdr exp)))
            		
                   ((pair? exp) 
                   	(cond 
                   		((quote? (car exp))(let ((value (cdadr exp)))
                   			(if (const? value) value `'(,@value))))
                   	 	((equal? (car exp) 'cons)(caddr exp))
                   	 	((equal? (car exp) 'list)(caddr exp))
                   	 	((equal?(car exp) 'append)(caddr exp))
                   	 	(else `(cdr ,exp)))
                    
                   )
                   (else `(cdr ,exp )))
                 )
             )) 


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
;  (pattern-rule
;    `(car (car (car (car ,(? 'expr ^var?)))))
;    (lambda (expr) `(caaaar ,expr)))
;  (pattern-rule
;    `(car (car (car (cdr ,(? 'expr ^var?)))))
;    (lambda (expr) `(caaadr ,expr)))
;  (pattern-rule
;    `(car (car (cdr (car ,(? 'expr ^var?)))))
;    (lambda (expr) `(caadar ,expr)))
;  (pattern-rule
;    `(car (car (cdr (cdr ,(? 'expr ^var?)))))
;    (lambda (expr) `(caaddr ,expr)))
;  (pattern-rule
;    `(car (cdr (car (car ,(? 'expr ^var?)))))
;    (lambda (expr) `(cadaar ,expr)))
;  (pattern-rule
;    `(car (cdr (car (cdr ,(? 'expr ^var?)))))
;    (lambda (expr) `(cadadr ,expr)))
;  (pattern-rule
;    `(car (cdr (cdr (car ,(? 'expr ^var?)))))
;    (lambda (expr) `(caddar ,expr)))
;  (pattern-rule
;    `(car (cdr (cdr (cdr ,(? 'expr ^var?)))))
;    (lambda (expr) `(cadddr ,expr)))
;  (pattern-rule
;    `(cdr (car (car (car ,(? 'expr ^var?)))))
;    (lambda (expr) `(cdaaar ,expr)))
;  (pattern-rule
;    `(cdr (car (car (cdr ,(? 'expr ^var?)))))
;    (lambda (expr) `(cdaadr ,expr)))
;  (pattern-rule
;    `(cdr (car (cdr (car ,(? 'expr ^var?)))))
;    (lambda (expr) `(cdadar ,expr)))
;  (pattern-rule
;    `(cdr (car (cdr (cdr ,(? 'expr ^var?)))))
;    (lambda (expr) `(cdaddr ,expr)))
;  (pattern-rule
;    `(cdr (cdr (car (car ,(? 'expr ^var?)))))
;    (lambda (expr) `(cddaar ,expr)))
;  (pattern-rule
;    `(cdr (cdr (car (cdr ,(? 'expr ^var?)))))
;    (lambda (expr) `(cddadr ,expr)))
;  (pattern-rule
;    `(cdr (cdr (cdr (car ,(? 'expr ^var?)))))
;    (lambda (expr) `(cdddar ,expr)))
;  (pattern-rule
;    `(cdr (cdr (cdr (cdr ,(? 'expr ^var?)))))
;    (lambda (expr) `(cddddr ,expr)))
; (pattern-rule
;    `(car (car (car ,(? 'expr ^var?))))
;    (lambda (expr) `(caaar ,expr)))
;  (pattern-rule
;    `(car (car (cdr ,(? 'expr ^var?))))
;    (lambda (expr) `(caadr ,expr)))
;  (pattern-rule
;    `(car (cdr (car ,(? 'expr ^var?))))
;    (lambda (expr) `(cadar ,expr)))
;  (pattern-rule
;    `(car (cdr (cdr ,(? 'expr ^var?))))
;    (lambda (expr) `(caddr ,expr)))
;  (pattern-rule
;    `(cdr (car (car ,(? 'expr ^var?))))
;    (lambda (expr) `(cdaar ,expr)))
;  (pattern-rule
;    `(cdr (car (cdr ,(? 'expr ^var?))))
;    (lambda (expr) `(cdadr ,expr)))
;  (pattern-rule
;    `(cdr (cdr (car ,(? 'expr ^var?))))
;    (lambda (expr) `(cddar ,expr)))
;  (pattern-rule
;    `(cdr (cdr (cdr ,(? 'expr ^var?))))
;    (lambda (expr) `(cdddr ,expr)))
;  	 (pattern-rule
;    `(car (car ,(? 'expr ^var?)))
;    (lambda (expr) `(caar ,expr)))
;  (pattern-rule
;    `(car (cdr ,(? 'expr ^var?)))
;    (lambda (expr) `(cadr ,expr)))
;  (pattern-rule
;    `(cdr (car ,(? 'expr ^var?)))
;    (lambda (expr) `(cdar ,expr)))
;  (pattern-rule
;    `(cdr (cdr ,(? 'expr ^var?)))
;    (lambda (expr) `(cddr ,expr)))
 
  


;;;;;;;;;;;;;;;;;;;;;
  (pattern-rule
   `(car (car (car (car ,(? 'expr-app list?)))))
   (lambda (expr) (get-ex-left caaaar  expr)))
 (pattern-rule
   `(car (car (car (cdr ,(? 'expr-app list?)))))
   (lambda (expr) (get-ex-left caaadr  expr)))
 (pattern-rule
   `(car (car (cdr (car ,(? 'expr-app list?)))))
   (lambda (expr) (get-ex-left caadar  expr)))
 (pattern-rule
   `(car (car (cdr (cdr ,(? 'expr-app list?)))))
   (lambda (expr) (get-ex-left caaddr  expr)))
 (pattern-rule
   `(car (cdr (car (car ,(? 'expr-app list?)))))
   (lambda (expr) (get-ex-left cadaar  expr)))
 (pattern-rule
   `(car (cdr (car (cdr ,(? 'expr-app list?)))))
   (lambda (expr) (get-ex-left cadadr  expr)))
 (pattern-rule
   `(car (cdr (cdr (car ,(? 'expr-app list?)))))
   (lambda (expr) (get-ex-left caddar  expr)))
 (pattern-rule
   `(car (cdr (cdr (cdr ,(? 'expr-app list?)))))
   (lambda (expr) (get-ex-left cadddr  expr)))
 (pattern-rule
   `(cdr (car (car (car ,(? 'expr-app list?)))))
   (lambda (expr) (get-ex-right cdaaar  expr)))
 (pattern-rule
   `(cdr (car (car (cdr ,(? 'expr-app list?)))))
   (lambda (expr) (get-ex-right cdaadr  expr)))
 (pattern-rule
   `(cdr (car (cdr (car ,(? 'expr-app list?)))))
   (lambda (expr)  (get-ex-right cdadar  expr)))
 (pattern-rule
   `(cdr (car (cdr (cdr ,(? 'expr-app list?)))))
   (lambda (expr) (get-ex-right cdaddr  expr)))
 (pattern-rule
   `(cdr (cdr (car (car ,(? 'expr-app list?)))))
   (lambda (expr) (get-ex-right cddaar  expr)))
 (pattern-rule
   `(cdr (cdr (car (cdr ,(? 'expr-app list?)))))
   (lambda (expr) (get-ex-right cddadr  expr)))
 (pattern-rule
   `(cdr (cdr (cdr (car ,(? 'expr-app list?)))))
   (lambda (expr) (get-ex-right cdddar  expr)))
 (pattern-rule
   `(cdr (cdr (cdr (cdr ,(? 'expr-app list?)))))
   (lambda (expr) (get-ex-right cddddr  expr)))
(pattern-rule
   `(car (car (car ,(? 'expr-app list?))))
   (lambda (expr) (get-ex-left caaar expr)))
 (pattern-rule
   `(car (car (cdr ,(? 'expr-app list?))))
   (lambda (expr) (get-ex-left caadr  expr)))
 (pattern-rule
   `(car (cdr (car ,(? 'expr-app list?))))
   (lambda (expr) (get-ex-left cadar  expr)))
 (pattern-rule
   `(car (cdr (cdr ,(? 'expr-app list?))))
   (lambda (expr) (get-ex-left caddr  expr)))
 (pattern-rule
   `(cdr (car (car ,(? 'expr-app list?))))
   (lambda (expr) (get-ex-right cdaar  expr)))
 (pattern-rule
   `(cdr (car (cdr ,(? 'expr-app list?))))
   (lambda (expr) (get-ex-right cdadr  expr)))
 (pattern-rule
   `(cdr (cdr (car ,(? 'expr-app list?))))
   (lambda (expr) (get-ex-right cddar expr)))
 (pattern-rule
   `(cdr (cdr (cdr ,(? 'expr-app list?))))
   (lambda (expr) (get-ex-right cdddr expr)))
 	 (pattern-rule
   `(car (car ,(? 'expr-app list?)))
   (lambda (expr) (get-ex-left caar expr)))
 (pattern-rule
   `(car (cdr ,(? 'expr-app list?)))
   (lambda (expr) (get-ex-left cadr expr)))
 (pattern-rule
   `(cdr (car ,(? 'expr-app list?)))
   (lambda (expr) (get-ex-right cdar xpr)))
 (pattern-rule
   `(cdr (cdr ,(? 'expr-app list?)))
   (lambda (expr) (get-ex-right cddr expr)))
  (pattern-rule
   `(car ,(? 'expr-app quote?))
   (lambda (expr) (get-ex-left car expr)))
   (pattern-rule
   `(cdr ,(? 'expr-app quote?))
   (lambda (expr) (get-ex-right cdr expr)))
(pattern-rule
				`(add1 ,(? 'rest))
				(lambda (rest)
					(fold `(+ 1 ,rest))))
			(pattern-rule
				`(sub1 ,(? 'rest))
				(lambda (rest)
					(fold `(+ -1 ,rest))))
			(pattern-rule
				`(number? ,(? 'exp))
				(lambda (exp)
					(let ((e (fold exp)))
						(if	(value? e)
							(number? e)
							`(number? ,e)))))
			(pattern-rule
				`(zero? ,(? `exp))
				(lambda (exp)
					(let ((e (fold exp)))
						(if	(number? e)
							(zero? e)
							`(zero? ,e )))))
			(pattern-rule
				`(null? ,(? `exp))
				(lambda (exp)
					(let ((e (fold exp)))
						(if	(value? e)
							(null? e)
							`(null? ,e)))))
			(pattern-rule
				`(string? ,(? exp))
				(lambda (exp)
					(let ((e (fold exp)))
						(if	(value? e)
							(string? e)
							`(string? ,e)))))
			(pattern-rule
				`(if ,(? 'pred) ,(? 'dit) )
				(lambda (pred dit)
					(let (	(pred-folded (fold pred))
							(dit-folded (fold dit)))
					(if (value? pred-folded)
						(if pred-folded dit-folded) 
						`(if ,pred-folded ,dit-folded )))))
			(pattern-rule
				`(if ,(? 'pred) ,(? 'dit) ,(? 'dif) )
				(lambda (pred dit dif)
					(let (	(pred-folded (fold pred))
							(dit-folded (fold dit))
							(dif-folded (fold dif)))
					(if (value? pred-folded)
						(if pred-folded dit-folded dif-folded) 
						`(if ,pred-folded ,dit-folded ,dif-folded)))))
			(pattern-rule
				`(string-append . ,(? 'expressions))
				(lambda (expressions)
					(let ((folded-expressions (map fold expressions )))
						(if	(andmap string? folded-expressions)
							(apply string-append folded-expressions)
							; `(string-append ,@folded-expressions)))))
							(append-strings (filter (lambda (x) (not (null? x))) (split-list-by-pred string? folded-expressions)))))))
			(pattern-rule
				`(append . ,(? 'expressions))
				(lambda (expressions)
					(let ((folded-expressions (map fold expressions)))
						(if	(andmap (lambda (x) (and (list? x) (value? x))) folded-expressions)
							(begin (display 1) (apply append (map (lambda (x) (cadr x)) folded-expressions)))
							(begin (display 2) `(append ,@folded-expressions)))
						)))
			(pattern-rule
				(? 'any-exp)
				id)
		)))
	(lambda (e)
			(run e
			(lambda ()
				(error 'parse
				(format "I can't recognize this: ~s" e)))))))

(define (flatten sym list)
	(letrec ((f (lambda (list succ)
					(cond ((null? list) (succ list))
					      ((and (list? (car list)) (eqv? (car (car list)) sym)) (f (cdr list) (lambda (rest) (succ `(,@(cdar list) ,@rest )))))
					      (else (f (cdr list) (lambda (rest) (succ `(,(car list) ,@rest)))))))))
		(f list (lambda(x) x))))

(define (quotify l)
	(if (and (list? l) (> (length l) 1))
		`(quote (,@l))
		l
		))

(define (part pred lst)
	(cons 	(filter pred lst)
			(filter (lambda (x) (not (pred x)) ) lst)))

(define (plus->mult lst)
	(letrec ((f (lambda (initial-lst rest-lst)
					(cond 	((null? rest-lst) initial-lst)
							(else (let* (	(count-sym (count 0 rest-lst (car rest-lst)))
											(filterd (filter (lambda (x) (not (eqv? x (car rest-lst)))) rest-lst)))
								(if (= 1 count-sym)
									(f (cons (car rest-lst) initial-lst) filterd)
									(f `((* ,count-sym ,(car rest-lst)) ,@initial-lst ) filterd ))))))))
	(f `() lst)))

(define (count initial lst symbol)
	(cond (	(null? lst) initial)
			((eqv? symbol (car lst)) (count (+ initial 1) (cdr lst) symbol))
			(else (count initial (cdr lst) symbol))))

(define (value? x)
	(or	(number? x)
		(boolean? x)
		(string? x)
		(char? x)
		(quote? x)
		(and (list? x) (andmap (lambda (x) (value? x) ) x))
		(and (pair? x) (value? (car x)) (value? (cdr x)) )
		))

(define (id-variadic . lst) lst)

(define append-strings
	(lambda (lst)
		(let ((appended-lst (map	(lambda (x)
					(if (list-of-strings? x)
						(apply string-append x)
						x))
						lst)))
		`(string-append ,@appended-lst)
		)))

(define list-of-strings?
	(lambda (x)
		(and (list? x)(andmap string? x))))

(define (split-list-by-pred pred lst)
	(letexp ((f (lambda(pred lst succ fail)
					(cond 	((null? lst) (succ `() `()))
							((pred (car lst)) (f 	pred
													(cdr lst)
													(lambda (succ-lst rest)
														(succ (cons (car lst) succ-lst) rest))
													(lambda (x rest)
														(succ (list (car lst)) (cons x rest) ))))
							(else (f 	pred
										(cdr lst)
										(lambda (succ-lst rest)
											(fail (car lst) (cons succ-lst rest) ))
										(lambda (x rest)
											(fail (car lst) (cons x rest) ))))))))
	(filter (lambda (x) (not (null? x)))
			(f 	pred
				lst
				(lambda (succ-lst rest)
					(cons succ-lst rest))
				(lambda (x rest)
					(cons `() (cons x rest)))))))
