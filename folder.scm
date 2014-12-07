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

(define constactor? (lambda(exp)
	(let ((e (car exp)))
	(or(equal? e 'cons)(equal? e 'list)(equal? e 'append)))))

(define (super-map-fold-cons x)
	(let ((exp (fold x)))
							(if (quote? exp )(cadr exp) exp)))


(define (super-map-fold vars)
	(map (lambda(x)(let ((exp (fold x)))
							(if (quote? exp )(cadr exp) exp))) vars))

(define (super-map-fold vars)
	(map (lambda(x)(let ((exp (fold x)))
							(if (quote? exp )(cadr exp) exp))) vars))
(define (super-map-fold-in-constractor vars)
	(map (lambda(x)(fold x))vars))
						

(define (super-map-fold-with-qoute vars)
	(map (lambda(x)(fold x)) vars))

(define (super-map-splice vars)
	(map (lambda(x)(let ((exp  (fold x)))
		(cond ((pair? exp)(if (quote? (car exp))(cadr exp)(cdr exp)))
			(else exp))))vars))
							

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
				`(cons . ,(? `first (lambda(vars)(andmap (lambda(x) (const? (fold x)))vars)))) 
					(lambda(vars)
					`'(,@(cons (super-map-fold-cons (car vars))(super-map-fold-cons (cadr vars))))))

					(pattern-rule
				`(cons . ,(? `vars (lambda(vars)(andmap (lambda(x)(not (or (^const? x)(quote? x))))vars))))
				(lambda(vars)
					`(cons ,@(super-map-fold-in-constractor vars))))
				(pattern-rule
				`(list . ,(? `first (lambda(vars)(andmap (lambda(x) (const? (fold x)))vars)))) 
					(lambda(vars)
					`'(,@(super-map-fold vars))))
			(pattern-rule
				`(list . ,(? `vars (lambda(vars)(andmap (lambda(x)(not (or (^const? x)(quote? x))))vars))))
				(lambda(vars)
					`(list ,@(super-map-fold-in-constractor vars))))

			(pattern-rule
				`(append  ,(? 'var))
				(lambda (var)
					var))

			(pattern-rule
				`(append . ,(? 'expressions (lambda(vars)(andmap (lambda(x)(or (list? x)(const? x)))vars))))
				(lambda (expressions)
					`'(,@(apply append (super-map-splice expressions)))))
						
			(pattern-rule 
				`(car ,(? 'arg list?)  )
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
								((equal? (car exp) 'list)(let ((e (cadr exp)))(cond ((quote? e) e) 
																					((pair? e)`(list ,e))
																					(else e))))
								((equal?(car exp) 'append)(let ((e (cadr exp)))(cond ((quote? e) e) 
																					((pair? e)`(list ,e))
																					(else e))))
								(else `(car ,exp))))
						(else `(car ,exp ))))))
			(pattern-rule
				`(cdr ,(? 'arg list?)  )
				(lambda(arg)
					(let ((exp (fold arg) ))
						(cond
							( (and (pair? exp) (equal? (car exp) 'car)) `(cdar ,@(cdr exp)))
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
									((equal? (car exp) 'list)(let ((e (cddr exp)))
										(cond ((quote? e) e)
											 ((pair? e)`(list ,@e))(else  e))))
									((equal?(car exp) 'append)(let ((e (cddr exp)))
										(cond ((quote? e) e)
											 ((pair? e)`(list ,@e))(else  e))))
									(else `(cdr ,exp))))
							(else `(cdr ,exp ))))))
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
						(cond 	((value? e) (empty-list? e))
								((cons-expression? e) #f) 	; we know that a cons expression can't be null
								(else `(null? ,e))))))
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
							(append-strings (split-list-by-pred string? folded-expressions))))))
			
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

;group elements in the list according to pred
; example (split-list-by-pred string? `(a b "c" "d" 1 2))
; 				=> `((a b) ("c" "d") `(1 2))
(define (split-list-by-pred pred lst)
	(letrec ((f (lambda(pred lst succ fail)
					(cond 	((null? lst) (succ `() `()))
							((pred (car lst)) (f 	pred
													(cdr lst)
													(lambda (succ-lst rest);succ
														(succ (cons (car lst) succ-lst) rest))
													(lambda (x rest);fail
														(succ (list (car lst)) (cons x rest) ))))
							(else (f 	pred
										(cdr lst)
										(lambda (succ-lst rest);succ
											(fail (car lst) (cons succ-lst rest) ))
										(lambda (x rest);fail
											(fail (car lst) (cons x rest) ))))))))
	(filter (lambda (x) (not (null? x)))
			(f 	pred
				lst
				(lambda (succ-lst rest);succ
					(cons succ-lst rest))
				(lambda (x rest);fail
					(cons `() (cons x rest)))))))

(define cons-expression?
	(lambda (exp)
		(and	(list? exp)
				(equal? (car exp) 'cons))))

(define empty-list?
	(lambda (exp)
		(or	(equal? exp '(list) )
			(equal? exp ''() ))))
