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


(define fold
	(let ((run
		(compose-patterns
			(pattern-rule
				`(+ . ,(? `args))
				(lambda (args)
					(let* (	(flat-list (flatten `+ args))
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
					(let* (	(flat-list (flatten `+ args))
							(part (part number? flat-list))
							(numbers (car part))
							(non-numbers (cdr part))
							(sum (apply * numbers)))
					(if	(null? non-numbers)
						sum
						`(* ,sum ,@non-numbers) ))))
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
						(if	(value? exp)
							(zero? exp)
							`(zero? ,exp )))))
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
				`(string-append . ,(? 'expressions))
				(lambda (expressions)
					(let ((folded-expressions (map fold expressions )))
						(if	(andmap string? folded-expressions)
							(apply string-append folded-expressions)
							; `(string-append ,@folded-expressions)))))
							(append-strings (filter (lambda (x) (not (null? x))) (split-list-by-pred string? folded-expressions)))))))
			(pattern-rule
				(? 'any-exp)
				id)
		)))
	(lambda (e)
		(let ((exp (if 	(list? e)
		    			(map fold e)
		    			e)))
			(run exp
			(lambda ()
				(error 'parse
				(format "I can't recognize this: ~s" e))))))))

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
		(string? x)))

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
	(letrec ((f (lambda(pred lst succ fail)
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
