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
; Ilya: 	+, *, add1, append, car, cdr, cons, list, null?
; Hagai:  	number?, string?, string-append, sub1, zero?


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
					(fold `(+ 1 ,rest)))
			)
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
