(load "compiler.scm")



(define quote? 
	(trace-lambda qq(e)
 (or (eq? e 'quote)(and (pair? e)(eq? (car e) 'quote)))))


(define get-ex (lambda (op e)(let*  ((exp (fold e))
   	(exp2 (if(quote? exp)`'(,@(op (cadr exp)))
   		(op exp))))
   	(if(equal? exp2 'cons)
   	(op(cdr exp))exp2))))

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
			`(cons . ,(? `vars (lambda(vars)(andmap (trace-lambda cont(x)(not (or (^const? x)(quote? x))))vars))))
			(lambda(vars)
				`(cons ,@vars)))
			
			(pattern-rule
			`(cons  ,(? `first quote?) ,(? 'quote quote?) ) 
				(trace-lambda a(first  second)
				`'(,(cadr (fold first)),@(cadr (fold second)))))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
 (pattern-rule
   `(car (car (car (car ,(? 'expr ^var?)))))
   (lambda (expr) `(caaaar ,expr)))
 (pattern-rule
   `(car (car (car (cdr ,(? 'expr ^var?)))))
   (lambda (expr) `(caaadr ,expr)))
 (pattern-rule
   `(car (car (cdr (car ,(? 'expr ^var?)))))
   (lambda (expr) `(caadar ,expr)))
 (pattern-rule
   `(car (car (cdr (cdr ,(? 'expr ^var?)))))
   (lambda (expr) `(caaddr ,expr)))
 (pattern-rule
   `(car (cdr (car (car ,(? 'expr ^var?)))))
   (lambda (expr) `(cadaar ,expr)))
 (pattern-rule
   `(car (cdr (car (cdr ,(? 'expr ^var?)))))
   (lambda (expr) `(cadadr ,expr)))
 (pattern-rule
   `(car (cdr (cdr (car ,(? 'expr ^var?)))))
   (lambda (expr) `(caddar ,expr)))
 (pattern-rule
   `(car (cdr (cdr (cdr ,(? 'expr ^var?)))))
   (lambda (expr) `(cadddr ,expr)))
 (pattern-rule
   `(cdr (car (car (car ,(? 'expr ^var?)))))
   (lambda (expr) `(cdaaar ,expr)))
 (pattern-rule
   `(cdr (car (car (cdr ,(? 'expr ^var?)))))
   (lambda (expr) `(cdaadr ,expr)))
 (pattern-rule
   `(cdr (car (cdr (car ,(? 'expr ^var?)))))
   (lambda (expr) `(cdadar ,expr)))
 (pattern-rule
   `(cdr (car (cdr (cdr ,(? 'expr ^var?)))))
   (lambda (expr) `(cdaddr ,expr)))
 (pattern-rule
   `(cdr (cdr (car (car ,(? 'expr ^var?)))))
   (lambda (expr) `(cddaar ,expr)))
 (pattern-rule
   `(cdr (cdr (car (cdr ,(? 'expr ^var?)))))
   (lambda (expr) `(cddadr ,expr)))
 (pattern-rule
   `(cdr (cdr (cdr (car ,(? 'expr ^var?)))))
   (lambda (expr) `(cdddar ,expr)))
 (pattern-rule
   `(cdr (cdr (cdr (cdr ,(? 'expr ^var?)))))
   (lambda (expr) `(cddddr ,expr)))
(pattern-rule
   `(car (car (car ,(? 'expr ^var?))))
   (lambda (expr) `(caaar ,expr)))
 (pattern-rule
   `(car (car (cdr ,(? 'expr ^var?))))
   (lambda (expr) `(caadr ,expr)))
 (pattern-rule
   `(car (cdr (car ,(? 'expr ^var?))))
   (lambda (expr) `(cadar ,expr)))
 (pattern-rule
   `(car (cdr (cdr ,(? 'expr ^var?))))
   (lambda (expr) `(caddr ,expr)))
 (pattern-rule
   `(cdr (car (car ,(? 'expr ^var?))))
   (lambda (expr) `(cdaar ,expr)))
 (pattern-rule
   `(cdr (car (cdr ,(? 'expr ^var?))))
   (lambda (expr) `(cdadr ,expr)))
 (pattern-rule
   `(cdr (cdr (car ,(? 'expr ^var?))))
   (lambda (expr) `(cddar ,expr)))
 (pattern-rule
   `(cdr (cdr (cdr ,(? 'expr ^var?))))
   (lambda (expr) `(cdddr ,expr)))
 	 (pattern-rule
   `(car (car ,(? 'expr ^var?)))
   (lambda (expr) `(caar ,expr)))
 (pattern-rule
   `(car (cdr ,(? 'expr ^var?)))
   (lambda (expr) `(cadr ,expr)))
 (pattern-rule
   `(cdr (car ,(? 'expr ^var?)))
   (lambda (expr) `(cdar ,expr)))
 (pattern-rule
   `(cdr (cdr ,(? 'expr ^var?)))
   (lambda (expr) `(cddr ,expr)))
 
  
;;;;;;;;;;;;;;;;;;;;;
  (pattern-rule
   `(car (car (car (car ,(? 'expr-app list?)))))
   (lambda (expr) (caaaar (fold expr))))
 (pattern-rule
   `(car (car (car (cdr ,(? 'expr-app list?)))))
   (lambda (expr) (caaadr (fold expr))))
 (pattern-rule
   `(car (car (cdr (car ,(? 'expr-app list?)))))
   (lambda (expr) (caadar (fold expr))))
 (pattern-rule
   `(car (car (cdr (cdr ,(? 'expr-app list?)))))
   (lambda (expr) (caaddr (fold expr))))
 (pattern-rule
   `(car (cdr (car (car ,(? 'expr-app list?)))))
   (lambda (expr) (cadaar (fold expr))))
 (pattern-rule
   `(car (cdr (car (cdr ,(? 'expr-app list?)))))
   (lambda (expr) (cadadr (fold expr))))
 (pattern-rule
   `(car (cdr (cdr (car ,(? 'expr-app list?)))))
   (lambda (expr) (caddar (fold expr))))
 (pattern-rule
   `(car (cdr (cdr (cdr ,(? 'expr-app list?)))))
   (lambda (expr) (cadddr (fold expr))))
 (pattern-rule
   `(cdr (car (car (car ,(? 'expr-app list?)))))
   (lambda (expr) (cdaaar (fold expr))))
 (pattern-rule
   `(cdr (car (car (cdr ,(? 'expr-app list?)))))
   (lambda (expr) (cdaadr (fold expr))))
 (pattern-rule
   `(cdr (car (cdr (car ,(? 'expr-app list?)))))
   (lambda (expr) (cdadar (fold expr))))
 (pattern-rule
   `(cdr (car (cdr (cdr ,(? 'expr-app list?)))))
   (lambda (expr) (cdaddr (fold expr))))
 (pattern-rule
   `(cdr (cdr (car (car ,(? 'expr-app list?)))))
   (lambda (expr) (cddaar (fold expr))))
 (pattern-rule
   `(cdr (cdr (car (cdr ,(? 'expr-app list?)))))
   (lambda (expr) (cddadr (fold expr))))
 (pattern-rule
   `(cdr (cdr (cdr (car ,(? 'expr-app list?)))))
   (lambda (expr) (cdddar (fold expr))))
 (pattern-rule
   `(cdr (cdr (cdr (cdr ,(? 'expr-app list?)))))
   (lambda (expr) (cddddr (fold expr))))
(pattern-rule
   `(car (car (car ,(? 'expr-app list?))))
   (lambda (expr) (caaar (fold expr))))
 (pattern-rule
   `(car (car (cdr ,(? 'expr-app list?))))
   (lambda (expr) (caadr (fold expr))))
 (pattern-rule
   `(car (cdr (car ,(? 'expr-app list?))))
   (lambda (expr) (cadar (fold expr))))
 (pattern-rule
   `(car (cdr (cdr ,(? 'expr-app list?))))
   (lambda (expr) (caddr (fold expr))))
 (pattern-rule
   `(cdr (car (car ,(? 'expr-app list?))))
   (lambda (expr) (cdaar (fold expr))))
 (pattern-rule
   `(cdr (car (cdr ,(? 'expr-app list?))))
   (lambda (expr) (cdadr (fold expr))))
 (pattern-rule
   `(cdr (cdr (car ,(? 'expr-app list?))))
   (lambda (expr) (cddar (fold expr))))
 (pattern-rule
   `(cdr (cdr (cdr ,(? 'expr-app list?))))
   (lambda (expr) (cdddr (fold expr))))
 	 (pattern-rule
   `(car (car ,(? 'expr-app list?)))
   (lambda (expr) (caar (fold expr))))
 (pattern-rule
   `(car (cdr ,(? 'expr-app list?)))
   (lambda (expr) (get-ex cadr expr)))
 (pattern-rule
   `(cdr (car ,(? 'expr-app list?)))
   (lambda (expr) (get-ex cdar xpr)))
 (pattern-rule
   `(cdr (cdr ,(? 'expr-app list?)))
   (lambda (expr) (get-ex cddr expr)))
  (pattern-rule
   `(car ,(? 'expr-app list?))
   (lambda (expr) (get-ex car expr)))
   (pattern-rule
   `(cdr ,(? 'expr-app list?))
   (lambda (expr) (get-ex cdr expr)))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 		

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
